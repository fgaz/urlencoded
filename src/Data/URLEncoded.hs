{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- |Implements a data type for constructing and destructing
-- x-www-urlencoded strings. See
-- <http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4.1>

module Data.URLEncoded
    ( -- * Representation of a URL-encoded string
      URLEncoded
    , filter
    , URLShow(..)
    , URLEncode(..)

    -- * Generate
    , empty
    , importString
    , importList
    , importURI
    , (%=)
    , (%=?)
    , (%&)
    , AddURLEncoded(..)

    -- * Query
    , null
    , keys
    , lookup
    , lookupAll
    , lookup1
    , lookupDefault
    , pairs
    , (%!)

    -- * Export
    , addToURI
    , export
    )
where

import qualified Prelude
import Prelude hiding ( null, lookup, filter )
import Data.List.Split ( splitOn )
import Control.Monad ( liftM )
import Control.Arrow ( (>>>) )
import Control.Monad.Except ( MonadError )
import Network.URI ( unEscapeString, escapeURIString, isUnreserved, URI(uriQuery) )
import Data.Monoid ( Monoid, mappend )
import Data.List ( intercalate )
import Data.Maybe ( fromMaybe )

-- | A container for URLEncoded data
newtype URLEncoded = URLEncoded { pairs :: [(String, String)] }
    deriving (Monoid, Eq)

class AddURLEncoded a where
    (%?) :: URLEncode args => a -> args -> a
infixr 6 %?

instance AddURLEncoded [Char] where
    str %? q = let (u, frag) = break (== '#') str
                   joiner = if last u == '?'
                            then ""
                            else if '?' `elem` u
                                 then "&"
                                 else "?"
               in concat [u, joiner, export $ urlEncode q, frag]

instance AddURLEncoded URI where
    u %? q = addToURI (urlEncode q) u

instance AddURLEncoded URLEncoded where
    q1 %? q2 = q1 `mappend` urlEncode q2

(%&) :: (URLEncode q1, URLEncode q2) => q1 -> q2 -> URLEncoded
q1 %& q2 = urlEncode q1 `mappend` urlEncode q2
infixr 7 %&

-- | Is this URLEncoded data empty?
null :: URLEncoded -> Bool
null = Prelude.null . pairs

-- | URLEncoded data with no pairs
empty :: URLEncoded
empty = URLEncoded []

-- |Import this list of pairs as URLEncoded data
importList :: [(String, String)] -> URLEncoded
importList = URLEncoded

-- |All of the keys from the URLEncoded value, in order, preserving duplicates
keys :: URLEncoded -> [String]
keys = map fst . pairs

-- |Create singleton URLEncoded data containing the supplied key and value
(%=) :: (URLShow a, URLShow b) => a -> b -> URLEncoded
k %= v = URLEncoded [(urlShow k, urlShow v)]
infixl 8 %=

-- |Encode a value as x-www-urlencoded
class URLEncode a where
    urlEncode :: a -> URLEncoded

instance (URLShow a, URLShow b) => URLEncode (a, b) where
    urlEncode (x, y) = importList [(urlShow x, urlShow y)]

instance URLEncode a => URLEncode (Maybe a) where
    urlEncode = maybe empty urlEncode

instance URLEncode URLEncoded where
    urlEncode = id

-- |Serialize a value into a String for encoding as part of an
-- x-www-urlencoded value
class URLShow a where
    urlShow :: a -> String

instance URLShow Char where
    urlShow = return

instance URLShow URI where
    urlShow = show

instance URLShow URLEncoded where
    urlShow = export

instance URLShow [Char] where
    urlShow = id

instance URLShow Int where
    urlShow = show

instance URLShow Integer where
    urlShow = show

instance URLShow Bool where
    urlShow True = "true"
    urlShow False = "false"

-- |If the second value is Nothing, return empty URLEncoded
-- data. Otherwise return singleton URLEncoded data that contains the
-- given key and value.
(%=?) :: (URLShow a, URLShow b) =>
         a {-^key-} -> Maybe b {-^value-} -> URLEncoded
k %=? v = maybe empty (k %=) v
infixl 8 %=?

-- |Add this URL-encoded data to the query part of a URI, after any
-- existing query arguments.
addToURI :: URLEncoded -> URI -> URI
addToURI q u =
    let uq = uriQuery u
        initial = if uq == "?"
                  then ""
                  else if Prelude.null (uriQuery u) then "?" else "&"
    in u { uriQuery = uriQuery u ++ initial ++ export q }

-- |Convert this URLEncoded object into an x-www-urlencoded String
-- (The resulting string is 7-bit clean ASCII, containing only
-- unreserved URI characters and %-encoded values)
export :: URLEncoded -> String
export q =
    let esc = escapeURIString isUnreserved
        encodePair (k, v) = esc k ++ "=" ++ esc v
    in intercalate "&" $ map encodePair $ pairs q

instance Show URLEncoded where
    showsPrec _ q = (export q ++)

-- |Parse this string as x-www-urlencoded
importString :: MonadError e m => String -> m URLEncoded
importString "" = return empty
importString s = liftM importList $ mapM parsePair $ splitOn "&" s
    where parsePair p =
              case break (== '=') p of
                (_, []) -> fail $ "Missing value in query string: " ++ show p
                (k, '=':v) -> return ( unesc k
                                     , unesc v
                                     )
                unknown -> error $ "impossible: " ++ show unknown
          unesc = unEscapeString . intercalate "%20" . splitOn "+"

importURI :: MonadError e m => URI -> m URLEncoded
importURI u = case uriQuery u of
                ('?':s) -> importString s
                [] -> return empty
                q -> fail $ "Unexpected query for URI: " ++ show q

-- |Return the /first/ value for the given key, or throw an error if the
-- key is not present in the URLEncoded data.
lookup1 :: (URLShow a, MonadError e m) => a -> URLEncoded -> m String
lookup1 k = lookup k >>> maybe missing return
    where missing = fail $ "Key not found: " ++ urlShow k

lookup :: URLShow a => a -> URLEncoded -> Maybe String
lookup k = pairs >>> Prelude.lookup (urlShow k)

lookupDefault :: URLShow a => String -> a -> URLEncoded -> String
lookupDefault dflt k q = fromMaybe dflt $ q %! k

-- |Return all values whose keys match the supplied key, in the order
-- they appear in the query. Will return an empty list if no keys
-- match.
lookupAll :: URLShow a => a -> URLEncoded -> [String]
lookupAll k urlenc = [ v | (k', v) <- pairs urlenc, k' == urlShow k ]

-- |Create a URLEncoded object that represents all pairs from the
-- input that match the supplied predicate
filter :: ((String, String) -> Bool) -> URLEncoded -> URLEncoded
filter p = pairs >>> Prelude.filter p >>> URLEncoded

-- |Look up a key in a URLEncoded value and return the first matching
-- value, or Nothing if there is no value that matches
(%!) :: URLShow a => URLEncoded -> a -> Maybe String
(%!) = flip lookup
infixr 1 %!
