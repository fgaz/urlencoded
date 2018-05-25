{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Monad ( liftM, replicateM )

import Data.Monoid ( mconcat )
import Data.Maybe ( isJust )
import Data.URLEncoded as U
import Data.List ( intercalate, intersperse )

import Network.URI
    ( isAllowedInURI, URI(..), URIAuth(..), escapeURIString, isUnreserved )

import Control.Applicative ( (<$>), Applicative(..) )

import Test.QuickCheck

instance Arbitrary URLEncoded where
    arbitrary = do
      numPairs <- sized $ \n -> choose (0, n)
      importList `liftM` replicateM numPairs (two arbitraryQueryArg)

two :: Gen b -> Gen (b, b)
two = fmap (\[x,y] -> (x,y)) . vectorOf 2

arbitraryQueryArg :: Gen String
arbitraryQueryArg = do
  qLen <- sized $ \n -> choose (0, n)
  replicateM qLen $ elements ['\x00'..'\xff']

httpUri :: Gen URI
httpUri = URI <$> proto <*> auth <*> path <*> query <*> frag
    where proto = (++ ":") <$> elements ["http", "https"]
          auth = sized $ \n -> if n == 0
                               then return Nothing
                               else Just <$> (URIAuth "" <$> host <*> port)
          lstart = elements ['a'..'z']
          lmid = elements $ ['a'..'z'] ++ ['0'..'9'] ++ "-"
          lend = elements $ ['a'..'z'] ++ ['0'..'9']
          hlabel = (:)
                   <$> lstart
                   <*> sized (\n -> do
                                m <- choose (0, n)
                                if m == 0
                                  then return []
                                  else (++) <$> (replicateM (m - 1) lmid)
                                           <*> (return <$> lend))
          host = intercalate "." <$> (flip replicateM hlabel =<< choose (1, 6))
          port = maybeEmpty (((':':) . show) <$> choose (1, 65535::Int))
          frag = maybeEmpty (('#':) <$> urlSafe)
          urlSafe = escapeURIString isUnreserved <$> arbitraryQueryArg
          query = maybeEmpty
                  ((('?':) . export) <$> (arbitrary :: Gen URLEncoded))
          path = sized $ \n -> do
                   m <- choose (0, n)
                   if m == 0
                     then return "/"
                     else concat <$> (replicateM m (('/':) <$> urlSafe))
          maybeEmpty g = sized $ \n -> do m <- choose (0, n)
                                          if m == 0 then return "" else g

pairLists :: Gen [(String, String)]
pairLists = sized $ \n -> do
              m <- choose (0, n)
              replicateM m $ two arbitraryQueryArg

multiLists :: String -> Gen (Int, URLEncoded)
multiLists k = sized $ \n -> do
                 m <- choose (2, 2 + n)
                 let getP = (k %=) <$> arbitraryQueryArg
                 us <- sequence $ intersperse getP $ replicate m arbitrary
                 return (m, mconcat us)

badKey :: URLEncoded -> String
badKey u = 'x':concat (keys u)

main :: IO ()
main = mapM_ quickCheck
       [ property $ \u ->
             importString (export u) == (Right u :: Either String URLEncoded)

       , property $ \u ->
           let s = export u
               expected = Right s :: Either String String
           in (export `liftM` importString s) == expected

       , property $ \u1 u2 ->
           not (U.null u1 || U.null u2) ==>
                   (export u1 ++ "&" ++ export u2) == export (u1 %& u2)

       , property $ all isAllowedInURI . export

       , property $ \u1 u2 ->
           ((u1::URLEncoded) %? (u2::URLEncoded)) == (u1 %& u2)

       , property $ \u ->
           forAll httpUri $ \uri ->
               (show (uri %? (u::URLEncoded))) == (show uri %? u)

       , forAll (two arbitraryQueryArg) $ \(a, b) -> pairs (a %= b) == [(a, b)]

       , forAll pairLists $ \l -> map fst l == keys (importList l)

       , property $ \u -> urlEncode u == u

       , property $ \m ->
           urlEncode (m :: Maybe URLEncoded) == maybe empty urlEncode m

       , property $ \m ->
           "x" %=? m == maybe empty ("x" %=) (m :: Maybe Int)

       , property $ \u -> show u == export u

       , property $ \u -> all (\k -> isJust (u %! k)) $ keys u

       , property $ \u -> (u %! badKey u) == Nothing

       , property $ \u -> lookupDefault "missing" (badKey u) u == "missing"

       , forAll (multiLists "repeated") $ \(_, u) -> isJust (u %! "repeated")

       , forAll (multiLists "repeated") $ \(_, u) ->
           length (lookupAll "repeated" u) > 0

       , property $ \u -> case lookup1 (badKey u) u :: Either String String of
                            Left _ -> True
                            Right _ -> False
       ]
