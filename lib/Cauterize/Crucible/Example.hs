module Cauterize.Crucible.Example
  ( runExample
  ) where

import qualified Cauterize.Crucible.ExampleOpts as OPT
import qualified Cauterize.Specification as Spec
import qualified Cauterize.Dynamic.Meta as DynMeta
import qualified Cauterize.Dynamic.Meta.Pretty as P

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.IO as T

import Numeric (showHex)
import Data.List (intercalate)

runExample :: OPT.ExampleOpts -> IO ()
runExample (OPT.ExampleOpts spec fmt) = do
  f <- Spec.parseSpecificationFromFile spec
  case f of
    Left e -> do
      putStrLn "Error parsing specification."
      print e
    Right spec' -> schemaExample spec' fmt

schemaExample :: Spec.Specification -> OPT.Format -> IO ()
schemaExample spec fmt = do
  t <- DynMeta.dynamicMetaGen spec
  let b = DynMeta.dynamicMetaPack spec t
  case fmt of
    OPT.FmtHex -> outputHex $ B.fromStrict b
    OPT.FmtCSV -> outputCSV $ B.fromStrict b
    OPT.FmtBinary -> outputBin $ B.fromStrict b
    OPT.FmtString -> outputStr spec t

outputHex :: B.ByteString -> IO ()
outputHex bs = putStrLn $ concatMap showByte bs'
  where
    showByte b = case showHex b "" of
                  [x,y] -> [x,y]
                  [x] -> ['0',x]
                  _ -> error "outputHex: This should be impossible."
    bs' = B.unpack bs

outputCSV :: B.ByteString -> IO ()
outputCSV bs = putStrLn $ intercalate "," $ map show bs'
  where
    bs' = B.unpack bs

outputBin :: B.ByteString -> IO ()
outputBin = B.putStr

outputStr :: Spec.Specification -> DynMeta.MetaType -> IO ()
outputStr s t = T.putStrLn (P.dynamicMetaPretty s t)
