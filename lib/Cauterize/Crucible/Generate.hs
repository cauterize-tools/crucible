module Cauterize.Crucible.Generate
  ( runGenerate
  ) where

import qualified Cauterize.Crucible.GenerateOpts as OPT
import Cauterize.Crucible.Prototypes
import Cauterize.Generate

import qualified Cauterize.Schema as SC
import qualified Data.Text.Lazy.IO as T

runGenerate :: OPT.GenerateOpts -> IO ()
runGenerate (OPT.GenerateOpts typeCount encSize prototypes) = outputCaut prototypes typeCount encSize

outputCaut :: [PrototypeVariant] -- which prototypes to allow
           -> Integer -- number of types in the schema
           -> Integer -- maximum encoded size of the schema
           -> IO ()
outputCaut ps tc es = do
  s <- generateSchemaWith tc es 0.95 ps
  case SC.checkSchema s of
    [] -> T.putStrLn . SC.prettyPrint $ s
    errors -> error $ "ERROR: " ++ show errors
