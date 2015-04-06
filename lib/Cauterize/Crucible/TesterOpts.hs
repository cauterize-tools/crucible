module Cauterize.Crucible.TesterOpts
  ( TesterOpts(..)
  , testerOptions
  ) where

import Cauterize.Crucible.Prototypes
import Cauterize.Generate
import Control.Monad (liftM)
import Options.Applicative
import qualified Data.Text.Lazy as T

data TesterOpts = TesterOpts
  { buildCmds :: [T.Text]
  , runCmd :: T.Text
  , schemaCount :: Integer
  , instanceCount :: Integer
  , schemaTypeCount :: Integer -- number of types
  , schemaEncSize :: Integer  -- maximum encoded size
  , allowedPrototypes :: [PrototypeVariant]
  } deriving (Show)

defaultSchemaCount, defaultInstanceCount :: Integer
defaultSchemaCount = 1
defaultInstanceCount = 100

-- Generate a number of schemas, insert their schemas and meta files into a
-- target generator, run the generator in the test loop, report the exit codes
-- from server and client as results.
--
-- Expanded tokens:
--  %s - the path to the specification file.
--  %d - the path to the working directory of the crucible command.
--
-- cauterize-test crucible --build-cmd="foo --schema=%s --output=%d/cmd"
--                         --build-cmd="cd %d/cmd && cabal build"
--                         --run-cmd="%d/dist/build/cmd/cmd"
--                         --schema-count=5
--                         --instance-count=100
--                         --schema-size=50

testerOptions :: Parser TesterOpts
testerOptions = TesterOpts
  <$> parseBuild
  <*> parseRun
  <*> parseSchemaCount
  <*> parseInstanceCount
  <*> parseSchemaTypeCount
  <*> parseSchemaEncSize
  <*> parseAllowedPrototypes
  where
    parseBuild = many $ option txt ( long "build-cmd"
                                  <> metavar "BLDCMD"
                                  <> help buildCmdHelp )
    parseRun = option txt ( long "run-cmd"
                         <> metavar "RUNCMD"
                         <> help runCmdHelp )
    parseSchemaCount = option auto ( long "schema-count"
                                  <> metavar "SCMCNT"
                                  <> value defaultSchemaCount
                                  <> help schemaCountHelp )
    parseInstanceCount = option auto ( long "instance-count"
                                    <> metavar "INSCNT"
                                    <> value defaultInstanceCount
                                    <> help instanceCountHelp )
    parseSchemaTypeCount = option auto ( long "type-count"
                                      <> metavar "SCMSIZE"
                                      <> value defaultMaximumTypes
                                      <> help schemaSizeHelp )
    parseSchemaEncSize = option auto ( long "enc-size"
                                    <> metavar "ENCSIZE"
                                    <> value defaultMaximumSize
                                    <> help schemaSizeHelp )
    parseAllowedPrototypes = option (str >>= parsePrototypeVariants . T.pack)
                                   (long "prototypes"
                                 <> metavar "PROTOTYPES"
                                 <> value allPrototypeVariants
                                 <> help allowedPrototypesHelp )

    buildCmdHelp = "The command to build the generated test client. Can be specified more than once."
    runCmdHelp = "The command to run the built test client. Can be specified more than once."
    schemaCountHelp = "The number of schemas to test."
    instanceCountHelp = "The number of instances of each schema to test."
    schemaSizeHelp = "The number of types to generate in each schema."
    allowedPrototypesHelp = concat [ "Which prototypes to include in schema generation. "
                                   , "Define using a comma-separated list including only the following elements: "
                                   , "array,combination,record,synonym,union,vector."
                                   ]

    txt = liftM T.pack str
