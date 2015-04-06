module Main
  ( main
  ) where

import Options.Applicative

import Cauterize.Crucible.Generate
import Cauterize.Crucible.GenerateOpts
import Cauterize.Crucible.Example
import Cauterize.Crucible.ExampleOpts
import Cauterize.Crucible.Tester
import Cauterize.Crucible.TesterOpts

data Options = Options Command deriving (Show)
data Command = GenerateCom GenerateOpts
             | ExampleCom ExampleOpts
             | TesterCom TesterOpts
  deriving (Show)

optParser :: Parser Options
optParser = Options
  <$> subparser
      ( command "generate" ( info (fmap GenerateCom genOptions)
                           ( progDesc "Generate a random schema." ) )
     <> command "example" ( info (fmap ExampleCom exampleOptions)
                          ( progDesc "Generate a random binary example fitting a specification." ) )
     <> command "tester" ( info (fmap TesterCom testerOptions)
                         ( progDesc "Test a code generator." ) )
      )

options :: ParserInfo Options
options = info (optParser <**> helper)
               (fullDesc <> progDesc "Test infrastructure for Cauterize")

main :: IO ()
main = do
  (Options c) <- execParser options
  case c of
    GenerateCom gOpts -> runGenerate gOpts
    ExampleCom eOpts -> runExample eOpts
    TesterCom tOpts -> runTester tOpts
