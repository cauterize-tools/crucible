module Main
  ( main
  ) where

import Options.Applicative

import Cauterize.Crucible.TesterOpts
import Cauterize.Crucible.Generate
import Cauterize.Crucible.GenerateOpts
import Cauterize.Crucible.ExampleOpts

data Options = Options Command deriving (Show)
data Command = GenerateCom GenerateOpts
  deriving (Show)

optParser :: Parser Options
optParser = Options
  <$> subparser
      ( command "generate"
        ( info (fmap GenerateCom genOptions)
          ( progDesc "Generate a random schema." ) )
      )

options :: ParserInfo Options
options = info (optParser <**> helper)
               (fullDesc <> progDesc "Test infrastructure for Cauterize")

main :: IO ()
main = do
  (Options c) <- execParser options
  case c of
    GenerateCom gOpts -> runGenerate gOpts
