module Cauterize.Crucible.ExampleOpts
  ( ExampleOpts(..)
  , exampleOptions
  , Format(..)
  ) where

import Options.Applicative

data ExampleOpts = ExampleOpts
  { spec :: FilePath
  , format :: Format
  } deriving (Show)

data Format = FmtHex
            | FmtCSV
            | FmtBinary
            | FmtString
  deriving (Show)

exampleOptions :: Parser ExampleOpts
exampleOptions = ExampleOpts
  <$> strOption
    ( long "spec" <> metavar "SPECIFICATION" <> help specHelp )
  <*> option (str >>= parseFormat)
    ( long "format" <> value FmtString <> metavar "FORMAT" <> help fmtHelp )
  where
    specHelp = "The specification file for which to generate an example binary string."
    fmtHelp = "The format in which to output the example. One of: hex,csv,bin."

parseFormat :: Monad m => String -> m Format
parseFormat "hex" = return FmtHex
parseFormat "csv" = return FmtCSV
parseFormat "bin" = return FmtBinary
parseFormat "str" = return FmtString
parseFormat x = fail $ "Invalid format: " ++ x
