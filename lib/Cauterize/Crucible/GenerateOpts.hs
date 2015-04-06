module Cauterize.Crucible.GenerateOpts
  ( GenerateOpts(..)
  , genOptions
  ) where

import Options.Applicative

import Cauterize.Crucible.Prototypes
import qualified Data.Text.Lazy as T

data GenerateOpts = GenerateOpts
  { count :: Integer
  , size :: Integer
  , prototypes :: [PrototypeVariant]
  } deriving (Show)

genOptions :: Parser GenerateOpts
genOptions = GenerateOpts
  <$> option auto
    ( long "count" <> metavar "COUNT" <> help countHelp )
  <*> option auto
    ( long "size" <> metavar "SIZE" <> help sizeHelp )
  <*> option (str >>= (parsePrototypeVariants . T.pack))
       ( long "prototypes"
      <> metavar "PROTOTYPES"
      <> value allPrototypeVariants
      <> help prototypesHelp
       )
  where
    countHelp = "The number of types to generate."
    sizeHelp = "The maximum number of bytes to allow in the encoded representation."
    prototypesHelp = concat [ "Which prototypes to include in schema generation. "
                            , "Define using a comma-separated list including only "
                            , "the following elements: array,combination,record,synonym,union,vector."
                            ]
