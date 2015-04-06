module Cauterize.Crucible.FileSystem
  ( inNewDir
  ) where

import System.Directory

-- Create a directory, and perform an IO action with that new directory as the
-- working directory of the IO action.
inNewDir :: String -> IO a -> IO a
inNewDir name a = do
  cd <- getCurrentDirectory
  createDirectory name
  setCurrentDirectory name
  a' <- a
  setCurrentDirectory cd
  return a'
