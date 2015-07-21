{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Crucible.Tester
  ( runTester
  ) where

import Cauterize.Crucible.FileSystem
import Cauterize.Crucible.Prototypes
import Cauterize.Generate
import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX
import System.Directory
import System.Exit
import System.IO
import System.Process
import qualified Cauterize.Crucible.TesterOpts as OPT
import qualified Cauterize.Dynamic.Meta as D
import qualified Cauterize.Schema as SC
import qualified Cauterize.Specification as SP
import qualified Cauterize.CommonTypes as C
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Context = Context
  { specificationPath :: T.Text
  , currentDir :: T.Text
  } deriving (Show)

data BuildCmdOutput = BuildCmdOutput
  { buildCmdStr :: T.Text
  , buildExitCode :: ExitCode
  } deriving (Show)

data TestOutput = TestOutput { testResult :: TestResult
                             , testStdErr :: T.Text
                             , testEncodedInstance :: B.ByteString
                             , testInstance :: D.MetaType
                             }
  deriving (Show)

data TestResult = TestPass
                | TestError { testErrorMessage :: T.Text }
                | TestFail
  deriving (Show)


runTester :: OPT.TesterOpts -> IO ()
runTester opts@OPT.TesterOpts { OPT.schemaCount = schemaCount
                              , OPT.allowedPrototypes = allowedPrototypes
                              , OPT.schemaEncSize = schemaEncSize
                              , OPT.schemaTypeCount = schemaTypeCount
                              , OPT.buildCmds = buildCmds
                              , OPT.runCmd = runCmd
                              , OPT.instanceCount = instanceCount
                              } = do
  printHeaderText opts

  t <- round `fmap` getPOSIXTime :: IO Integer
  failCounts <- inNewDir ("crucible-" ++ show t) $
                  forM [0..schemaCount-1] $
                    \ix -> inNewDir ("schema-" ++ show ix) go
  case sum failCounts of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)

  where
    aSchema c s = generateSchemaWith c s 0.95 allowedPrototypes
    go = do
      -- Generate a schema. From this, also compile a specification file and a meta
      -- file. Write them to disk.
      schema <- aSchema schemaTypeCount schemaEncSize
      let spec = SP.mkSpecification schema

      T.writeFile "schema.txt" $ SC.formatSchema schema
      T.writeFile "specification.txt" $ SP.formatSpecification spec

      -- Construct a context with the paths to the specification and meta files
      -- along with the current directory.
      ctx <- liftM2 Context (return "specification.txt")
                            (liftM T.pack getCurrentDirectory)

      -- Chain together the commands specified on the command line after
      -- expanding their variables. Run the commands in order. If any
      -- command fails in the build sequence, the remaining commands will
      -- not be run.
      let buildCmds' = map (expandCmd ctx) buildCmds
      let runCmd' = expandCmd ctx runCmd

      buildOutputs <- runDependentCommands buildCmds'

      -- If all the build commands are successful, go ahead and run several
      -- binary instances conforming to the schema against the generated
      -- code under test.
      --
      -- If anything failed, print a summary of the failure.
      if all runWasSuccessful buildOutputs
        then testCmdWithSchemaInstances runCmd' spec instanceCount >>= renderResults
        else putStrLn "Build failure:" >> printResult (last buildOutputs) >> return 1

printHeaderText :: OPT.TesterOpts -> IO ()
printHeaderText opts = do
  putStrLn "Running crucible."
  putStrLn $ "  Schema count: " ++ show schemaCount
  putStrLn $ "  Types per schema: " ++ show typeCount
  putStrLn $ "  Instance from each schema: " ++ show instCount
  putStrLn $ "  Maximum encoded size of each type: " ++ show maxEncSize
  putStrLn $ "  Allowed prototypes: " ++ usePrototypesStr
  where
    schemaCount = OPT.schemaCount opts
    instCount = OPT.instanceCount opts
    typeCount = OPT.schemaTypeCount opts
    maxEncSize = OPT.schemaEncSize opts
    usePrototypesStr = L.intercalate ", " $ map protoVarToStr (OPT.allowedPrototypes opts)

-- Use a context to expand variables in a command.
expandCmd :: Context -> T.Text -> T.Text
expandCmd ctx cmd = repSpecPath . repDirPath $ cmd
  where
    repSpecPath = T.replace "%s" (specificationPath ctx)
    repDirPath = T.replace "%d" (currentDir ctx)

-- | Run each command in order. If any command fails, do not run the remaining
-- commands.
runDependentCommands :: [T.Text] -> IO [BuildCmdOutput]
runDependentCommands [] = return []
runDependentCommands (c:cmds) = do
  putStrLn $ "Running command: " ++ T.unpack c
  c' <- runBuildCmd c
  if runWasSuccessful c'
    then liftM (c':) (runDependentCommands cmds)
    else return [c']

-- | Convenience function that evaluates to True when a RunOutput represents a
-- successful termination.
runWasSuccessful :: BuildCmdOutput -> Bool
runWasSuccessful BuildCmdOutput { buildExitCode = e } = e == ExitSuccess

-- | Run the specified text as a build command. Package the outputs.
runBuildCmd :: T.Text -> IO BuildCmdOutput
runBuildCmd cmd = do
  (_, _, _, ph) <- createProcess shelled
  e <- waitForProcess ph

  return BuildCmdOutput { buildCmdStr = cmd
                        , buildExitCode = e
                        }
  where
    shelled = (shell $ T.unpack cmd) { std_out = Inherit
                                     , std_err = Inherit
                                     }

-- Tests the ability of an executable to transcode an instance of a type from
-- the schema that correstponds to the passed specification and meta file.
--
-- One instance is tested per invocation of the process.
--
-- NOTE: currently there is no timeout. If the child process hangs, everything
-- hangs.
--
-- TODO: come up with timeout mechanism.
testCmdWithSchemaInstances :: T.Text  -- ^ the path to the exectuable to test
                           -> SP.Specification -- ^ the specification from which to generate the instance
                           -> Integer -- ^ how many instances to test
                           -> IO [TestOutput]
testCmdWithSchemaInstances _ _ 0 = return []
testCmdWithSchemaInstances cmd spec count = do
  -- Create the process. Grab its stdin and stdout.
  (Just stdih, Just stdoh, Just stdeh, ph) <- createProcess shelled
  (result, unpacked, packed) <- runTest stdih stdoh spec

  -- Wait for the process to terminate, collect the result of stdout, package
  -- everything up properly according to the exit status of the process.
  e <- waitForProcess ph
  errorOutput <- T.hGetContents stdeh

  let o r = TestOutput { testResult = r
                       , testStdErr = errorOutput
                       , testEncodedInstance = packed
                       , testInstance = unpacked }
  case e of
     ExitSuccess -> case result of
                      TestPass -> liftM (o result:) (testCmdWithSchemaInstances cmd spec (count - 1))
                      TestFail -> return [o result]
                      te -> return [o te]
     ExitFailure c -> let te = TestError { testErrorMessage = "Client process exited with failure code: " `T.append` (T.pack . show) c }
                      in return [o te]
  where
    shelled = (shell $ T.unpack cmd) { std_out = CreatePipe
                                     , std_err = CreatePipe
                                     , std_in = CreatePipe
                                     }

-- Run a single test against a binary instance of a schema.
runTest :: Handle -- ^ stdin of the process under test
        -> Handle -- ^ stdout of the process under test
        -> SP.Specification -- ^ specification derived from the schema under test
        -> IO (TestResult, D.MetaType, B.ByteString)
runTest ih oh spec = do
  -- Generate a type according to the specification, then pack it to binary.
  mt <- D.dynamicMetaGen spec
  let packed = D.dynamicMetaPack spec mt

  -- We keep this here as a sanity check. If this hits, it *IS* an error. We
  -- don't want to continue if this happens. Ever.
  let packedLen = fromIntegral $ B.length packed
  let specMaxLen = C.sizeMax $ SP.specSize spec
  when (packedLen > (specMaxLen + fromIntegral hlen))
       (error $ "LENGTH OF BS TOO LONG! Expected "
             ++ show packedLen
             ++ " to be less than "
             ++ show specMaxLen ++ ".")

  -- Setup a *THREAD* to do write the encoded type to the process-under-test's
  -- stdin. This is in a separate thread because it's likely we can deadlock
  -- for larger types due to OS buffering rules.
  encodeDone <- newEmptyMVar
  _ <- flip forkFinally (\_ -> putMVar encodeDone ()) $ do
      B.hPut ih packed
      hClose ih
      putMVar encodeDone ()

  -- Begin decoding from the process' stdout.
  hdrBytes <- B.hGet oh hlen
  let hdr = D.dynamicMetaUnpackHeader spec hdrBytes

  result <- case hdr of
              -- Unable to unpack the header.
              Left err -> return $ TestError (T.pack err)

              -- Got a header.
              Right (mh, _) -> do
                -- Read the payload bytes.
                payload <- B.hGet oh (fromIntegral . D.metaLength $ mh)

                -- Try to unpack the types from the payload bytes.
                return $ case D.dynamicMetaUnpackFromHeader spec mh payload of
                          Left str -> TestError { testErrorMessage = str }
                          Right (mt', rest ) | not (B.null rest) -> TestError { testErrorMessage =
                                                                      "Not all bytes were consumed: " `T.append` (T.pack . show . B.unpack) rest }
                                             | otherwise -> if mt /= mt'
                                                              then TestFail
                                                              else TestPass
  -- Make sure the encoding thread is finished.
  _ <- takeMVar encodeDone

  -- Dump something on the console to represent the running state.
  case result of
    TestPass -> putStr "."
    _ -> putStrLn "X"
  hFlush stdout

  return (result, mt, packed)
  where
    hlen = fromIntegral (SP.specTypeLength spec)
         + (fromIntegral . C.sizeMax . C.tagToSize . SP.specLengthTag) spec

-- Output the result of a build command.
printResult :: BuildCmdOutput -> IO ()
printResult BuildCmdOutput { buildCmdStr = cs, buildExitCode = ec } =
  case ec of
    ExitSuccess -> T.putStrLn "## SUCCESS"
    ExitFailure c -> do
      T.putStrLn $ "## FAILED: " `T.append` (T.pack . show) c
      T.putStrLn $ "## Command String: " `T.append` cs

-- Do something pretty with a list of TestOutput types.
renderResults :: [TestOutput] -> IO Int
renderResults rs = go rs 0
  where
    successStr = "\nSchema success!"
    failStr = "\nSCHEMA FAILED!"

    go [] 0 = putStrLn successStr >> return (0 :: Int)
    go [] n = putStrLn failStr >> return n
    go (TestOutput t e b mt:rest) failures = case t of
                                              TestPass -> go rest failures
                                              TestError m -> printErr m >> go rest (failures + 1)
                                              TestFail -> printFail >> go rest (failures + 1)
      where
        printErr m = do T.putStrLn $ "Error: " `T.append` m
                        T.putStrLn $ "Error encoded bytes: " `T.append` (T.pack . show . B.unpack) b
                        T.putStrLn $ "Standard error output: " `T.append` e
                        T.putStrLn $ "Show metatype: " `T.append` (T.pack . show) mt
        printFail = do T.putStrLn $ "Failure for encoded bytes: " `T.append` (T.pack . show . B.unpack) b
                       T.putStrLn $ "Standard error output: " `T.append` e
                       T.putStrLn $ "Show metatype: " `T.append` (T.pack . show) mt
