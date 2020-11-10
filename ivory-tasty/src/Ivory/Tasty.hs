{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Test Ivory functions by executing compiled C programs.
--
-- Example usage:
--
-- > import Ivory.Language
-- > import Ivory.Serialize
-- > import Ivory.Tasty
--
-- > someDeps :: [Module]
-- > someDeps = [serializeModule]
-- >
-- > someArtifacts :: [Located Artifact]
-- > someArtifacts = serializeArtifacts
--
-- > someTestedFunction :: Def ('[] ':-> Sint32)
-- > someTestedFunction = proc "main" $ body $ do
-- >   assert false
-- >   ret 0
--
-- > main :: IO ()
-- > main = defaultMain $ ivoryTestGroup someDeps someArtifacts [ mkSuccess "test description" someTestedFunction ]
--
-- With module dependencies:
--
-- > main :: IO ()
-- > main = defaultMain
-- >   $ ivoryTestGroup someDeps someArtifacts
-- >   $ map (setDeps (depend uartTypes >> depend serializeModule)) -- set deps for all tests
-- >     [ mkSuccess "serialization test" samplesTest ]
--
-- Dependencies per test:
--
-- > main :: IO ()
-- > main = defaultMain
-- >   $ ivoryTestGroup someDeps someArtifacts
-- >     [ setDeps (depend uartTypes) $ mkSuccess "uart test" uartTest
-- >     , setDeps (depend serializeModule) $ mkSuccess "serialize test" serializeTest ]
--

--}

module Ivory.Tasty (
    ivoryTestGroup
  , defaultMain
  , mkSuccess
  , mkFailure
  , setDeps
  , TestTree
  , compileAndRun
  ) where

import Ivory.Artifact
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import System.Exit (ExitCode(ExitSuccess))
import qualified System.IO
import qualified System.IO.Temp
import qualified System.Process
import qualified Text.Printf

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase)

data IvoryTestCase = IvoryTestCase {
    itcName          :: String
  , itcFun           :: Def ('[] ':-> Sint32)
  , itcDeps          :: ModuleDef
  , itcExpectSuccess :: Bool
  }

-- | Create `IvoryTestCase` from test name and function,
-- expecting 0 exit code.
mkSuccess
  :: String
  -> Def ('[] ':-> Sint32)
  -> IvoryTestCase
mkSuccess name f = IvoryTestCase name f (pure ()) True

-- | Create `IvoryTestCase` from test name and function,
-- expecting non-0 exit code.
mkFailure
  :: String
  -> Def ('[] ':-> Sint32)
  -> IvoryTestCase
mkFailure name f = IvoryTestCase name f (pure ()) False

-- | Set additional `IvoryTestCase` dependencies for main module
setDeps :: ModuleDef -> IvoryTestCase -> IvoryTestCase
setDeps ds x = x { itcDeps = ds }

-- | Build a `TestTree` for list of main functions and their ModuleDeps,
-- sharing common dependencies and artifacts.
ivoryTestGroup
  :: [Module]
  -> [Located Artifact]
  -> [IvoryTestCase]
  -> TestTree
ivoryTestGroup globalDeps globalArtifacts testCases = testGroup "ivory-tasty" $ map mk testCases
  where
    mk IvoryTestCase{..} = testCase itcName $ do
      (o, r) <- compileAndRun (mkModule itcFun itcDeps) globalDeps globalArtifacts
      case itcExpectSuccess of
        True -> assertBool ("Expected sample to exit cleanly. Output:\n" ++ o) r
        False -> assertBool ("Expected sample to fail. Output:\n" ++ o) (not r)
    mkModule f modDefs = package "main" $ do
      incl f
      modDefs

-- | Compile main module with its dependencies and artifacts.
-- Run the result and check whether we get zero exit-code.
--
-- Returns stdout and stderr contents of the process for
-- further examination.
compileAndRun
  :: Module
  -> [Module]
  -> [Located Artifact]
  -> IO (String, Bool)
compileAndRun mainMod deps artifacts = System.IO.Temp.withSystemTempDirectory "ivory-tasty" $ \testDir -> do
  runCompiler
    ([mainMod] ++ deps)
    artifacts
    initialOpts { outDir = Just testDir }

  let cmd = Text.Printf.printf "(cd %s && cc -g -I. -std=c99 -DIVORY_TEST *.c && ./a.out)" testDir
  (_,oh,eh,pid) <- System.Process.runInteractiveCommand cmd
  code <- System.Process.waitForProcess pid
  out <- System.IO.hGetContents oh
  err <- System.IO.hGetContents eh
  return (out ++ "\n" ++ err, code == ExitSuccess)
