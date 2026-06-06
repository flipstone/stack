import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- This tests building a project package (pkg) that depends on an immutable
-- dependency (dep) with both a main library and a sub-library, in the case
-- where the dependency is installed by copying it from the precompiled cache.
-- The dependency is first built from source in the custom1 project. The
-- custom2 project uses a different custom snapshot, so it has a distinct
-- snapshot package database and installs the dependency from the precompiled
-- cache. A subsequent build of the custom2 project should then do no work.
-- Previously, the Cabal configuration cache written for the project package
-- recorded the dependency without the package ids of its sub-libraries, while
-- later builds compared it against a set that included them, causing a
-- spurious "dependencies changed" rebuild.

main :: IO ()
main = do
  -- The '--install-ghc' flag is passed here, because IntegrationSpec.runApp
  -- sets up `config.yaml` with `system-ghc: true` and `install-ghc: false`.
  stack ["build", "--install-ghc", "--stack-yaml", "custom1/stack.yaml", "dep"]
  stackCheckStderr ["build", "--stack-yaml", "custom2/stack.yaml"] $ \out ->
    unless ("using precompiled package" `isInfixOf` out) $
      error "Didn't use precompiled package!"
  stackCheckStderr ["build", "--stack-yaml", "custom2/stack.yaml"] $ \out ->
    unless (null (compilingModulesLines out)) $
      error "Stack recompiled code"

-- Returns the lines where a module is compiled
compilingModulesLines :: String -> [String]
compilingModulesLines = filter (isInfixOf " Compiling ") . lines
