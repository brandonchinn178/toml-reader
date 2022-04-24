{-# LANGUAGE LambdaCase #-}

import qualified Data.Text.IO as Text
import System.Environment (getArgs, getExecutablePath)

main :: IO ()
main =
  getArgs >>= \case
    [] -> runTomlTest
    ["--check"] -> checkTOML
    args -> error $ "Invalid arguments: " ++ show args

-- | Run 'toml-test -- $0 --check'
runTomlTest :: IO ()
runTomlTest = do
  exe <- getExecutablePath
  -- TODO
  print exe

-- | Parse TOML data in stdin.
checkTOML :: IO ()
checkTOML = do
  input <- Text.getContents
  -- TODO
  print input
