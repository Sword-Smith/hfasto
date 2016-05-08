module Main where

import System.Environment
import MipsCodeGenerator as MCG
import Data.List.Split

main :: IO ()
main = do
  files <- getArgs
  case files of
    f:[] -> do
      putStrLn ("Reading from file " ++ f)
      let fBase = head (splitOn ".fo" f)
      putStrLn ("Writing to file " ++ (fBase ++ ".asm"))
      source <- readFile f
      writeFile (fBase ++ ".asm") (MCG.compileAFile source)
    _            -> putStrLn "Usage: Main <source file name.fo>"
