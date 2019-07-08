module Main (main) where

import Language.ELisp
import Language.ELisp.Parser

import Control.Monad

import System.Directory
import System.Exit

-- getDirectoryContents :: FilePath -> IO [FilePath]
-- getDirectoryContents _ = return ["01.el" , "02.el"]

parse1 :: FilePath -> IO Bool
parse1 file = either (const False) (const True)
          <$> parseFile file

main :: IO ()
main = do
  putStrLn "\n"
  files <- filter notDots <$> getDirectoryContents "tests/ELispFiles"
  flip mapM_ files $ \f -> do
    putStr ("Parsing: " ++ f ++ " ...")
    success <- parseFile ("tests/ELispFiles/" ++ f)
    case success of
      Right _  -> putStrLn "OK"
      Left err -> putStrLn ("FAIL\n" ++ show err) >> exitFailure
  exitSuccess
 where
   notDots "."  = False
   notDots ".." = False
   notDots _    = True
