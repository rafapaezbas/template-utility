{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import System.Directory
import Data.Text
import System.FilePath

done_message :: String
done_message = "Done!"

path :: String
path = "/home/rpaezbas/Desktop/file.txt"

cat :: String -> IO (Either SomeException String)
cat path' = (try $ readFile path') :: IO (Either SomeException String)

replace_tags :: [String] -> String -> IO()
replace_tags [] template = do
  writeFile (path ++ "_") template
  putStrLn done_message
replace_tags (x:xs) template = do
  content <- readFile ((takeDirectory path) ++ "/" ++ x)
  replace_tags xs $ replace_tag template content (['{'] ++ x ++ ['}'])


replace_tag :: String -> String -> String -> String
replace_tag "" _ _ = ""
replace_tag template "" _ = template
replace_tag template content tag = unpack $ replace (pack tag) (pack content) (pack template)

main :: IO ()
main = do
  content <- cat path
  files <- try $ getDirectoryContents (takeDirectory path) :: IO (Either SomeException [FilePath])
  case (content, files) of
    ( Left _ , _ ) -> putStrLn "File not found"
    ( _ , Left _ ) -> putStrLn "Folder not found"
    (Right a, Right b) -> replace_tags ["file2.txt", "file3.txt"]  a
