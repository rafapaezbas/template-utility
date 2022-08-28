import Control.Exception
import System.Directory
import Data.Text
import System.FilePath
import Options.Applicative

data Opts = Opts { opt_path :: !String }

optsParser :: ParserInfo Opts
optsParser = info (helper <*> version <*> opts) (fullDesc <> progDesc "Generic text file generator from tags." <> header "Template-generator")
version :: Parser (a -> a)
version = infoOption "0.1" (long "version" <> help "Show version")
opts :: Parser Opts
opts = Opts <$> strOption (long "file-path" <> help "Template path.")

done_message :: String
done_message = "Done!"

checkFile :: String -> IO (Either SomeException String)
checkFile path = (try $ readFile path) :: IO (Either SomeException String)

checkFolder :: String -> IO (Either SomeException [String])
checkFolder path = try $ getDirectoryContents (takeDirectory path)

replace_tags :: String -> [String] -> String -> IO()
replace_tags path (x:xs) template = do
  content <- readFile (joinPath[(takeDirectory path), x])
  replace_tags path xs $ replace_tag template content (['{'] ++ x ++ ['}'])
replace_tags path [] template = do
  putStrLn done_message
  writeFile (path ++ "_") template

replace_tag :: String -> String -> String -> String
replace_tag "" _ _ = ""
replace_tag template "" _ = template
replace_tag template content tag = unpack $ replace (pack tag) (pack content) (pack template)

main :: IO ()
main = do
  options <- execParser optsParser
  content <- checkFile $ opt_path options
  files <- checkFolder $ (takeDirectory $ opt_path options)
  case (content, files) of
    ( Left _ , _ ) -> putStrLn "File not found"
    ( _ , Left _ ) -> putStrLn "Folder not found"
    (Right a, Right b) -> replace_tags (opt_path options) ["file2.txt", "file3.txt"]  a
