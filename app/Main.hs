import Control.Exception
import System.Directory
import Data.Text
import System.FilePath
import Options.Applicative

data Opts = Opts { opt_path :: !String }

opts_parser :: ParserInfo Opts
opts_parser = info (helper <*> version <*> opts) (fullDesc <> progDesc "Generic text file generator from tags." <> header "Template-generator")
version :: Parser (a -> a)
version = infoOption "0.1" (long "version" <> help "Show version")
opts :: Parser Opts
opts = Opts <$> strOption (long "file-path" <> help "Template path.")

check_file :: String -> IO (Either SomeException String)
check_file path = try $ readFile path

check_folder :: String -> IO (Either SomeException [String])
check_folder path = try $ getDirectoryContents path

replace_tag :: String -> String -> String -> String
replace_tag "" _ _ = ""
replace_tag template "" _ = template
replace_tag template content tag = unpack $ replace (pack tag) (pack content) (pack template)

replace_tags :: String -> [String] -> String -> IO(String)
replace_tags _ [] template = return template
replace_tags path (x:xs) template = do
  is_dir <- (check_folder $ (joinPath [(takeDirectory path), x]))
  case is_dir of
    Right _ -> replace_tags path xs template
    Left _ -> do
      content <- (readFile (joinPath[(takeDirectory path), x]))
      replace_tags path xs $ replace_tag template content (['{'] ++ x ++ ['}'])

main :: IO ()
main = do
  options <- execParser opts_parser
  content <- check_file $ opt_path options
  files <- check_folder $ takeDirectory $ opt_path options
  case (content, files) of
    ( Left _ , _ ) -> putStrLn "File not found"
    ( _ , Left _ ) -> putStrLn "Folder not found"
    (Right a, Right b) -> do
      t <- replace_tags (opt_path options) b a
      writeFile ((opt_path options) ++ "_") t
