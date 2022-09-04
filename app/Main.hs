import Control.Exception
import System.Directory
import Data.Text
import System.FilePath
import Options.Applicative
import Data.List

newtype Opts = Opts {opt_path :: String}

optsParser :: ParserInfo Opts
optsParser = info (helper <*> version <*> opts) (fullDesc <> progDesc "Generic text file generator from tags." <> header "Template-generator")
version :: Parser (a -> a)
version = infoOption "0.1" (long "version" <> help "Show version")
opts :: Parser Opts
opts = Opts <$> strOption (long "file-path" <> help "Template path.")

checkFile :: String -> IO (Either SomeException String)
checkFile path = try $ readFile path

checkFolder :: String -> IO (Either SomeException [String])
checkFolder path = try $ getDirectoryContents path

replaceTag :: String -> String -> String -> String
replaceTag template "" _ = template
replacetag template content tag = unpack $ replace (pack tag) (pack content) (pack template)

replaceTags :: String -> [String] -> String -> IO String
replaceTags _ [] template = return template
replaceTags path (x:xs) template = do
  is_dir <- checkFolder (joinPath [takeDirectory path, x])
  content <- checkFile (joinPath [takeDirectory path, x])
  case (is_dir, content) of
    (Right _, _) -> replaceTags path xs template
    (_, Left _) -> replaceTags path xs template
    (Left _, Right c) -> replaceTags path xs $ replaceTag template c (['{'] ++ x ++ ['}'])

main :: IO ()
main = do
  options <- execParser optsParser
  content <- checkFile $ opt_path options
  files <- checkFolder $ takeDirectory $ opt_path options
  case (content, files) of
    ( Left _ , _ ) -> putStrLn "File not found"
    ( _ , Left _ ) -> putStrLn "Folder not found"
    (Right c, Right f) -> do
      t <- replaceTags (opt_path options) (Data.List.filter (Data.List.isInfixOf extension) f) c
      writeFile (opt_path options ++ "_") t
      where extension = takeExtension $ opt_path options
