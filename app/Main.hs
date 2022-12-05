{-# LANGUAGE OverloadedLists #-}

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor
import Text.Read (readMaybe)

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Options.Applicative
import Options.Applicative.Help (softline, linebreak, string, indent, fill, vcat)

import Kata.Base
import Kata.Scrape


main :: IO ()
main = do
  params' <- execParser opts
  params <- case params' of
    Just p -> return p
    Nothing -> getParamsInteractive
  flip runReaderT params $ do
    sources <- getSources
    forM_ sources (prepareSource >=> processChapter)




--------------- Command Line Parameters

formatReader :: ReadM NameFmt
formatReader = eitherReader (first show . parseFormat)

interactiveParser :: Parser (Maybe Params)
interactiveParser = Nothing <$ switch (long "interactive" <> short 'i' <> help "Give options interactively")

optParser :: Parser (Maybe Params)
optParser = switch (long "command-line" <> short 'c' <> help "Give options as command line arguments") *> fmap Just (Params
  <$> switch (long "download-only" <> short 'd' <> help "Download chapters from katalepsis.net but don't extract their content")
  <*> (optional $ strOption (long "local-chapters" <> short 'l' <> metavar "DIR" <> help "Use chapters in DIR instead of downloading them from katalepsis.net"))
  <*> switch (long "discard-html" <> short 'x' <> help "Don't save unprocessed html files")
  <*> option formatReader (value defaultFormat <> long "format" <> short 'f' <> metavar "FORMAT" <> (helpDoc $ Just doc))
  )
  where
    para = foldr (\d d' -> string d <> softline <> d') mempty . words
    doc =  para "The naming scheme used when saving chapter files" <> linebreak <> para "May contain the following format specifiers:" <> linebreak
        <> indent 5 (vcat $ specsDoc <$> fmtSpecs) <> linebreak
        <> para ("(default: " ++ defaultFmtString ++ ")")

    specsDoc (s, t) = fill 5 (string s) <> string t

opts :: ParserInfo (Maybe Params)
opts = info (interactiveParser <|> optParser <**> helper)
  $ fullDesc
  <> progDesc "Download and extract plaintext content from Katalepsis chapters"
  <> header "kata-dl -- A Katalepsis Archival Program"





--------------- Interactive Parameters

getOpt :: a -> (String -> Maybe a) -> IO a
getOpt d act = loop where
  loop = do
    input <- getLine
    if null input then
      return d
    else case act input of
      Nothing -> putStrLn "invalid input, please try again" >> loop
      Just x -> putStrLn "" >> return x

getSwitch :: NonEmpty (String, a) -> IO a
getSwitch os = printOpts >> getOpt (snd . NE.head $ os) (readMaybe >=> index as . pred) where
  as = NE.toList . fmap snd $ os
  addDefault (x:|xs) = (x ++ " (default)") :| xs
  os' = zip ([1..] :: [Int]) . NE.toList . addDefault . fmap fst $ os
  printOpts = mapM_ (\(i, s) -> putStrLn ("  " ++ show i ++ ") " ++ s)) os'


askDownload :: IO (Maybe FilePath, Bool, Bool)
askDownload = do
  putStrLn "Do you want to download the chapters from katalepsis.net or use a local source?"
  join $ getSwitch [("Download from katalepsis.net", download), ("Use local chapter files", lcl)]
  where
    download = do
      d <- askDiscard
      e <- askExtract
      return (Nothing, d, e)

    lcl = do
      l <- askLocalSources
      return (Just l, True, False)

askDiscard :: IO Bool
askDiscard = do
  putStrLn "Do you wish to keep the original html files?"
  getSwitch [("Yes", False), ("No", True)]

askLocalSources :: IO FilePath
askLocalSources = do
  putStrLn "Please enter the directory you want to read the chapters from"
  resp <- getLine
  putStrLn ""
  return resp

askExtract :: IO Bool
askExtract = do
  putStrLn "Would you like to extract the chapter content?"
  getSwitch [("Yes", False), ("No", True)]


askFormat :: IO NameFmt
askFormat = do
  putStrLn "Please enter the naming scheme to be used for the output"
  putStrLn "The scheme may use the following format specifiers:"
  mapM_ (\(s, d) -> putStrLn ("  " ++ s ++ "     " ++ d)) fmtSpecs
  putStrLn $ "(default: " ++ defaultFmtString ++ ")"
  getOpt defaultFormat $ \s -> case parseFormat s of
    Left _ -> Nothing
    Right fmt -> Just fmt


getParamsInteractive :: IO Params
getParamsInteractive = do
  (lcl, discard, dontExtract) <- askDownload
  fmt <- askFormat
  return Params { dontExtract = dontExtract, localRaws = lcl, discardRaws = discard, fileFormat = fmt }
