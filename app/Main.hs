{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Maybe
import Data.Char
import Data.List
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Error.Class
import Control.Monad.IO.Class

import Control.Concurrent

import System.FilePath
import System.Directory

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Status as HTTP

import Text.HTML.Scalpel

import Options.Applicative
import Options.Applicative.Help (softline, linebreak, string, indent, fill, vcat)



-------------- Plaintext Tree

data Formatted
  = Newline
  | Plaintext Text
  | Paragraph [Formatted]
  | Collect [Formatted]
  | Skip
  deriving Show

render :: Formatted -> Text
render Newline = "\n"
render (Plaintext s) = s
render (Paragraph fs) = renders fs <> "\n\n"
render (Collect fs) = renders fs
render Skip = ""

renders :: [Formatted] -> Text
renders = T.concat . fmap render



--------------- Name Format

data Padding = NoPad | YesPad deriving Show

data NameComponent = Txt String | ArcName | ArcNumber Padding | ChapterNumber Padding deriving Show

type NameFmt = [NameComponent]

showIntPadding :: Padding -> Int -> String
showIntPadding NoPad = show
showIntPadding YesPad = justifyRight 2 '0' . show

renderName :: ChapterInfo -> NameFmt -> String
renderName inf = foldr f "" where
  f (Txt t) n = t <> n
  f ArcName n = T.unpack inf.arcName <> n
  f (ArcNumber p) n = showIntPadding p inf.arcNumber <> n
  f (ChapterNumber p) n = showIntPadding p inf.chapter <> n

parseNameComponent :: MonadError String m => Char -> m NameComponent
parseNameComponent 'n' = pure ArcName
parseNameComponent 'A' = pure (ArcNumber YesPad)
parseNameComponent 'a' = pure (ArcNumber NoPad)
parseNameComponent 'C' = pure (ChapterNumber YesPad)
parseNameComponent 'c' = pure (ChapterNumber NoPad)
parseNameComponent c = throwError $ "Failed to parse name component " ++ [c]

parseFormat :: MonadError String m => String -> m NameFmt
parseFormat st = when (null st) (throwError "FORMAT may not be empty") >> go st where
  go t = let (p, s) = break (== '%') t in
    case uncons (drop 1 s) of
      Nothing -> return [Txt p]
      Just (c, s') -> do
        comp <- parseNameComponent c
        rest <- go s'
        return $ Txt p : comp : rest



--------------- Command Line Parameters

data Params = Params
  { dontProcess :: Bool
  , localRaws :: Maybe FilePath
  , discardRaws :: Bool
  , baseDir :: FilePath
  , dirInfo :: NameFmt
  }

formatReader :: ReadM NameFmt
formatReader = eitherReader (runExcept . parseFormat)

defaultFormat :: NameFmt
defaultFormat = [ArcNumber NoPad, Txt "-", ArcName, Txt [pathSeparator], ArcName, Txt "-", ArcNumber NoPad, Txt "-", ChapterNumber NoPad]

optionalStr :: IsString a => ReadM (Maybe a)
optionalStr = maybeReader (Just . Just . fromString)

optParser :: Parser Params
optParser = Params
  <$> switch (long "download-only" <> short 'd' <> help "Download chapters from katalepsis.net but don't extract their content")
  <*> option optionalStr (value Nothing <> long "local-chapters" <> short 'l' <> metavar "DIR" <> help "Use chapters in DIR instead of downloading them from katalepsis.net")
  <*> switch (long "discard-html" <> short 'x' <> help "Don't save unprocessed html files")
  <*> strOption (long "out-dir" <> short 'o' <> metavar "DIR" <> value (addTrailingPathSeparator "chapters") <> help "The base directory in which to save output files")
  <*> option formatReader (value defaultFormat <> long "format" <> short 'f' <> metavar "FORMAT" <> (helpDoc $ Just doc))
  where
    para = foldr (\d d' -> string d <> softline <> d') mempty . words
    doc =  para "The naming scheme used when saving chapter files." <> linebreak <> para "May contain the following format specifiers:" <> linebreak
        <> indent 5 (vcat $ specsDoc <$> specs) <> linebreak
        <> para ("(default: '%a-%n" ++ [pathSeparator] ++ "%n-%a-%c')")
    specs =
      [ ("%n", "Arc name")
      , ("%a", "Arc number")
      , ("%c", "Chapter number")
      , ("%A", "Arc number with leading zeroes")
      , ("%C", "Chapter number with leading zeroes")
      ]
    specsDoc (s, t) = fill 5 (string s) <> string t

opts :: ParserInfo Params
opts = info (optParser <**> helper)
  $ fullDesc
  <> progDesc "Download and extract plaintext content from Katalepsis chapters"
  <> header "kata-dl -- A Katalepsis Archival Program"



--------------- Main

main :: IO ()
main = do
  params <- execParser opts
  sources <- case params.localRaws of
    Nothing -> getKataUrls
    Just path -> getFiles path
  forM_ sources (getRaw (isJust params.localRaws) >=> processChapter params)




--------------- Katalepsis Scraping

getKataUrls :: IO [String]
getKataUrls = do
  raw <- getUrl "https://katalepsis.net/table-of-contents/"
  case scrapeStringLike raw (content extractUrls) of
    Nothing -> error "Error while scraping index for URLs"
    Just us -> pure us

getRaw :: Bool -> String -> IO Text
getRaw local = if local then T.readFile else ((putStrLn "Waiting..." >> threadDelay 2000000 >>) . getUrl)

processChapter :: Params -> Text -> IO ()
processChapter params raw = do
  (inf, mfmt) <- runExceptF . scrapeStringLikeF "Error while scraping html" raw $ scrapeChapter params.dirInfo params.dontProcess
  unless (params.discardRaws || isJust params.localRaws) $ write inf Raw raw
  maybeM mfmt $ write inf Plain . renders
  where
    write = writeKataFile params.baseDir params.dirInfo

scrapeChapter :: (MonadError String m, MonadIO m) => NameFmt -> Bool -> ScraperT Text m (ChapterInfo, Maybe [Formatted])
scrapeChapter fmt dontProc = do
  url <- originalUrl
  inf <- parseKataUrl url
  if not dontProc then do
    liftIO $ T.putStrLn $ "Processing " <> url
    t <- content extractText
    return (inf, Just t)
  else
    return (inf, Nothing)

extractText :: Monad m => ScraperT Text m [Formatted]
extractText = inSerial . many . stepNext $ extractOne where

  extractOne = newline <|> plaintext <|> paragraph <|> link <|> skip

  newline = Newline <$ matches (sister "br")
  plaintext = Plaintext <$> text (sister textSelector)
  paragraph = Paragraph <$> chroot (sister "p") extractText
  link = Skip <$ matches (sister ("a" @: [match $ const . (== "href")]))
  skip = Collect <$> chroot anySelector extractText

content :: Monad m => ScraperT Text m a -> ScraperT Text m a
content = chroot ("div" @: [hasClass "entry-content"])

extractUrls :: Scraper Text [String]
extractUrls = fmap T.unpack <$> attrs "href" "a"

originalUrl :: Monad m => ScraperT Text m Text
originalUrl = attr "href" ("link" @: ["rel" @= "canonical"])



--------------- File Writing

data FileType = Plain | Raw

toplevel :: FilePath
toplevel = "./output/"

directory :: FileType -> FilePath
directory Plain = "plaintext"
directory Raw = "html"

extension :: FileType -> String
extension Plain = "txt"
extension Raw = "html"

writeKataFile :: FilePath -> NameFmt -> ChapterInfo -> FileType -> Text -> IO ()
writeKataFile basedir fmt inf ty t = do
  let outFile = basedir </> directory ty </> renderName inf fmt -<.> extension ty
      outDir = takeDirectory outFile
  createDirectoryIfMissing True outDir
  putStrLn $ "Writing " ++ outFile
  T.writeFile outFile t




--------------- Chapter Info

data ChapterInfo = KC { fullUrl :: Text , fullName :: Text , arcName :: Text , arcNumber :: Int , chapter :: Int } deriving Show

parseKataUrl :: MonadError String m => Text -> m ChapterInfo
parseKataUrl url = let
  fullName = correctArc1Name . last . T.splitOn "/" . dropSuffix "/" $ url
  (arcName, numStr) = T.break isDigit $ fullName
  (arcStr, chapStr) = T.break (== '-') $ numStr
  in do
    arc <- readNumber "arc" arcStr
    chap <- readNumber "chapter" (T.tail chapStr)
    return $ KC url fullName (T.init arcName) arc chap
  where
    readNumber ty s = either
      (const $ throwError $ "Error: Couldn't parse " ++ ty ++ " number '" ++ T.unpack s ++ "' in url '" ++ T.unpack url ++ "'")
      (return . fst)
      (T.decimal s)

correctArc1Name :: Text -> Text
correctArc1Name s = case T.stripPrefix "mindcorrelating" s of
  Just rest -> "mind-correlating" <> rest
  Nothing -> s



--------------- Utils

runExceptF :: MonadFail m => ExceptT String m a -> m a
runExceptF x = do
  res <- runExceptT x
  case res of
    Left err -> fail err
    Right a -> return a

scrapeStringLikeF :: MonadFail m => String -> Text -> ScraperT Text m a -> m a
scrapeStringLikeF e t s = do
  res <- scrapeStringLikeT t s
  case res of
    Nothing -> fail e
    Just a -> return a

sister :: Selector -> Selector
sister = flip atDepth 0


getUrl :: String -> IO Text
getUrl url = do
  manager <- HTTP.getGlobalManager
  request <- HTTP.parseRequest url
  putStrLn $ "Downloading " ++ url
  response <- HTTP.httpLbs request manager
  putStrLn $ "Got status: " ++ (show . HTTP.statusCode . HTTP.responseStatus $ response)
  return . T.decodeUtf8 . LBS.toStrict . HTTP.responseBody $ response

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  entries <- listDirectory path
  (files, dirs) <- partitionM doesFileExist ((path </>) <$> entries)
  files' <- mapM getFiles dirs
  return (files ++ concat files')


maybeM :: Applicative f => Maybe a -> (a -> f ()) -> f ()
maybeM Nothing f = pure ()
maybeM (Just a) f = f a

partitionM :: Applicative f => (a -> f Bool) -> [a] -> f ([a], [a])
partitionM p = foldr f (pure ([], [])) where
  f x xs = g x <$> p x <*> xs
  g x b (l, r) = if b then (x : l, r) else (l, x : r)

dropSuffix :: Text -> Text -> Text
dropSuffix s t = case T.stripSuffix s t of
  Nothing -> t
  (Just t') -> t'

justifyRight :: Int -> Char -> String -> String
justifyRight l c s
  | l > length s = replicate (l - length s) c ++ s
  | otherwise = s
