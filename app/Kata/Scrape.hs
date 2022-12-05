{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Kata.Scrape where


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Catch

import Control.Concurrent

import System.FilePath
import System.Directory

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Status as HTTP

import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Text.StringLike (StringLike)

import Kata.Base


-- | hacky orphan instance, yay
instance MonadThrow m => MonadThrow (ScraperT str m) where
  throwM e = lift (throwM e)



--------------- Getting chapter files

getKataUrls :: IO [String]
getKataUrls = do
  putStrLn "Fetching chapter list"
  raw <- getUrl "https://katalepsis.net/table-of-contents/"
  case scrapeStringLike raw (content extractUrls) of
    Nothing -> error "Error while scraping index for URLs"
    Just us -> pure us

extractUrls :: Scraper Text [String]
extractUrls = fmap T.unpack <$> attrs "href" "a"


data Source = Remote String | Local FilePath

isLocal :: Source -> Bool
isLocal (Remote _) = False
isLocal (Local _) = True

instance Show Source where
  show (Remote url) = url
  show (Local path) = path

getSources :: (MonadReader Params m, MonadIO m) => m [Either String FilePath]
getSources = do
  params <- ask
  case params.localRaws of
    Nothing -> fmap Left <$> liftIO getKataUrls
    Just path -> fmap Right <$> liftIO (getFiles path)

prepareSource :: (MonadReader Params m, MonadIO m, MonadThrow m) => Either String FilePath -> m Source
prepareSource (Right p) = return $ Local p
prepareSource (Left url) = do
  params <- ask
  inf <- parseKataUrl (T.pack url)
  let name = filename params.fileFormat inf Raw
  existsH <- liftIO $ doesFileExist name
  if existsH then do
    liftIO $ putStrLn (name ++ " exists, using local file instead of downloading again")
    return $ Local name
  else
    return $ Remote url

getRaw :: MonadIO m => Source -> m Text
getRaw (Local p) = liftIO $ T.readFile p
getRaw (Remote url) = liftIO $ do
  putStrLn "Waiting..."
  threadDelay 2000000
  getUrl url




--------------- Processing Chapters

processChapter :: (MonadReader Params m, MonadIO m, MonadThrow m) => Source -> m ()
processChapter src = do
  params <- ask
  liftIO $ putStrLn $ "Processing " ++ show src
  raw <- getRaw src
  let tags = parseTags raw
  inf <- scrapeE (ScrapeException "Error while extracting chapter info") extractInfo tags
  unless (params.discardRaws || isLocal src) $ writeKataFile params.fileFormat inf Raw raw
  unless params.dontExtract $ do
    let name = filename params.fileFormat inf Plain
    exists <- liftIO $ doesFileExist name
    if exists then
      liftIO $ putStrLn (name <> " exists, skipping")
    else do
      liftIO $ putStrLn "Extracting chapter text"
      fmt <- scrapeE (ScrapeException "Error while extracting chapter text") (content extractText) tags
      writeKataFile params.fileFormat inf Plain (render fmt)

writeKataFile :: MonadIO m => NameFmt -> ChapterInfo -> FileType -> Text -> m ()
writeKataFile fmt inf ty t = liftIO $ do
  let outFile = filename fmt inf ty
      outDir = takeDirectory outFile
  createDirectoryIfMissing True outDir
  putStrLn $ "Writing " ++ outFile
  T.writeFile outFile t


extractInfo :: MonadThrow m => ScraperT Text m ChapterInfo
extractInfo = originalUrl >>= parseKataUrl

extractText :: Monad m => ScraperT Text m Formatted
extractText = Collect <$> manyBetween linkPara linkPara extractOne where

  extractOne = newline <|> plaintext <|> paragraph <|> other
  go = inSerial . many $ stepNext extractOne

  newline = Newline <$ matches ("br" `atDepth` 0)
  plaintext = Plaintext <$> text (textSelector `atDepth` 0)
  paragraph = Paragraph <$> chroot ("p" `atDepth` 0) go
  other = Collect <$> chroot anySelector go

  linkPara = chroot ("p" `atDepth` 0) $ matches "a"

content :: Monad m => ScraperT Text m a -> ScraperT Text m a
content = chroot ("div" @: [hasClass "entry-content"])

originalUrl :: Monad m => ScraperT Text m Text
originalUrl = attr "href" ("link" @: ["rel" @= "canonical"])




--------------- Utils

scrapeE :: (MonadThrow m, Exception e) => e -> ScraperT Text m a -> [Tag Text] -> m a
scrapeE e s ts = do
  res <- scrapeT s ts
  case res of
    Nothing -> throwM e
    Just a -> return a

manyBetween :: (Monad m, StringLike t) => ScraperT t m () -> ScraperT t m () -> ScraperT t m a -> ScraperT t m [a]
manyBetween l r s = inSerial $ seekNext l >> loop where
  loop = do
    res <- stepNext next
    case res of
      Left _ -> return []
      Right a -> (a:) <$> loop
  next = (Left <$> r) <|> (Right <$> s)


getUrl :: String -> IO Text
getUrl url = do
  manager <- HTTP.getGlobalManager
  request <- HTTP.parseRequest url
  putStrLn $ "Downloading " ++ url
  response <- HTTP.httpLbs request manager
  putStrLn $ "Got status " ++ (show . HTTP.statusCode . HTTP.responseStatus $ response)
  return . T.decodeUtf8 . LBS.toStrict . HTTP.responseBody $ response

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  entries <- listDirectory path
  (files, dirs) <- partitionM doesFileExist ((path </>) <$> entries)
  files' <- mapM getFiles dirs
  return (files ++ concat files')


partitionM :: Applicative f => (a -> f Bool) -> [a] -> f ([a], [a])
partitionM p = foldr f (pure ([], [])) where
  f x xs = g x <$> p x <*> xs
  g x b (l, r) = if b then (x : l, r) else (l, x : r)

index :: [a] -> Int -> Maybe a
index xs i = case drop i xs of
  [] -> Nothing
  (x:_) -> Just x
