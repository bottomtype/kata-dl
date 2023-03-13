{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Kata.Base where

import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Control.Monad.Catch
import Control.Monad
import System.FilePath

data KataException = ParseException String | ScrapeException String deriving Show
instance Exception KataException



-------------- Plaintext Tree

data Formatted
  = Newline
  | Plaintext Text
  | Paragraph [Formatted]
  | Collect [Formatted]
  deriving Show



renderWith :: (Text -> Text) -> Formatted -> Text
renderWith f = go where
  go Newline = "\n"
  go (Plaintext s) = f s
  go (Paragraph fs) = gos fs <> "\n\n"
  go (Collect fs) = gos fs

  gos = T.concat . fmap go

render :: Formatted -> Text
render = renderWith id



--------------- Chapter Info

data ChapterInfo = KC { fullUrl :: Text , fullName :: Text , arcName :: Text , arcNumber :: Int , chapter :: Int } deriving Show

parseKataUrl :: MonadThrow m => Text -> m ChapterInfo
parseKataUrl url = let
  fullName = correctArc1Name . last . T.splitOn "/" . dropSuffix "/" $ url
  (arcName, numStr) = T.break isDigit fullName
  (arcStr, chapStr) = T.break (== '-') numStr
  in do
    arc <- readNumber "arc" arcStr
    chap <- readNumber "chapter" (T.tail chapStr)
    return $ KC url fullName (T.init arcName) arc chap
  where
    readNumber ty s = either
      (const . throwM . ParseException $ "Couldn't parse " ++ ty ++ " number '" ++ T.unpack s ++ "' in url '" ++ T.unpack url ++ "'")
      (return . fst)
      (T.decimal s)

correctArc1Name :: Text -> Text
correctArc1Name s = case T.stripPrefix "mindcorrelating" s of
  Just rest -> "mind-correlating" <> rest
  Nothing -> s



--------------- Name Format

data FileType = Plain | Raw deriving Show
data Padding = NoPad | YesPad deriving Show

data NameComponent = Txt String | ArcName | ArcNumber Padding | ChapterNumber Padding | FileType deriving Show

type NameFmt = [NameComponent]


directory :: FileType -> FilePath
directory Plain = "plaintext"
directory Raw = "html"

extension :: FileType -> String
extension Plain = "txt"
extension Raw = "html"

showIntPadding :: Padding -> Int -> String
showIntPadding NoPad = show
showIntPadding YesPad = justifyRight 2 '0' . show

renderNameComponent :: FileType -> ChapterInfo -> NameComponent -> String
renderNameComponent ty inf (Txt t) = t
renderNameComponent ty inf ArcName = T.unpack inf.arcName
renderNameComponent ty inf (ArcNumber p) = showIntPadding p inf.arcNumber
renderNameComponent ty inf (ChapterNumber p) = showIntPadding p inf.chapter
renderNameComponent ty inf FileType = directory ty

renderName :: FileType -> ChapterInfo -> NameFmt -> String
renderName ty inf = foldr ((<>) . renderNameComponent ty inf) ""

filename :: NameFmt -> ChapterInfo -> FileType -> FilePath
filename fmt inf ty = renderName ty inf fmt <.> extension ty

parseNameComponent :: MonadThrow m => Char -> m NameComponent
parseNameComponent 'n' = pure ArcName
parseNameComponent 'A' = pure (ArcNumber YesPad)
parseNameComponent 'a' = pure (ArcNumber NoPad)
parseNameComponent 'C' = pure (ChapterNumber YesPad)
parseNameComponent 'c' = pure (ChapterNumber NoPad)
parseNameComponent 't' = pure FileType
parseNameComponent c = throwM $ ParseException $ "Failed to parse specifier %" ++ [c]

parseFormat :: MonadThrow m => String -> m NameFmt
parseFormat st = when (null st) (throwM $ ParseException "FORMAT may not be empty") >> go st where
  go t = let (p, s) = break (== '%') t in
    case uncons (drop 1 s) of
      Nothing -> return [Txt p]
      Just (c, s') -> do
        comp <- parseNameComponent c
        rest <- go s'
        return $ Txt p : comp : rest



--------------- Parameters

data Params = Params
  { dontExtract :: Bool
  , localRaws :: Maybe FilePath
  , discardRaws :: Bool
  , fileFormat :: NameFmt
  } deriving (Show)


fmtSpecs :: [(String, String)]
fmtSpecs =
  [ ("%n", "Arc name")
  , ("%a", "Arc number")
  , ("%c", "Chapter number")
  , ("%A", "Arc number with leading zeroes")
  , ("%C", "Chapter number with leading zeroes")
  , ("%t", "Content type (i.e. html or plaintext)")
  ]

defaultFmtString :: String
defaultFmtString = "chapters" ++ [pathSeparator] ++ "%t" ++ [pathSeparator] ++ "%a-%n" ++ [pathSeparator] ++ "%n-%a-%c"

defaultFormat :: NameFmt
defaultFormat = [Txt ("chapters" ++ [pathSeparator]), FileType, Txt [pathSeparator], ArcNumber NoPad, Txt "-", ArcName, Txt [pathSeparator], ArcName, Txt "-", ArcNumber NoPad, Txt "-", ChapterNumber NoPad]



--------------- Utils

dropSuffix :: Text -> Text -> Text
dropSuffix s t = case T.stripSuffix s t of
  Nothing -> t
  (Just t') -> t'

justifyRight :: Int -> Char -> String -> String
justifyRight l c s
  | l > length s = replicate (l - length s) c ++ s
  | otherwise = s
