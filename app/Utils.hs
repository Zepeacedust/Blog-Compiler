{-# LANGUAGE OverloadedStrings #-}
module Utils where

import DataTypes

import qualified Data.Text as T
import           Text.Pandoc
import           Data.Time
import           System.FilePath (replaceExtension, takeDirectory)

errorWrap :: a -> Maybe b -> Either a b
errorWrap fallback Nothing = Left fallback
errorWrap _fallback (Just val) = Right val

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _ ) = meta

getMetaThing :: T.Text -> Pandoc -> Maybe MetaValue
getMetaThing thing content = lookupMeta thing (getMeta content)

clobberInline :: [Inline] -> T.Text
clobberInline = T.concat . map (\s -> case s of
                         Str t -> t
                         Space -> " ")

getTitle :: String -> Pandoc -> Either String T.Text
getTitle filename cont = do
  titleBlock <- errorWrap (filename ++ " Fatal: Title section not found.") $ getMetaThing "title" cont
  case titleBlock of
    MetaInlines inline -> Right (clobberInline inline)
    _otherwise -> Left (filename ++ " FATAL: Wonky title. Looks like " ++ show titleBlock)

getDate :: String -> Pandoc -> Either String UTCTime
getDate filename cont = do
  result <- errorWrap (filename ++ " FATAL: Date not found.") $ getMetaThing "date" cont
  let MetaInlines[Str dateString] = result -- fuck ugly but it works
  errorWrap (filename ++ " FATAL: Date improperly formatted") $ parseTimeM True defaultTimeLocale "%-d/%-m/%Y" (T.unpack dateString)

getDesiredTemplate :: String -> Pandoc -> Either String (Maybe T.Text)
getDesiredTemplate filename cont = do
  case getMetaThing "template" cont of
    Just something -> case something of
                        MetaInlines [Str title] -> Right (Just title)
                        _otherwise -> Left (filename ++ " FATAL: Malformed template tag")
    Nothing -> Right Nothing


getTags :: String -> Pandoc -> Either String [T.Text]
getTags fileName cont = do
  case getMetaThing "tags" cont of
    Nothing -> return []
    Just tags -> do
      rawTagList <- case tags of
        MetaList metaTags -> Right metaTags
        _otherwise -> Left (fileName ++ " FATAL: Malformed Tags")
      return $ map (\(MetaInlines [Str tag]) -> tag) rawTagList -- This is damn silly

noteLefts :: [Either String b] -> IO [b]
noteLefts [] = return []
noteLefts (Left a:xs) = do
  putStrLn a
  noteLefts xs
noteLefts (Right a:xs) = do
  rest <- noteLefts xs
  return $ a : rest


isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix []     []     = True
isPrefix []     x      = True
isPrefix x      []     = False
isPrefix (x:xs) (b:bs) = if x == b then isPrefix xs bs else False


replacePrefix :: (Eq a, Show a) => [a] -> [a] -> [a] -> [a]
replacePrefix old new op = if isPrefix old op then new ++ drop (length old) op else error (show old ++ " Is not prefix of " ++ show op) where

findDestination :: Config -> String -> String
findDestination conf source = replaceExtension (replacePrefix src dst source) "html" where
  src = sourceFolder conf
  dst = destinationFolder conf
