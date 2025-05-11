{-# LANGUAGE OverloadedStrings #-}


module Reading where

import Utils
import DataTypes
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import           Data.Time
import           Text.Pandoc

loadConfig :: String -> IO Config
loadConfig path = do
  file <- B.readFile path
  case decode file of
    Just config -> return config
    _errorInParsing -> error "Config file badly formed"

getFilesRecursive :: FilePath -> IO [String]
getFilesRecursive root = do
  isFile <- D.doesFileExist root
  isFolder <- D.doesDirectoryExist root
  case (isFile, isFolder) of
    (True, False) -> do
      return [root]
    (False, True) -> do
      relativeFiles <- D.listDirectory root
      let absFiles = map (\f -> root++"/"++f) relativeFiles
      chunks <- mapM getFilesRecursive absFiles
      return.concat $ chunks
    (True, True) -> error (root ++ " is both File and Directory... What?")
    (False,False) -> error (root ++ " exists but does not exist... What?")

makeTemplate ::Config ->  String -> IO (T.Text , Template T.Text)
makeTemplate conf templateName = do
  Right source <- runIO (getTemplate templateName)
  Right template <- compileTemplate "" source
  return (T.pack (replacePrefix (templateFolder conf ++ "/") "" templateName), template)

getTemplateSelector :: Config -> IO (Maybe T.Text -> Either String (Template T.Text))
getTemplateSelector config = do
  templateFiles <- getFilesRecursive $ templateFolder config
  templates <- mapM (makeTemplate config) templateFiles
  let defTemp = case lookup (defaultTemplate config) templates of
                  Nothing -> error ("Default template not found, templates are " ++ show templates ++ show templateFiles)
                  Just template -> template
  return $ templateServer templates defTemp

templateServer :: [(T.Text, Template T.Text)] -> Template T.Text -> Maybe T.Text -> Either String (Template T.Text)
templateServer _templates def Nothing = Right def
templateServer templates _def (Just request) =
  errorWrap ("Fatal: template " ++ T.unpack request ++ "not found, templates are" ++ show templates) $ lookup request templates

processFile :: String -> IO Pandoc
processFile fileName = do
  text <- TIO.readFile fileName
  result <- runIO $ do
    readMarkdown def{
      readerExtensions = extensionsFromList [Ext_yaml_metadata_block,
                                             Ext_backtick_code_blocks,
                                             Ext_grid_tables,
                                             Ext_tex_math_dollars],
      readerStandalone = True} text
  handleError $ result

makePost :: String -> IO (Either String BlogPost)
makePost name = do
  content <- processFile name
  return $ do
    title <- getTitle name content
    tags <- getTags name content
    date <- getDate name content
    templ <- getDesiredTemplate name content
    return $ BlogPost {
      fileName = name,
      postTitle = title,
      postTags = tags,
      postDate = date,
      postContent = content,
      desiredTemplate = templ
      }
readPosts :: Config -> IO [BlogPost]
readPosts config = do
  postFiles <- getFilesRecursive $ sourceFolder config
  posts <- mapM makePost postFiles
  noteLefts posts
