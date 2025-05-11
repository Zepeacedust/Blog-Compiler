module Writing where

import Utils
import DataTypes
import Text.Pandoc
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import           System.FilePath (replaceExtension, takeDirectory)
import qualified Data.Map.Strict as M


writeMakingFile :: String -> T.Text -> IO()
writeMakingFile name content = do
  D.createDirectoryIfMissing True . takeDirectory $ name
  TIO.writeFile name content

pasteFile :: Template T.Text -> Pandoc -> String -> IO ()
pasteFile template content file = do
  result <- runIO $ do
    writeHtml5String def {writerTemplate = Just template} content
  postText <- handleError result
  writeMakingFile file postText

dumpPost :: Config -> (Maybe T.Text -> Either String (Template T.Text)) -> BlogPost -> IO ()
dumpPost conf templateSelector post = do
  let template = templateSelector $ desiredTemplate post
  case template of
    Left e -> error e
    Right temp -> pasteFile temp (postContent post) (findDestination conf  $ fileName post)

dumpPosts :: Config -> (Maybe T.Text-> Either String (Template T.Text))-> [BlogPost] -> IO ()
dumpPosts conf templateSelector = mapM_ (dumpPost conf templateSelector)

createPostStub :: Config ->  BlogPost -> MetaValue
createPostStub conf post = MetaMap(M.union original path) where
  original = unMeta.getMeta $  postContent post
  path     = M.singleton (T.pack "path") (MetaString (T.pack.findDestination conf $ fileName post ))

createIndexFromPosts :: Config -> Template T.Text -> [BlogPost] -> String -> IO ()
createIndexFromPosts conf temp posts fileName = do
  let postMetas = map (createPostStub conf) (L.sortOn postDate posts)
      newMeta = Meta (M.singleton (T.pack "post") (MetaList postMetas))
  pasteFile temp (Pandoc newMeta []) fileName
  return ()

makeIndex :: Config -> (Maybe T.Text -> Either String (Template T.Text)) -> [BlogPost] -> IndexDescriptor -> IO ()
makeIndex conf templateSelector posts (IndexDescriptor {indexFolder = folder, indexTemplate = reqTemplate}) = do
  let
    containedPosts = filter (isPrefix folder . fileName) posts
    template = case templateSelector $ Just reqTemplate of
      Left e -> error e
      Right temp -> temp
    tags = L.nub . concat $ map postTags posts
  mapM_ (\tag ->
           createIndexFromPosts
             conf
             template
             (filter (elem tag . postTags ) containedPosts)
             (findDestination conf $ folder ++ "/index_"++T.unpack tag)
        ) tags
  createIndexFromPosts conf template containedPosts (findDestination conf $ folder++"/index")

makeIndices :: Config -> (Maybe T.Text -> Either String (Template T.Text)) -> [BlogPost] -> IO ()
makeIndices conf templateSelector posts = mapM_ (makeIndex conf templateSelector posts) $ indices conf
