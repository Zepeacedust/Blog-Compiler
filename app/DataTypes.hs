{-# LANGUAGE TemplateHaskell #-}


module DataTypes where

import qualified Data.Text as T
import Data.Time
import Text.Pandoc
import           Data.Aeson.TH

data IndexDescriptor = IndexDescriptor {
  indexFolder :: String,
  indexTemplate :: T.Text
  }
$(deriveJSON defaultOptions ''IndexDescriptor)

data Config = Config {
  sourceFolder :: String,
  destinationFolder :: String,
  templateFolder :: String,
  defaultTemplate :: T.Text,
  indices :: [IndexDescriptor]
  }
$(deriveJSON defaultOptions ''Config)

data BlogPost = BlogPost {
  fileName :: String,
  postTitle :: T.Text,
  postTags :: [T.Text],
  postDate :: UTCTime,
  postContent :: Pandoc,
  desiredTemplate :: Maybe T.Text
              }
