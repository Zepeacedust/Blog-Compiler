module Main where

import DataTypes
import Reading
import Writing

import System.Environment

import Text.Pandoc
import qualified Data.Text as T

main :: IO ()
main = do
  print "Hey what the fuck"
  args <- getArgs

  config <- loadConfig $ head args

  templateSelector <- getTemplateSelector config

  blogPosts <- readPosts config

  dumpPosts config templateSelector blogPosts

  makeIndices config templateSelector blogPosts
