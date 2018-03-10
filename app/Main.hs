{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Diagrams.Prelude
import Diagrams.TwoD.Size
import Diagrams.Backend.Rasterific
import Web.Hastodon
import Dhall
import qualified Data.Text.Lazy as T

cfgFile = "~/.treebot"
filename = "/tmp/treebottree.png"

data HastodonConfig = HastodonConfig { clientId :: Text,
                       clientSecret :: Text,
                       username :: Text,
                       password :: Text,
                       server :: Text} deriving Generic

instance Interpret HastodonConfig

mkHastodonClientFromConfig :: HastodonConfig -> IO (Maybe HastodonClient)
mkHastodonClientFromConfig (HastodonConfig cId cSecret un pw serv) =
  mkHastodonClient (T.unpack cId) (T.unpack cSecret) (T.unpack un) (T.unpack pw) (T.unpack serv)

main :: IO ()

toot t = "Would you like a tree in this trying time?  It has "++show (weight t)++" nodes, descending "++show (depth t)++" nodes down."

main = do
  cfg <- input auto "~/.treebot"
  maybeClient <- mkHastodonClientFromConfig cfg
  case maybeClient of
    Just client -> do
      tree <- genTree
      renderRasterific filename (mkWidth 1200) (diagram tree)
      attRes <- postMediaFile client filename "A randomly generated tree."
      case attRes of
        Left _ -> do
          putStr "Failed to upload file"
        Right attachment -> do
          let treeId = attachmentId attachment
          result <- postStatusWithMediaIds client (toot tree) [treeId]
          return ()
          --print result
    Nothing -> do
      putStr "Failed to connect to Mastodon"
