{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
module Servant.XML
  ( ToNode(..)
  , XML
  , Text.Hamlet.XML.xml
  , iso8601 )
  where

import Text.XML
import ClassyPrelude
import Text.Hamlet.XML
import Servant
import Network.HTTP.Media.MediaType

data XML

instance (ToNode a) => MimeRender XML a where
  mimeRender _ a =
    let [NodeElement root] = toNode a
    in renderLBS def (Document (Prologue [] Nothing []) root [])

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

iso8601 :: UTCTime -> Text
iso8601 = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

class ToNode a where
  toNode :: a -> [Node]

instance (ToNode a) => ToNode [a] where
  toNode = concatMap toNode
