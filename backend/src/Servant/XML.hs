{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeApplications #-}
module Servant.XML
  ( ToNode(..)
  , XML
  , OPDS
  , Text.Hamlet.XML.xml
  , iso8601 )
  where

import Text.XML
import ClassyPrelude
import Text.Hamlet.XML
import Servant
import Network.HTTP.Media.MediaType

data XML

data OPDS

instance (ToNode a) => MimeRender XML a where
  mimeRender _ a =
    let [NodeElement root] = toNode a
    in renderLBS def (Document (Prologue [] Nothing []) root [])

instance (ToNode a) => MimeRender OPDS a where
  mimeRender _ a = mimeRender (Proxy @XML) a

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance Accept OPDS where
  contentType _ = "application" // "atom+xml" /: ("charset", "utf-8") /: ("profile", "opds-catalog")

iso8601 :: UTCTime -> Text
iso8601 = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

class ToNode a where
  toNode :: a -> [Node]

instance (ToNode a) => ToNode [a] where
  toNode = concatMap toNode
