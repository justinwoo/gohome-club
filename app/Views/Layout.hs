{-# LANGUAGE OverloadedStrings #-}

module Views.Layout (layout) where

import           Prelude                     hiding (head)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (style, title)

layout :: Html -> Html -> Html
layout titleContent bodyContent = docTypeHtml $ do
  head $ do
    title titleContent
    meta ! charset "utf-8"
    style "BODY { font: 8pt/12pt verdana } H1 { font: 13pt/15pt verdana } H2 { font: 8pt/12pt verdana } A:link { color: red } A:visited { color: maroon }"
  body . table . tr $ td bodyContent

