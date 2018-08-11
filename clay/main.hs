{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (rem)
import Clay
import Data.Monoid

main :: IO ()
main = putCss myStyles

maxSize :: Css
maxSize = do
  height (pct 100)
  width (pct 100)

myStyles :: Css
myStyles = do
  baseStyles
  wrapStyles
  editorStyles
  rendererStyles

baseStyles :: Css
baseStyles = do
  html <> body ? do
    maxSize
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
  ".main" ? do
    maxWidth (px 1400)
    margin (px 20) auto (px 0) auto
    height (pct 100)
    display flex

wrapStyles :: Css
wrapStyles = ".wrap" ? do
  maxSize
  header ? do
    height (px 50)
    borderBottom solid (px 3) black
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    h4 ? do
      paddingLeft (px 50)
      marginBottom (px 0)
      paddingBottom (px 0)

editorStyles :: Css
editorStyles = ".editor" ? do
  flexGrow 0
  flexShrink 0
  flexBasis (pct 39)
  marginRight (pct 2)
  textarea ? do
    maxSize
    backgroundColor "#f1f1f1"
    boxSizing borderBox
    border solid (px 5) "#f1f1f1"
    "resize" -: "none"

rendererStyles :: Css
rendererStyles = ".renderer" ? do
  flexGrow 1
