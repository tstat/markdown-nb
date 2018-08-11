{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (rem, all, (**))
import Clay
import qualified Clay.Media as Media
import Data.Monoid
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main =
  T.writeFile "../html/app.css"
  $ renderWith pretty [] markdownNbStyles

maxSize :: Css
maxSize = do
  height (pct 100)
  width (pct 100)

reset :: Css
reset = star ? do
  fontSize (pct 100)
  "margin" -: "0"
  "padding" -: "0"
  "border" -: "0"
  "font" -: "inherit"
  "vertical-align" -: "baseline"

markdownNbStyles :: Css
markdownNbStyles = do
  reset
  typography
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
    queryOnly all [Media.maxWidth (px 1450)] $ do
      maxWidth (pct 96)
    maxWidth (px 1400)
    margin (px 20) auto (px 0) auto
    height (pct 100)
    display flex

wrapStyles :: Css
wrapStyles = ".wrap" ? do
  maxSize
  header ? do
    height (px 45)
    borderBottom solid (px 3) black
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    h4 ? do
      paddingLeft (px 50)
      marginBottom (px 0)
      marginTop (px 10)
      paddingBottom (px 0)

editorStyles :: Css
editorStyles = ".editor" ? do
  fontFamily [] [monospace]
  fontSize (rem $ typeScale ^^ (-1))
  flexGrow 0
  flexShrink 0
  flexBasis (pct 39)
  marginRight (px 20)
  textarea ? do
    maxSize
    backgroundColor codeBg
    boxSizing borderBox
    border solid (px 10) codeBg
    "resize" -: "none"

rendererStyles :: Css
rendererStyles = ".renderer" ? do
  flexGrow 1

typography :: Css
typography = do
  html ? do
    fontFamily [] [sansSerif]
  p ? do
    lineHeight (rem 1.2)
    marginTop (rem 1.2)
    marginBottom nil
  ul <> ol ? do
    marginTop (rem 1.2)
    marginBottom (rem 1.2)
  foldr1 (<>) ((** li) <$> [ul, ol]) ? do
    lineHeight (rem 1.2)
  foldr1 (<>) [x ** x | x <- [ul, ol]] ? do
    marginTop nil
    marginBottom nil
  blockquote ? do
    lineHeight (rem 1.2)
    marginTop (rem 1.2)
    marginBottom (rem 1.2)
  ul |> li ? do
    marginLeft (px 20)
  p ? do
    lineHeight (rem 1.2)
    marginTop (rem 1.2)
  hr ? do
    backgroundColor black
    height (px 2)

  pre ? do
    padding (rem 1) (rem 1) (rem 1) (rem 1)
    backgroundColor "#333333"
    fontColor "#f1f1f1"
  code ? do
    fontSize (rem $ typeScale ^ 0)
    fontFamily [] [monospace]

  mapM_ (uncurry setFontSize) [(h1,4), (h2,3), (h3,2), (h4,1), (h5,0), (h6,0)]

  where
    baseSize :: Size LengthUnit
    baseSize = (px 14)


    setFontSize :: Selector -> Int -> Css
    setFontSize sel i = sel ? do
      let s = typeScale ^ i
      fontSize (rem s)
      lineHeight (rem $ 1.2 * s)

typeScale :: Double
typeScale = 1.414

codeBg :: Color
codeBg = "#f1f1f1"
