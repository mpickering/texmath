module Text.TeXMath.Shared 
  (getMMLType, getTextType) where


import Text.TeXMath.Types
import qualified Data.Map as M
import Data.Maybe

getMMLType :: TextType -> String
getMMLType t = fromMaybe "normal" (M.lookup t (M.fromList types))


getTextType :: String -> TextType
getTextType s = fromMaybe TextNormal (M.lookup s revTypes)
  where
    revTypes = M.fromList (map (\(k,v) -> (v,k)) types)

types :: [(TextType, String)]
types = 
  [ ( TextNormal       , "normal")
  , ( TextBold         , "bold")
  , ( TextItalic       , "italic")
  , ( TextMonospace    , "monospace")
  , ( TextSansSerif    , "sans-serif")
  , ( TextDoubleStruck , "double-struck")
  , ( TextScript       , "script")
  , ( TextFraktur      , "fraktur")
  , ( TextBoldItalic          , "bold-italic")
  , ( TextBoldSansSerif       , "bold-sans-serif")
  , ( TextBoldSansSerifItalic , "sans-serif-bold-italic")
  , ( TextBoldScript          , "bold-script")
  , ( TextBoldFraktur         , "bold-fraktur")
  , ( TextSansSerifItalic     , "sans-serif-italic") ]
