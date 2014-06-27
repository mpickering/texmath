module Text.TeXMath.Shared 
  (getMMLType, getTextType, getSpaceCommand, getLaTeXTextCommand) where


import Text.TeXMath.Types
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative  

getMMLType :: TextType -> String
getMMLType t = fromMaybe "normal" (fst <$> M.lookup t (M.fromList types))

getLaTeXTextCommand :: TextType -> String
getLaTeXTextCommand t = fromMaybe "\\mathrm" (snd <$> M.lookup t (M.fromList types))


getTextType :: String -> TextType
getTextType s = fromMaybe TextNormal (M.lookup s revTypes)
  where
    revTypes = M.fromList (map (\(k,v) -> (fst v,k)) types)

getSpaceCommand :: String -> String 
getSpaceCommand width = snd $ fromMaybe (M.findMax spaceMap) (M.lookupGE width spaceMap)
  where 
    spaceMap = M.fromList (map (\(k, ESpace s) -> (s, k)) spaceCommands)

spaceCommands :: [(String, Exp)]
spaceCommands = 
           [ ("\\!", ESpace "-0.167em")
           , (""   , ESpace "0.0em")
           , ("\\,", ESpace "0.167em")
           , ("\\>", ESpace "0.222em")
           , ("\\:", ESpace "0.222em")
           , ("\\;", ESpace "0.278em")
           , ("~", ESpace "0.333em")
           , ("\\quad", ESpace "1.0em")
           , ("\\qquad", ESpace "2.0em")]


--TextType to (MathML, LaTeX)
types :: [(TextType, (String, String))]
types = 
  [ ( TextNormal       , ("normal", "\\mathrm"))
  , ( TextBold         , ("bold", "\\mathbf"))
  , ( TextItalic       , ("italic","\\mathit"))
  , ( TextMonospace    , ("monospace","\\mathtt"))
  , ( TextSansSerif    , ("sans-serif","\\mathsf"))
  , ( TextDoubleStruck , ("double-struck","\\mathbb"))
  , ( TextScript       , ("script","\\mathcal"))
  , ( TextFraktur      , ("fraktur","\\mathfrak"))
  , ( TextBoldItalic          , ("bold-italic","\\mathbfit"))
  , ( TextBoldSansSerif       , ("bold-sans-serif","\\mathbfsfup"))
  , ( TextBoldSansSerifItalic , ("sans-serif-bold-italic","\\mathbfsfit"))
  , ( TextBoldScript          , ("bold-script","\\mathbfscr"))
  , ( TextBoldFraktur         , ("bold-fraktur","\\mathbffrak"))
  , ( TextSansSerifItalic     , ("sans-serif-italic","\\mathsfit")) ]

