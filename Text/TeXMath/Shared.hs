{-
Copyright (C) 2014 Matthew Pickering <matthewtpickering@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
module Text.TeXMath.Shared 
  ( getMMLType
  , getTextType
  , getSpaceCommand
  , getLaTeXTextCommand
  , getScalerCommand
  , getScalerValue
  , getDiacriticalCommand
  , getDiacriticalCons) where


import Text.TeXMath.Types
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative  
import Debug.Trace

getMMLType :: TextType -> String
getMMLType t = fromMaybe "normal" (fst <$> M.lookup t (M.fromList types))

getLaTeXTextCommand :: TextType -> String
getLaTeXTextCommand t = fromMaybe "\\mathrm" (snd <$> M.lookup t (M.fromList types))

getTextType :: String -> TextType
getTextType s = fromMaybe TextNormal (M.lookup s revTypes)
  where
    revTypes = M.fromList (map (\(k,v) -> (fst v,k)) types)

getSpaceCommand :: String -> String 
getSpaceCommand width = snd $ fromMaybe (M.findMax spaceMap) (M.lookupGE (readSpace width) spaceMap)
  where 
    spaceMap = M.fromList (map (\(k, ESpace s) -> (readSpace s, k)) spaceCommands)
    readSpace :: String -> Float
    readSpace s = maybe 0  fst $ listToMaybe $ reads s 

getScalerCommand :: String -> Maybe String
getScalerCommand width = (M.lookup width scalerMap)
  where
    scalerMap = M.fromList (reverseKeys scalers)

getScalerValue :: String -> Maybe String 
getScalerValue command = M.lookup command scalerMap
  where 
    scalerMap = M.fromList scalers

-- Not sure this behavoir is correct
getDiacriticalCons :: String -> Maybe (Exp -> Exp)
getDiacriticalCons command = 
    f <$> M.lookup command diaMap
  where
    diaMap = M.fromList (reverseKeys diacriticals)
    f s e = (if s `elem` under then EUnder else EOver) e (ESymbol Accent s)

getDiacriticalCommand  :: String -> Maybe String
getDiacriticalCommand symbol = M.lookup symbol diaMap
  where
    diaMap = M.fromList diacriticals


reverseKeys :: [(a, b)] -> [(b, a)]
reverseKeys = map (\(k,v) -> (v, k)) 

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


scalers :: [(String, String)]
scalers = 
          [ ("\\bigg", "2.2")
          , ("\\Bigg", "2.9")
          , ("\\big", "1.2")
          , ("\\Big", "1.6")
          , ("\\biggr", "2.2")
          , ("\\Biggr", "2.9")
          , ("\\bigr", "1.2")
          , ("\\Bigr", "1.6")
          , ("\\biggl", "2.2")
          , ("\\Biggl", "2.9")
          , ("\\bigl", "1.2")]

-- Accents which go under the character
under :: [String]
under = ["\xFE38", "\x23B5", "\x00AF"]


diacriticals :: [(String, String)]
diacriticals = 
               [ ("\x00B4", "\\acute")
               , (("\x0060", "\\grave"))
               , (("\x02D8", "\\breve"))
               , (("\x02C7", "\\check"))
               , (("\x307", "\\dot"))
               , (("\x308", "\\ddot"))
               , (("\x00B0", "\\mathring"))
               , (("\x20D7", "\\vec"))
               , (("\x20D7", "\\overrightarrow"))
               , (("\x20D6", "\\overleftarrow"))
               , (("\x005E", "\\hat"))
               , (("\x0302", "\\widehat"))
               , (("\x02C6", "\\widehat"))
               , (("\x0303", "\\tilde"))
               , (("\x02DC", "\\widetilde"))
               , (("\x203E", "\\bar"))
               , (("\xFE37", "\\overbrace"))
               , (("\x23DE", "\\overbrace"))
               , (("\x23B4", "\\overbracket"))
               , (("\x00AF", "\\overline"))
               , (("\xFE38", "\\underbrace"))
               , (("\x23DF", "\\underbrace"))
               , (("\x23B5", "\\underbracket"))
               , (("\x0332", "\\underline"))
               , (("\x0333", "\\underbar"))
               ]

