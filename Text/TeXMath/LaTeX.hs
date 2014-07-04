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

module Text.TeXMath.LaTeX (fixTree, toLaTeX) where 

import Text.TeXMath.Types
import Data.List (intersperse)
import Text.TeXMath.UnicodeToLaTeX (getLaTeX, convertText, escapeLaTeX)
import Text.TeXMath.Unidecode (getASCII)
import qualified Text.TeXMath.Shared as S
import Data.Maybe (fromMaybe)
import Debug.Trace(traceShow, traceShowId)
import Data.Generics (everywhere, mkT)

toLaTeX :: [Exp] -> String
toLaTeX es = concatMap (writeExp . fixTree) es 

writeExp :: Exp -> String
writeExp (ENumber s) = getLaTeX s
writeExp (EGrouped es) = concatMap writeExp es
writeExp (EDelimited open close es) = 
  "\\left" ++
  getLaTeX open ++ 
  concatMap writeExp es ++ 
  "\\right" ++
  getLaTeX close
writeExp (EIdentifier s) = inBraces $ getLaTeX s
writeExp o@(EMathOperator s) = 
  fromMaybe ("\\operatorname" ++ (inBraces $ escapeSpace $ getLaTeX s)) (getOperator o)
writeExp (ESymbol _ s) = getLaTeX s
writeExp (ESpace width) = " " ++ S.getSpaceCommand width 
writeExp (EBinary s e1 e2) 
  | s `elem` square = s ++ (evalInSquare e1) ++ (evalInBraces e2)
  | otherwise = s ++ (evalInBraces e1) ++ (evalInBraces e2)
writeExp (ESub b e1) = under b e1 
writeExp (ESuper b e1) = over b e1  
writeExp (ESubsup b e1 e2) = underOver b e1 e2  
writeExp (EOver b e1) = 
  case b of
    (EMathOperator _) -> over b e1 
    _ -> "\\overset" ++ evalInBraces e1 ++ evalInBraces b
writeExp (EUnder b e1) = 
  case b of   
    (EMathOperator _) -> under b e1 
    _ -> "\\underset" ++ evalInBraces e1 ++ evalInBraces b
writeExp (EUnderover b e1 e2) = 
  case b of 
    (EMathOperator _) -> underOver b e1 e2
    _ -> writeExp $ EUnder (EOver b e2) e1
writeExp (EUp b e1) = over b e1
writeExp (EDown b e1) = under b e1 
writeExp (EDownup b e1 e2) = underOver b e1 e2
writeExp (EUnary s e) = s ++ evalInBraces e
writeExp (EScaled size e) = fromMaybe "" (S.getScalerCommand size) ++ evalInBraces e
writeExp (EStretchy (ESymbol Open e)) = let e' = getLaTeX e in 
                                            case e' of {"" -> ""; _ -> "\\left" ++  e' ++ " "}
writeExp (EStretchy (ESymbol Close e)) = let e' = getLaTeX e in
                                              case e' of {"" -> ""; _ -> "\\right" ++ e' ++ " "}
writeExp (EStretchy e) = writeExp e
writeExp (EArray aligns rows) = table aligns rows
writeExp (EText ttype s) = getLaTeXTextCommand ttype ++ inBraces (escapeSpace $ getLaTeX s)

escapeSpace :: String -> String 
escapeSpace = concatMap (\c -> if c == ' ' then "\\ " else [c]) 

square :: [String]
square = ["\\sqrt"]

table :: [Alignment] -> [ArrayLine] -> String
table as rows = "\\begin{array}" ++ inBraces columnAligns ++ "\n" ++ concatMap row rows ++ "\\end{array}"
  where 
    columnAligns = map alignmentToLetter as
    alignmentToLetter AlignLeft = 'l'
    alignmentToLetter AlignCenter = 'c'
    alignmentToLetter AlignRight = 'r'
    alignmentToLetter AlignDefault = 'c'

row :: ArrayLine -> String
row cells = (concat  (intersperse " & " (map cell cells))) ++ " \\\\\n"
  where 
    cell es = concatMap writeExp es

-- Utility 

-- Text commands availible in amsmath
formats :: [String]
formats = ["\\mathrm", "\\mathit", "\\mathsf", "\\mathtt", "\\mathfrak", "\\mathcal"]

alts :: [(String, String)]
alts = [ ("\\mathbfit", "\\mathbf"), ("\\mathbfsfup", "\\mathbf"), ("\\mathbfsfit", "\\mathbf")
       , ("\\mathbfscr", "\\mathcal"), ("\\mathbffrak", "\\mathfrak"), ("\\mathsfit", "\\mathsf")]

getLaTeXTextCommand :: TextType -> String
getLaTeXTextCommand t 
  | cmd `elem` formats = cmd
  | otherwise = fromMaybe "\\mathrm" (lookup cmd alts)
  where
    cmd = S.getLaTeXTextCommand t 
  


under :: Exp -> Exp -> String
under = bin "_"

over :: Exp -> Exp -> String
over = bin "^"

underOver :: Exp -> Exp -> Exp -> String
underOver b e1 e2 = bin "_" b e1 ++ "^" ++ evalInBraces e2

bin :: String -> Exp -> Exp -> String
bin s b e = evalInBraces b ++ s ++ evalInBraces e

evalInBraces :: Exp -> String
evalInBraces = inBraces  . writeExp

inBraces :: String -> String
inBraces = around "{" "}"

around :: String -> String -> String -> String
around o c s = o ++ s ++ c

evalInSquare :: Exp -> String
evalInSquare = around "[" "]" . writeExp 

removeAccentStretch :: Exp -> Exp
removeAccentStretch (EStretchy e@(ESymbol Accent _)) = e
removeAccentStretch x = x

reorderDiacritical' :: String -> Exp -> Exp -> Exp 
reorderDiacritical' def b e@(ESymbol Accent a) = 
  case S.getDiacriticalCommand a of
    Just accentCmd -> EUnary accentCmd b
    Nothing -> EBinary def e b
reorderDiacritical' _ _ _ = error "Must be called with Accent"

reorderDiacritical :: Exp -> Exp
reorderDiacritical (EOver b e@(ESymbol Accent _)) = reorderDiacritical' "\\overset" b e
reorderDiacritical (EUnder b e@(ESymbol Accent _)) = reorderDiacritical' "\\underset" b e
reorderDiacritical (EUnderover b e@(ESymbol Accent _) e1) = reorderDiacritical' "\\underset" (EOver b e1) e
reorderDiacritical (EUnderover b e1 e@(ESymbol Accent _)) = reorderDiacritical' "\\overset" (EUnder b e1) e
reorderDiacritical x = x


matchStretch' :: [Exp] -> Int
matchStretch'  [] = 0
matchStretch' (a@(EStretchy (ESymbol Open s)): xs) = let s' = getLaTeX s in 
                                                      case s' of {"" -> 0; _ -> 1} 
                                                        + (matchStretch' xs)
matchStretch' (b@(EStretchy (ESymbol Close s)): xs) = let s' = getLaTeX s in
                                                       case s' of {"" -> 0; _ -> (-1)} 
                                                        + (matchStretch' xs)
matchStretch' (_:xs) = matchStretch' xs

matchStretch :: [Exp] -> [Exp] 
matchStretch es 
  | n < 0 = (replicate (0 - n) $ EStretchy (ESymbol Open ".")) ++ es
  | n > 0 = es ++ (replicate n $ EStretchy (ESymbol Close "."))
  | otherwise = es
  where 
    n = matchStretch' es

ms :: Exp -> Exp
ms (EGrouped xs) = EGrouped (matchStretch xs)
ms (EDelimited o c xs) = EDelimited o c (matchStretch xs) 
ms (EArray as rs) = EArray as (map (map matchStretch) rs)
ms x = x

fixTree :: Exp -> Exp
fixTree = everywhere 
            ( mkT ms  
            . mkT reorderDiacritical 
            . mkT removeAccentStretch )

-- Operator Table

getOperator :: Exp -> Maybe String
getOperator = flip lookup operators

operators :: [(Exp, String)]
operators = 
           [ (EMathOperator "arccos", "\\arccos")
           , (EMathOperator "arcsin", "\\arcsin")
           , (EMathOperator "arctan", "\\arctan")
           , (EMathOperator "arg", "\\arg")
           , (EMathOperator "cos", "\\cos")
           , (EMathOperator "cosh", "\\cosh")
           , (EMathOperator "cot", "\\cot")
           , (EMathOperator "coth", "\\coth")
           , (EMathOperator "csc", "\\csc")
           , (EMathOperator "deg", "\\deg")
           , (EMathOperator "det", "\\det")
           , (EMathOperator "dim", "\\dim")
           , (EMathOperator "exp", "\\exp")
           , (EMathOperator "gcd", "\\gcd")
           , (EMathOperator "hom", "\\hom")
           , (EMathOperator "inf", "\\inf")
           , (EMathOperator "ker", "\\ker")
           , (EMathOperator "lg", "\\lg")

           , (EMathOperator "lim", "\\lim")
           , (EMathOperator "liminf", "\\liminf")
           , (EMathOperator "limsup", "\\limsup")
           , (EMathOperator "ln", "\\ln")
           , (EMathOperator "log", "\\log")
           , (EMathOperator "max", "\\max")
           , (EMathOperator "min", "\\min")
           , (EMathOperator "Pr", "\\Pr")
           , (EMathOperator "sec", "\\sec")
           , (EMathOperator "sin", "\\sin")
           , (EMathOperator "sinh", "\\sinh")
           , (EMathOperator "sup", "\\sup")
           , (EMathOperator "tan", "\\tan")
           , (EMathOperator "tanh", "\\tanh") ]

