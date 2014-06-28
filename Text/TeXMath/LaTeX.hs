module Text.TeXMath.LaTeX (writeLaTeX) where 

import Text.TeXMath.Types
import Data.List (intersperse)
import Text.TeXMath.UnicodeToLaTeX
import qualified Text.TeXMath.Shared as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace

writeLaTeX :: DisplayType -> [Exp] -> String
writeLaTeX t es = concatMap writeExp es

writeExp :: Exp -> String
writeExp (ENumber s) = getLaTeX s
writeExp (EGrouped es) = concatMap writeExp es
writeExp (EDelimited open close es) = "\\left"++getLaTeX open ++ concat (intersperse "," (map writeExp es)) ++ "\\right" ++ getLaTeX close
writeExp (EIdentifier s) = inBraces $ getLaTeX s
writeExp o@(EMathOperator s) = fromMaybe (getLaTeX s) (findOperator o)
writeExp (ESymbol _ s) = getLaTeX s
writeExp (ESpace width) = S.getSpaceCommand width 
writeExp (EBinary s e1 e2) = s ++ (evalInBraces e1) ++ (evalInBraces e2)
writeExp (ESub b e1) = writeExp b ++ "_" ++ (evalInBraces e1) 
writeExp (ESuper b e1) = writeExp b ++ "^" ++ (evalInBraces e1)  
writeExp (ESubsup b e1 e2) = writeExp b ++ "_" ++ (evalInBraces e1) ++ "^" ++ (evalInBraces e2)  
writeExp (EOver b e1) = writeExp b ++ "^_" ++ (evalInBraces e1) 
writeExp (EUnder b e1) = writeExp b ++ "_" ++ (evalInBraces e1)  
writeExp (EUnderover b e1 e2) = writeExp b ++ "_" ++ (evalInBraces e1) ++ "^" ++ (evalInBraces e2)
writeExp (EUp b e1) = writeExp b ++ "^" ++ (evalInBraces e1) 
writeExp (EDown b e1) = writeExp b ++ "_" ++ (evalInBraces e1) 
writeExp (EDownup b e1 e2) = writeExp b ++ "_" ++ (evalInBraces e1) ++ "^" ++ evalInBraces e2
writeExp (EUnary s e) = s ++ evalInBraces e
writeExp (EScaled size e) = fromMaybe "" (S.getScalerCommand size) ++ evalInBraces e
writeExp (EStretchy (ESymbol Open e)) = "\\left" ++ getLaTeX e
writeExp (EStretchy (ESymbol Close e)) = "\\right" ++ getLaTeX e
writeExp (EStretchy e) = writeExp e
writeExp (EArray aligns rows) = table aligns rows
writeExp (EText ttype s) = S.getLaTeXTextCommand ttype ++ inBraces s

evalInBraces :: Exp -> String
evalInBraces = inBraces . writeExp

inBraces :: String -> String
inBraces s = "{" ++ s ++ "}"

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


-- Operator Table

findOperator :: Exp -> Maybe String
findOperator = flip lookup operators

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

