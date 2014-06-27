module Text.TeXMath.LaTeX (writeLaTeX) where 

import Text.TeXMath.Types
import Data.List (intersperse)
import Text.TeXMath.UnicodeToLaTeX
import Text.TeXMath.Shared
import qualified Data.Map as M

writeLaTeX :: DisplayType -> [Exp] -> String
writeLaTeX t es = concatMap writeExp es

writeExp :: Exp -> String
writeExp (ENumber s) = getLaTeX s
writeExp (EGrouped es) = concatMap writeExp es
writeExp (EDelimited open close es) = getLaTeX open ++ concat (intersperse "," (map writeExp es)) ++ getLaTeX close
writeExp (EIdentifier s) = "{" ++ getLaTeX s ++ "}"
writeExp (EMathOperator s) = getLaTeX s
writeExp (ESymbol _ s) = getLaTeX s
writeExp (ESpace width) = getSpaceCommand width 
writeExp (EBinary s e1 e2) = s ++ (inBraces e1) ++ (inBraces e2)
writeExp (ESub b e1) = writeExp b ++ "_" ++ (inBraces e1) 
writeExp (ESuper b e1) = writeExp b ++ "^" ++ (inBraces e1)  
writeExp (ESubsup b e1 e2) = writeExp b ++ "_" ++ (inBraces e1) ++ "^" ++ (inBraces e2)  
writeExp (EOver b e1) = writeExp b ++ "^_" ++ (inBraces e1) 
writeExp (EUnder b e1) = writeExp b ++ "_" ++ (inBraces e1)  
writeExp (EUnderover b e1 e2) = writeExp b ++ "_" ++ (inBraces e1) ++ "^" ++ (inBraces e2)
writeExp (EUp b e1) = writeExp b ++ "^" ++ (inBraces e1) 
writeExp (EDown b e1) = writeExp b ++ "_" ++ (inBraces e1) 
writeExp (EDownup b e1 e2) = writeExp b ++ "_" ++ (inBraces e1) ++ "^" ++ inBraces e2
writeExp (EUnary s e) = s ++ inBraces e
writeExp (EScaled size e) = ""
writeExp (EStretchy (ESymbol Open e)) = "\\left" ++ getLaTeX e
writeExp (EStretchy (ESymbol Close e)) = "\\right" ++ getLaTeX e
writeExp (EStretchy e) = writeExp e
writeExp (EArray aligns rows) = ""
writeExp (EText ttype s) = getLaTeXTextCommand ttype ++ "{"++s++"}"

inBraces :: Exp -> String
inBraces e = "{" ++ writeExp e ++ "}"




