{-# LANGUAGE ViewPatterns #-}
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
{-
Parses MathML in conformance with the MathML3 specification. 

Unimplemented features
  - menclose
  - mstyle 
  - mpadded
  - mliteral
  - mmultiscripts (etc)
  - malignmark
  - maligngroup
  - Elementary Math

To Improve
  - Handling of menclose
  - Handling of mstyle
-}

module Text.TeXMath.MathMLParser (parseMathML) where

import Text.XML.Light hiding (onlyText)
import Text.TeXMath.Types
import Text.TeXMath.MMLDict (getOperator)
import Text.TeXMath.EntityMap (getUnicode)
import Control.Applicative ((<$>), (<|>))
import Control.Arrow ((&&&))
import Text.TeXMath.Shared (getTextType)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (mconcat, First(..), getFirst)
import Data.List (intersperse, transpose)
import Control.Monad.Except ( throwError, catchError
                            , Except, runExcept, MonadError)
import Control.Monad.Reader
import Debug.Trace

parseMathML :: String -> Either String [Exp]
parseMathML inp = (:[]) <$> (runExcept (runReaderT (i >>= expr) []))
  where
    i = maybeToEither "Invalid XML" (parseXMLDoc inp)

type MML = ReaderT [Attr] (Except String)

empty :: Exp
empty = EGrouped []

expr :: Element -> MML Exp
expr e = local (elAttribs e ++) (expr' e)

expr' :: Element -> MML Exp
expr' e = 
  case name e of
    "math" -> EGrouped <$> (mapM expr cs)
    "mi" -> ident e
    "mn" -> number e
    "mo" -> op e
    "mtext" -> text e
    "mspace" -> space e
    "mrow" -> row e
    "mfrac" -> frac e
    "msqrt" -> msqrt e
    "mroot" -> kroot e
    "mstyle" -> row e
    "merror" -> return $ empty
    "mpadded" -> row e
    "mphantom" -> phantom e
    "mfenced" -> fenced e
    "menclose" -> enclosed e
    "msub" -> sub e
    "msup" -> sup e
    "msubsup" -> subsup e
    "munder" -> under e
    "mover" -> over e
    "munderover" -> underover e
    "mtable" -> table e
    "maction" -> action e
    "semantics" -> semantics e
    "annotation-xml" -> annotation e 
    _ -> return $ empty 
  where
    cs = elChildren e

-- Tokens

getString :: Element -> MML String
getString e = if null s then throwError ("getString " ++ (err e)) else return s
  where s = (stripSpaces . concatMap cdData .  onlyText . elContent) e

ident :: Element -> MML Exp
ident e =  EIdentifier <$> catchError (getString e) (const $ return "")  

number :: Element -> MML Exp
number e = ENumber <$> getString e

op :: Element -> MML Exp
op e = do 
  env <- ask
  opDict <- getOperator <$> getString e
  let props = filter (checkAttr env) (properties opDict) 
  let stretchCons = if ("stretchy" `elem` props) 
                  then EStretchy else id
  let position = getPosition (form opDict)
  let ts =  [("accent", ESymbol Accent), ("mathoperator", EMathOperator), 
            ("fence", ESymbol position)]
  let constructor = 
        fromMaybe (ESymbol Op) 
          (getFirst . mconcat $ map (First . flip lookup ts) props)
  return $ (stretchCons . constructor) (oper opDict)
  where 
    checkAttr env v = maybe True (=="true") (findAttrQ v e <|> lookupAttrQ v env)  
    
getPosition :: FormType -> TeXSymbolType
getPosition (FPrefix) = Open
getPosition (FPostfix) = Close
getPosition (FInfix) = Op               
 
text :: Element -> MML Exp 
text e = do
  let textStyle = maybe TextNormal getTextType 
                    (findAttrQ "mathvariant" e)
  EText textStyle <$> getString e 

space :: Element -> MML Exp
space e = do
  let width = fromMaybe "0.0em" (findAttrQ "width" e)
  return $ ESpace width

-- Layout 

row :: Element -> MML Exp
row e = EGrouped <$> mapM expr (elChildren e)

frac :: Element -> MML Exp
frac e = do
  [num, dom] <- mapM expr =<< (checkArgs 2 e)
  let constructor = maybe "\\frac" (\l -> "\\genfrac{}{}{" ++ thicknessToNum l ++ "}") (findAttrQ "linethickness" e)
  return $ EBinary constructor num dom

msqrt :: Element -> MML Exp
msqrt e = EUnary "\\sqrt" <$> (row e)

kroot :: Element -> MML Exp
kroot e = do 
  [base, index] <- mapM expr =<< (checkArgs 2 e)
  return $ EBinary "\\sqrt" base index

phantom :: Element -> MML Exp
phantom e = EUnary "\\phantom" <$> row e

fenced :: Element -> MML Exp
fenced e = do
  let open  = fromMaybe "(" (findAttrQ "open" e) 
  let close = fromMaybe ")" (findAttrQ "close" e) 
  let enclosed = not (null open || null close)
  let sep   = fromMaybe "," (findAttrQ "separators" e)
  let expanded = intersperse (unode "mo" sep) (elChildren e)
  case (sep, enclosed) of 
    ("", True) -> EDelimited open close <$> mapM expr (elChildren e)
    (s, True)  -> expr $ sepAttr (unode "mfenced" 
                    (elAttribs e, expanded))
    (s, False) -> expr $ unode "mrow" 
                          ([unode "mo" open | not $ null open] ++ 
                           [unode "mrow" expanded] ++ 
                           [unode "mo" close | not $ null close])
  where 
    sepAttr = add_attr (Attr (unqual "separators") "")

-- This could approximate the variants better
enclosed :: Element -> MML Exp
enclosed = row 

action :: Element -> MML Exp
action e = 
  let selection = maybe 1 read (findAttrQ "selction" e) in -- 1-indexing
  expr =<< maybeToEither ("Selection out of range") 
            (listToMaybe $ drop (selection - 1) (elChildren e))

-- Scripts and Limits

sub :: Element -> MML Exp
sub e = do
  [base, subs] <- mapM expr =<< (checkArgs 2 e)
  return $ ESub base subs

sup :: Element -> MML Exp
sup e = do
  [base, sups] <- mapM expr =<< (checkArgs 2 e)
  return $ ESuper base sups

subsup :: Element -> MML Exp
subsup e = do
  [base, subs, sups] <- mapM expr =<< (checkArgs 3 e)
  return $ ESubsup base subs sups

under :: Element -> MML Exp
under e = do
  [base, below] <- mapM expr =<< (checkArgs 2 e)
  return $ EUnder base below

over :: Element -> MML Exp
over e = do
  [base, above] <- mapM expr =<< (checkArgs 2 e)
  return $ EOver base above

underover :: Element -> MML Exp
underover e = do
  [base, below, above] <- mapM expr =<< (checkArgs 3 e)
  return $ EUnderover base below above

-- Other

semantics :: Element -> MML Exp
semantics e = EGrouped <$> mapM expr (elChildren e)

annotation :: Element -> MML Exp
annotation e = 
  case findAttrQ "encoding" e of 
    Just "application/mathml-presentation+xml" -> 
      EGrouped <$> mapM expr (elChildren e)
    Just "MathML-Presentation" -> 
      EGrouped <$> mapM expr (elChildren e)
    _ -> return empty

-- Table

table :: Element -> MML Exp
table e = do
  let defAlign = maybe AlignDefault toAlignment (findAttrQ "columnalign" e)
  rs <- mapM (tableRow defAlign) (elChildren e)
  let (onlyAligns, exprs) = (map .map) fst &&& (map . map) snd $ rs
  let rs' = map (pad (maximum (map length rs))) exprs
  let aligns = map findAlign (transpose onlyAligns)
  return $ EArray aligns rs'
  where
    findAlign xs = if null xs then AlignDefault
                    else foldl1 combine xs
    combine x y = if x == y then x else AlignDefault 

tableRow :: Alignment -> Element -> MML [(Alignment, [Exp])]
tableRow a e = 
  case name e of
    "mtr" -> mapM (tableCell align) (elChildren e)
    "mlabeledtr" -> mapM (tableCell align) (tail $ elChildren e)
    _ -> throwError $ "tableRow " ++ err e
  where
    align = maybe a toAlignment (findAttrQ "columnalign" e)

tableCell :: Alignment -> Element -> MML (Alignment, [Exp])
tableCell a e = 
  case name e of
    "mtd" -> (,) align <$> mapM expr (elChildren e) 
    _ -> throwError $ "tableCell " ++ err e
  where
    align = maybe a toAlignment (findAttrQ "columnalign" e)

-- Utility

checkArgs :: Int -> Element -> MML [Element]
checkArgs x e = do
  let cs = elChildren e
  if nargs x cs 
    then return cs 
    else (throwError ("Incorrect number of arguments for " ++ err e))

nargs :: Int -> [a] -> Bool
nargs n xs = length xs == n 

onlyText :: [Content] -> [CData]
onlyText [] = []
onlyText ((Text c):xs) = c : onlyText xs
onlyText (CRef s : xs)  = (CData CDataText (fromMaybe s $ getUnicode s) Nothing) : onlyText xs
onlyText (_:xs) = onlyText xs
    
err :: Element -> String
err e = name e ++ " line: " ++ (show $ elLine e) ++ (show e)

maybeToEither :: (MonadError e m) => e -> Maybe a -> m a
maybeToEither = flip maybe return . throwError

findAttrQ :: String -> Element -> Maybe String
findAttrQ s = findAttr (QName s Nothing Nothing)

lookupAttrQ :: String -> [Attr] -> Maybe String
lookupAttrQ s = lookupAttr (QName s Nothing Nothing)

name :: Element -> String
name (elName -> (QName n _ _)) = n

stripSpaces :: String -> String
stripSpaces = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace)

toAlignment :: String -> Alignment
toAlignment "left" = AlignLeft
toAlignment "center" = AlignCenter
toAlignment "right" = AlignRight
toAlignment _ = AlignDefault

pad :: Int -> [[a]] -> [[a]]
pad n xs = xs ++ (replicate (n - len) [])
  where 
    len = length xs

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _    = False

thicknessToNum :: String -> String
thicknessToNum "thin" = "0.05mm"
thicknessToNum "medium" = ""
thicknessToNum "thick" = "0.3mm"
thicknessToNum v = processLength v

processLength :: String -> String 
processLength s = show (n * (unitToLaTeX unit)) ++ "mm"
  where 
    ((n, unit): _) = reads s :: [(Float, String)]

unitToLaTeX :: String -> Float
unitToLaTeX "pt" = 2.84
unitToLaTeX "mm" = 1
unitToLaTeX "cm" = 10
unitToLaTeX "in" = 25.4
unitToLaTeX "ex" = 1.51
unitToLaTeX "em" = 3.51 
unitToLaTeX "mu" = 18 * unitToLaTeX "em"
unitToLaTeX _    = 1
