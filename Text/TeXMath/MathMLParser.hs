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
  - mpadded
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
import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Arrow ((&&&))
import Text.TeXMath.Shared (getTextType)
import Data.Maybe (fromMaybe, listToMaybe, fromJust)
import Data.Monoid (mconcat, First(..), getFirst)
import Data.List (intersperse, transpose)
import Control.Monad.Except ( throwError 
                            , Except, runExcept, MonadError)
import Control.Monad.Reader
import Debug.Trace
import Data.Generics (everywhere, mkT)

parseMathML :: String -> Either String [Exp]
parseMathML inp = (:[]) . fixTree <$> (runExcept (runReaderT (i >>= expr) def ))
  where
    i = maybeToEither "Invalid XML" (parseXMLDoc inp)

fixTree :: Exp -> Exp
fixTree = everywhere (mkT fixNesting)

data MMLState = MMLState { attrs :: [Attr]   
                         , position :: Maybe FormType }
 

type MML = ReaderT MMLState (Except String)

empty :: Exp
empty = EGrouped []

expr :: Element -> MML Exp
expr e = local (addAttrs (elAttribs e)) (expr' e)

expr' :: Element -> MML Exp
expr' e = 
  case name e of
    "math" -> row e
    "mi" -> ident e
    "mn" -> number e
    "mo" -> op e
    "mtext" -> text e
    "ms" -> literal e
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


-- Tokens

getString :: Element -> MML String
getString e = return s
  where s = (stripSpaces . concatMap cdData .  onlyText . elContent) e

ident :: Element -> MML Exp
ident e =  do
  mv <- maybe EIdentifier (EText . getTextType) <$> findAttrQ "mathvariant" e 
  mv <$> getString e  
  

number :: Element -> MML Exp
number e = ENumber <$> getString e

op :: Element -> MML Exp
op e = do 
  inferredPosition <- fromJust <$>  ((<|>) <$> (getFormType <$> findAttrQ "form" e) <*> asks position)
  opDict <- getOperator <$> getString e <*> return inferredPosition
  props <- filterM (checkAttr (properties opDict)) ["mathoperator", "fence", "accent", "stretchy"]
  let position = form opDict
  let stretchCons = if ("stretchy" `elem` props) 
                  then EStretchy else id
  let ts =  [("accent", ESymbol Accent), ("mathoperator", EMathOperator), 
            ("fence", ESymbol (getPosition position))]
  let constructor = 
        fromMaybe (ESymbol Op) 
          (getFirst . mconcat $ map (First . flip lookup ts) props)
  return $ (stretchCons . constructor) (oper opDict)
  where 
    checkAttr ps v = maybe (v `elem` ps) (=="true") <$> findAttrQ v e   
    
 
text :: Element -> MML Exp 
text e = do
  textStyle <- maybe TextNormal getTextType 
                <$> (findAttrQ "mathvariant" e)
  EText textStyle <$> getString e 

literal :: Element -> MML Exp
literal e = do
  lquote <- fromMaybe "\x201C" <$> findAttrQ "lquote" e
  rquote <- fromMaybe "\x201D" <$> findAttrQ "rquote" e
  EText ttype cont <- text e
  return $ EText ttype (lquote ++ cont ++ rquote)

space :: Element -> MML Exp
space e = do
  width <- fromMaybe "0.0em" <$> 
            (findAttrQ "width" e)
  return $ ESpace width

-- Layout 

row :: Element -> MML Exp
row e = do
  front <- mapM expr frontSpaces
  middle <- local resetPosition (row' body)
  end <- local resetPosition (mapM expr endSpaces)
  return $ EGrouped (front ++ middle ++ end)
  where
    cs = elChildren e
    (frontSpaces, noFront)  = span spacelike cs
    (endSpaces, body) = let (as, bs) = span spacelike (reverse noFront) in
                          (reverse as, reverse bs)

row' :: [Element] -> MML [Exp]
row' [] = return []
row' [x] = do 
              pos <- maybe FInfix (const FPostfix) <$> asks position
              (:[]) <$> local (setPosition pos) (expr x)
row' (x:xs) =
  do 
    pos <- maybe FPrefix (const FInfix) <$> asks position
    e  <- local (setPosition pos) (expr x)
    es <- local (setPosition pos) (row' xs)
    return (e: es)


frac :: Element -> MML Exp
frac e = do
  [num, dom] <- mapM expr =<< (checkArgs 2 e)
  --constructor <- maybe "\\frac" (\l -> "\\genfrac{}{}{}{" ++ thicknessToNum l ++ "}{}") <$> (findAttrQ "linethickness" e)
  let constructor = "\\frac"
  return $ EBinary constructor num dom

msqrt :: Element -> MML Exp
msqrt e = EUnary "\\sqrt" <$> (row e)

kroot :: Element -> MML Exp
kroot e = do 
  [base, index] <- mapM expr =<< (checkArgs 2 e)
  return $ EBinary "\\sqrt" index base

phantom :: Element -> MML Exp
phantom e = EUnary "\\phantom" <$> row e

fenced :: Element -> MML Exp
fenced e = do
  open  <- fromMaybe "(" <$> (findAttrQ "open" e) 
  close <- fromMaybe ")" <$> (findAttrQ "close" e) 
  let enclosed = not (null open || null close)
  sep  <- fromMaybe "," <$> (findAttrQ "separators" e)
  let expanded = 
        case sep of
          "" -> elChildren e
          _  ->
            let seps = map (\x -> unode "mo" [x]) sep
                sepsList = seps ++ repeat (last seps) in
                fInterleave (elChildren e) (sepsList) 
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

--interleave up to end of shorter list 
fInterleave :: [a] -> [a] -> [a]
fInterleave [] ys = []
fInterleave xs [] = []
fInterleave (x:xs) ys = x : fInterleave ys xs

-- This could approximate the variants better
enclosed :: Element -> MML Exp
enclosed = row 

action :: Element -> MML Exp
action e = do 
  selection <-  maybe 1 read <$> (findAttrQ "selction" e)  -- 1-indexing
  expr =<< maybeToEither ("Selection out of range") 
            (listToMaybe $ drop (selection - 1) (elChildren e))

-- Scripts and Limits

sub :: Element -> MML Exp
sub e = do
  [base, subs] <- (checkArgs 2 e)
  ESub <$> expr base <*> local (setPosition FPostfix) (expr subs)

sup :: Element -> MML Exp
sup e = do
  [base, sups] <- (checkArgs 2 e)
  ESuper <$> expr base <*> local (setPosition FPostfix) (expr sups)

subsup :: Element -> MML Exp
subsup e = do
  [base, subs, sups] <- (checkArgs 3 e)
  ESubsup <$> expr base <*> local (setPosition FPostfix) (expr subs) 
                         <*> local (setPosition FPostfix) (expr sups)

under :: Element -> MML Exp
under e = do
  [base, below] <- (checkArgs 2 e)
  EUnder <$> expr base <*> local (setPosition FPostfix) (expr below)

over :: Element -> MML Exp
over e = do
  [base, above] <- (checkArgs 2 e)
  EOver <$>  expr base <*> local (setPosition FPostfix) (expr above)

underover :: Element -> MML Exp
underover e = do
  [base, below, above] <- (checkArgs 3 e)
  EUnderover <$> expr base  <*> local (setPosition FPostfix) (expr below)
                             <*> local (setPosition FPostfix) (expr above)

-- Other

semantics :: Element -> MML Exp
semantics e = EGrouped <$> mapM expr (elChildren e)

annotation :: Element -> MML Exp
annotation e = do 
  encoding <- findAttrQ "encoding" e
  case encoding of 
    Just "application/mathml-presentation+xml" -> 
      EGrouped <$> mapM expr (elChildren e)
    Just "MathML-Presentation" -> 
      EGrouped <$> mapM expr (elChildren e)
    _ -> return empty

-- Table

table :: Element -> MML Exp
table e = do
  defAlign <- maybe AlignDefault toAlignment <$> (findAttrQ "columnalign" e)
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
tableRow a e = do
  align <- maybe a toAlignment <$> (findAttrQ "columnalign" e)
  case name e of
    "mtr" -> mapM (tableCell align) (elChildren e)
    "mlabeledtr" -> mapM (tableCell align) (tail $ elChildren e)
    _ -> throwError $ "tableRow " ++ err e

tableCell :: Alignment -> Element -> MML (Alignment, [Exp])
tableCell a e = do 
  align <- maybe a toAlignment <$> (findAttrQ "columnalign" e)
  case name e of
    "mtd" -> (,) align <$> mapM expr (elChildren e) 
    _ -> throwError $ "tableCell " ++ err e

-- Fixup

-- See Exception for embellished operators, this only affects stretchy

fixNesting :: Exp -> Exp
fixNesting (EOver (EStretchy e) s) = EStretchy (EOver e s)
fixNesting (EUnder (EStretchy e) s) = EStretchy (EUnder e s)
fixNesting (EUnderover (EStretchy e) s1 s2) = EStretchy (EUnderover e s1 s2)
fixNesting (ESub (EStretchy e) s) = EStretchy (ESub e s)
fixNesting (ESuper (EStretchy e) s) = EStretchy (ESuper e s)
fixNesting (ESubsup (EStretchy e) s1 s2) = EStretchy (ESubsup e s1 s2)
fixNesting e = e

-- MMLState helper functions

def :: MMLState
def = MMLState [] Nothing

addAttrs :: [Attr] -> MMLState -> MMLState
addAttrs as s = s {attrs = as ++ attrs s }

setPosition :: FormType -> MMLState -> MMLState
setPosition p s = s {position = Just p}

resetPosition :: MMLState -> MMLState
resetPosition s = s {position = Nothing}

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

findAttrQ :: String -> Element -> MML (Maybe String)
findAttrQ s e = do 
  inherit <- asks (lookupAttrQ s . attrs)
  return $
    findAttr (QName s Nothing Nothing) e
      <|> inherit 

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

getPosition :: FormType -> TeXSymbolType
getPosition (FPrefix) = Open
getPosition (FPostfix) = Close
getPosition (FInfix) = Op               

getFormType :: Maybe String -> Maybe FormType
getFormType (Just "infix") = (Just FInfix)
getFormType (Just "prefix") = (Just FPrefix)
getFormType (Just "postfix") = (Just FPostfix)
getFormType _ = Nothing

pad :: Int -> [[a]] -> [[a]]
pad n xs = xs ++ (replicate (n - len) [])
  where 
    len = length xs

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _    = False

spacelikeElems, cSpacelikeElems :: [String]
spacelikeElems = ["mtext", "mspace", "maligngroup", "malignmark"]
cSpacelikeElems = ["mrow", "mstyle", "mphantom", "mpadded"]

spacelike :: Element -> Bool
spacelike e@(name -> uid) = 
  uid `elem` spacelikeElems || uid `elem` cSpacelikeElems &&
    and (map spacelike (elChildren e))


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
