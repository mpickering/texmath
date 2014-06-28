{-# LANGUAGE ViewPatterns #-}
{-

Unimplemented 
  
  mliteral
  multi
  malignmark
  maligngroup
  maction
  Elementary Math
  -}
module Text.TeXMath.MathMLParser (testRead, parseMathML) where

import Text.XML.Light hiding (onlyText)
import Text.TeXMath.Types
import Text.TeXMath.MMLDict
import Text.TeXMath.EntityMap
import qualified Data.Map as M
import Control.Applicative ((<*>),(<$>))
import Control.Arrow ((&&&))
import Text.TeXMath.Shared (getTextType)
import Data.Maybe
import Data.Monoid
import Data.List (transpose)
import Debug.Trace
import Control.Monad.Except

dict :: M.Map String Operator
dict = M.fromList (map (\o -> (oper o, o)) operators)

safeLookup :: String -> Operator
safeLookup s = fromMaybe (Operator s "" FInfix 0 0 0 ["mathoperator"]) (M.lookup s dict) 

testRead :: String -> IO (Either String [Exp])
testRead s = parseMathML <$> readFile s 

parseMathML :: String -> Either String [Exp]
parseMathML inp = (:[]) <$> (runExcept (i >>= expr))
  where
    i = maybeToEither "Invalid XML" (parseXMLDoc inp)

type MML = Except String 

expr :: Element -> MML Exp
expr e = 
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
    "mstyle" -> style e
    "merror" -> merror e
    "mpadded" -> padded e
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
    _ -> return $ EGrouped []
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
  opDict <- safeLookup <$> getString e
  let props = properties opDict ++ 
                ["fence" | fencedAttr] ++ ["accent" | accented]
  let stretchCons = if ("stretchy" `elem` props || stretchy) 
                  then EStretchy else id
  let position = getPosition (form opDict)
  let ts = [("accent", ESymbol Accent), ("mathoperator", EMathOperator), 
            ("fence", ESymbol position)]
  let constructor = 
        fromMaybe (ESymbol Op) 
          (getFirst . mconcat $ map (First . flip lookup ts) props)
  return $ (stretchCons . constructor) (oper opDict)
  where 
    checkAttr v = maybe False (=="true") (findAttrQ v e)
    fencedAttr = checkAttr "fence"
    accented = checkAttr "accent" 
    stretchy = checkAttr "stretchy"
    
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

checkArgs :: Int -> Element -> MML [Element]
checkArgs x e = do
  let cs = elChildren e
  if nargs x cs 
    then return cs 
    else (throwError ("Incorrect number of arguments for " ++ err e))

row :: Element -> MML Exp
row e = EGrouped <$> mapM expr (elChildren e)

frac :: Element -> MML Exp
frac e = do
  cs <- checkArgs 2 e
  EBinary "\\frac" <$> (expr (cs !! 0))  <*> (expr (cs !! 1))

msqrt :: Element -> MML Exp
msqrt e = do
  cs <- catchError (checkArgs 1 e) (const $ return ([unode "mrow" (elChildren e)])) 
  EUnary "\\sqrt" <$> expr (cs !! 0)

kroot :: Element -> MML Exp
kroot e = do 
  cs <- checkArgs 2 e 
  EBinary "\\sqrt" <$> expr (cs !! 0) <*> expr (cs !! 1)

style :: Element -> MML Exp
style = row 

merror :: Element -> MML Exp
merror = row

padded :: Element -> MML Exp
padded = row

phantom :: Element -> MML Exp
phantom e = EUnary "\\phantom" <$> row e

fenced :: Element -> MML Exp
fenced e = do
  let open = fromMaybe "(" (findAttrQ "open" e) 
  let close = fromMaybe ")" (findAttrQ "close" e) 
  EDelimited open close <$> mapM expr (elChildren e)

enclosed :: Element -> MML Exp
enclosed = expr

action :: Element -> MML Exp
action e = 
  let selection = maybe 1 read (findAttrQ "selction" e) in
  expr =<< maybeToEither ("Selection out of range") (listToMaybe $ drop ((traceShow (selection) (selection)) - 1) (elChildren e))

-- Scripts and Limits

sub :: Element -> MML Exp
sub e = do
  cs <- checkArgs 2 e 
  ESub <$> expr (cs !! 0) <*> expr (cs !! 1)

sup :: Element -> MML Exp
sup e = do
  cs <- checkArgs 2 e
  ESuper <$> expr (cs !! 0) <*> expr (cs !! 1)

subsup :: Element -> MML Exp
subsup e = do
  cs <- checkArgs 3 e 
  ESubsup <$> expr (cs !! 0)  <*> expr (cs !! 1) <*> expr (cs !! 2)

under :: Element -> MML Exp
under e = do
  cs <- checkArgs 2 e
  EUnder <$> expr (cs !! 0)  <*> expr (cs !! 1)

over :: Element -> MML Exp
over e = do
  cs <- checkArgs 2 e
  EOver <$> expr (cs !! 0) <*> expr (cs !! 1)

underover :: Element -> MML Exp
underover e = do
  cs <- checkArgs 3 e
  EUnderover <$> expr (cs !! 0)  <*> expr (cs !! 1) <*> expr (cs !! 2)

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

