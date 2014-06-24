module Text.TeXMath.MathMLParser (parseMathML) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Control.Monad
import Text.TeXMath.Types
import Control.Applicative ((<*>), (<*), (*>), (<$>), (<$))
import Control.Arrow ((&&&))
import Text.Parsec.Pos (newPos)
import Text.Parsec hiding (space)
import Text.TeXMath.Shared (getTextType)
import Data.Maybe
import Debug.Trace
import Data.List (transpose)

parseMathML :: String -> Either String [Exp]
parseMathML inp = either (Left . show) (Right . id) $ 
                    (parse formula "" tags)
    where tags = removeComments $ canonicalizeTags $
                   parseTagsOptions parseOptions{ optTagPosition = True } inp

type MML = Parsec [Tag String] ()

formula :: MML [Exp]
formula = do 
  optional pBlank
  r <- pInTags "math" (many expr)
  optional pBlank
  eof
  return r

expr :: MML Exp
expr =
  optional (many ignored) *>
  choice  
    [ ident
    , number
    , op
    , text 
    , space 
    -- grouping
    , row
    , frac
    , msqrt
    , kroot
    , style
    , merror
    , padded
    , phantom
    , enclosed
    , fenced
    , sub
    , sup
    , subsup 
    , under
    , over 
    , underover
    , table
    ] <* (optional $ many ignored)

  
ignored :: MML ()
ignored = try $ choice $
  [ literal
  , pBlank
  , multi
  , none
  , alignmark
  , aligngroup
  , action
  ] ++ elemMath
  

-- Tokens

ident :: MML Exp
ident =  EIdentifier <$> pInTags "mi" (pAnyString <|> return "")

number :: MML Exp
number = ENumber <$> pInTags "mn" pAnyString

op :: MML Exp
op = try $ do
  tag <- lookAhead $ pSatisfy $ (~== TagOpen "mo" []) 
  let stretchy = fromAttrib "stretchy" tag
  let f = case stretchy of 
            "true" -> EStretchy
            _ -> id
  f . EMathOperator <$> pInTags "mo" pAnyString

text :: MML Exp 
text = try $ do
  tag <- lookAhead $ pSatisfy $ (~== TagOpen "mtext" [])
  let textStyle = getTextType $ fromAttrib "mathvariant" tag
  EText textStyle <$> pInTags "mtext" pAnyString


space :: MML Exp
space = try $ do
  TagOpen _ attr <- pSelfClosing "mspace" 
  let width = fromMaybe "0em" (lookup "width" attr)
  return $ ESpace width


literal :: MML ()
literal = () <$ pInTags "ms" pAnyTag

none :: MML ()
none = () <$ pSelfClosing "none" 


-- Layout 

row :: MML Exp
row = EGrouped <$> pInTags "mrow" (many expr)

frac :: MML Exp
frac = pInTags "mfrac" (EBinary "\\frac" <$> expr <*> expr)

msqrt :: MML Exp
msqrt = EUnary "\\sqrt" <$> pInTags "msqrt" maybeRow

kroot :: MML Exp
kroot = pInTags "mroot" (EBinary "\\sqrt" <$> expr <*> expr)

style :: MML Exp
style = pInTags "mstyle" maybeRow

merror :: MML Exp
merror = pInTags "merror" maybeRow

padded :: MML Exp
padded = pInTags "mpadded" maybeRow

phantom :: MML Exp
phantom = EUnary "\\phantom" <$> pInTags "mphantom" maybeRow

fenced :: MML Exp
fenced = try $ do
  TagOpen _ attr <- lookAhead $ pSatisfy (~== TagOpen "mfenced" [])
  let open = fromMaybe "(" (lookup "open" attr) 
  let close = fromMaybe ")" (lookup "close" attr) 
  EDelimited open close <$> pInTags "mfenced" (many expr)

enclosed :: MML Exp
enclosed = pInTags "menclose" expr

-- Scripts and Limits

sub :: MML Exp
sub = pInTags "msub" (ESub <$> expr <*> expr)

sup :: MML Exp
sup = pInTags "msup" (ESuper <$> expr <*> expr)

subsup :: MML Exp
subsup = pInTags "msubsup" (ESubsup <$> expr <*> expr <*> expr)

under :: MML Exp
under = pInTags "munder" (EUnder <$> expr <*> expr)

over :: MML Exp
over = pInTags "mover" (EOver <$> expr <*> expr)

underover :: MML Exp
underover = pInTags "munderover" (EUnderover <$> expr <*> expr <*> expr)

multi :: MML ()
multi = () <$ pInTags "mmultiscripts" maybeRow

-- Table

table :: MML Exp
table = try $ do
  TagOpen _ attr <- lookAhead $ pSatisfy (~== TagOpen "mtable" [])
  let defAlign = maybe AlignDefault toAlignment (lookup "columnalign" attr)
  rs <- pInTags "mtable" (many $ (optional pBlank *> tableRow defAlign))
  let (onlyAligns, exprs) = (map .map) fst &&& (map . map) snd $ rs
  let rs' = map (pad (maximum (map length rs))) exprs
  let aligns = map findAlign (transpose onlyAligns)
  return $ EArray aligns rs'
  where
    findAlign xs = if null xs then AlignDefault
                    else foldl combine (head xs) (tail xs)
    combine x y = if x == y then x else AlignDefault 

tableRow :: Alignment -> MML [(Alignment, [Exp])]
tableRow a = try $ do
  optional pBlank
  TagOpen _ attr <- lookAhead $ tr 
  let align = maybe a toAlignment (lookup "columnalign" attr)
  pInTags "mtr" (many $ (tableCell align)) <|>
    pInTags "mlabeledtr" (optional pBlank *> pInTags "mtd" expr *> 
      many (tableCell align))
        <* optional pBlank
  where 
    tr = pSatisfy (~== TagOpen "mtr" []) <|>
          pSatisfy (~== TagOpen "mlabeledtr" [])  

tableCell :: Alignment -> MML (Alignment, [Exp])
tableCell a = try $ do 
  optional pBlank
  TagOpen _ attr <- lookAhead $ pSatisfy (~== TagOpen "mtd" [])
  let align = maybe a  toAlignment (lookup "columnalign" attr)
  (,) align <$> pInTags "mtd" (many expr) 
    <* optional pBlank

-- ignored table specific

aligngroup :: MML ()
aligngroup = () <$ pSelfClosing "maligngroup"

alignmark :: MML ()
alignmark = () <$ pSelfClosing "malignmark"

-- Ignored Elementary Math

elemMath :: [MML ()] 
elemMath = [stack, sgroup, srow, sline, scarries, scarry, longdiv]

stack, sgroup, srow, sline, scarries, scarry, longdiv :: MML ()
stack = () <$ pInTags "mstack" expr
sgroup = () <$ pInTags "msgroup" expr
srow = () <$ pInTags "msrow" expr
sline = () <$ pInTags "msline" expr
scarries = () <$ pInTags "mscarries" expr
scarry = () <$ pInTags "mscarry" expr
longdiv = () <$ pInTags "mlongdiv" expr

-- Action Elements

action :: MML ()
action = () <$ pInTags "maction" expr

-- Utility

stripSpaces :: String -> String
stripSpaces = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace)

maybeRow :: MML Exp
maybeRow = try row <|> (EGrouped <$> many expr)

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

removeComments :: [Tag String] -> [Tag String]
removeComments [] = [] 
removeComments (_: TagComment _: xs) = removeComments xs
removeComments (x:xs) = x: removeComments xs

-- Useful combinators taken from Pandoc HTML reader

pAnyString :: MML String
pAnyString = try $ do 
  (TagText s) <- pSatisfy isTagText
  return (stripSpaces s)

pLocation :: MML ()
pLocation = do
  (TagPosition r c) <- pSat isTagPosition
  setPosition $ newPos "input" r c

pSat :: (Tag String -> Bool) -> MML (Tag String)
pSat f = do
  pos <- getPosition
  token show (const pos) (\x -> if f x then Just x else Nothing)

pSatisfy :: (Tag String -> Bool) -> MML (Tag String)
pSatisfy f = try $ optional pLocation *> pSat f

pAnyTag :: MML (Tag String)
pAnyTag = pSatisfy (const True)

pSelfClosing :: String 
             -> MML (Tag String)
pSelfClosing t = do
  open <- pSatisfy (~== TagOpen t [])
  optional $ pSatisfy (~== TagClose t)
  return open

pInTags :: String -> MML a
        -> MML a
pInTags tagtype parser = try $ do
  pSatisfy (~== TagOpen tagtype [])
  trace (tagtype ++ " open") (return ())
  r <- parser 
  pSatisfy (~== TagClose tagtype)
  trace (tagtype ++ " close") (return ())
  return r

pBlank :: MML ()
pBlank = try $ do
  (TagText str) <- pSatisfy isTagText
  guard $ all isSpace str


