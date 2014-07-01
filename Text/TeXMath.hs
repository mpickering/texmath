{-
Copyright (C) 2009-2012 John MacFarlane <jgm@berkeley.edu>

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

{- | Functions for converting LaTeX math formulas to MathML.
-}

module Text.TeXMath ( texMathToMathML, 
                      texMathToOMML, 
                      texMathToPandoc,
                      mathMLToOMML,
                      mathMLToPandoc,
                      mathMLToLaTeX, 
                      DisplayType(..) )
where
import Text.TeXMath.Parser
import Text.TeXMath.MathMLParser
import Text.TeXMath.MathML
import Text.TeXMath.OMML
import Text.TeXMath.Pandoc
import Text.TeXMath.LaTeX
import Text.TeXMath.Types
import Text.XML.Light
import Text.Pandoc.Definition
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

texMathToMathML :: DisplayType -> String -> Either String Element
texMathToMathML dt inp = inp `seq`
  toMathML dt <$> parseFormula inp

texMathToOMML :: DisplayType -> String -> Either String Element
texMathToOMML dt inp = inp `seq`
  toOMML dt <$> parseFormula inp

texMathToPandoc :: DisplayType -> String -> Either String [Inline]
texMathToPandoc dt inp = inp `seq`
  fromMaybe fallback . toPandoc dt <$> parseFormula inp
  where fallback = [Str $ delim ++ inp ++ delim]
        delim    = case dt of { DisplayInline -> "$"; DisplayBlock -> "$$" }

mathMLToOMML :: DisplayType -> String -> Either String Element
mathMLToOMML dt inp = inp `seq`
   (toOMML dt) <$> parseMathML inp

mathMLToPandoc :: DisplayType -> String -> Either String [Inline]
mathMLToPandoc dt inp = inp `seq`
  fromMaybe fallback . toPandoc dt <$> parseMathML inp
  where fallback = [Str $ delim ++ inp ++ delim]
        delim    = case dt of { DisplayInline -> "$"; DisplayBlock -> "$$" }

mathMLToLaTeX :: DisplayType -> String -> Either String Inline
mathMLToLaTeX dt inp = inp `seq`
  rt . toLaTeX <$> parseMathML inp 
  where 
    mathType = case dt of { DisplayInline -> InlineMath; 
                            DisplayBlock -> DisplayMath } 
    rt s = case s of 
            "" -> Str ""
            _  -> Math mathType s
