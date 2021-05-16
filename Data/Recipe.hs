
module CraftingHelper.Data.Recipe where

import CraftingHelper.Data.ItemStack

import AbLib.Control.Parser
import AbLib.Control.ParserUtils

import Data.List

--------------------------------------------------------------------------------

data Recipe = Recipe
   { inputs :: [ItemStack]
   , output :: ItemStack }

--------------------------------------------------------------------------------

instance Show Recipe where
   show (Recipe i o) = (intercalate " + " $ map show i) ++ " => " ++ show o

instance Parse Recipe where
   parser = do
      inputs <- greedy parseItemStackList
      optional ws >> match "=>" >> optional ws
      output <- parser
      optional ws
      maybeRate <- optional parseRate
      let qMult = maybe 1 (/ quantity output) maybeRate
      let qAlt = \s -> s {quantity = qMult * quantity s}
      pure $ Recipe (sort $ map qAlt inputs) (qAlt output)
      
      where
      parseItemStackList :: Parser [ItemStack]
      parseItemStackList = pure [] <|> do
         item1 <- parser
         items <- many (optional ws >> match "+" >> optional ws >> parser)
         pure (item1:items)

      parseRate :: Parser Rational
      parseRate = match "@" >> optional ws >> parser
