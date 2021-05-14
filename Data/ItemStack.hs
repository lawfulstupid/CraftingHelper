
module CraftingHelper.Data.ItemStack where

import AbLib.Control.Parser
import AbLib.Control.ParserUtils

import Data.List

--------------------------------------------------------------------------------

type Item = String
type Quantity = Rational

data ItemStack = ItemStack
   { item :: Item
   , quantity :: Quantity }

--------------------------------------------------------------------------------

instance Show ItemStack where
   show (ItemStack s n) = show (fromRational n) ++ " " ++ s

instance Parse ItemStack where
   parser = do
      c <- peek next
      guard (c /= ' ')
      n <- parser -- rational
      words <- greedy $ some (ws >> parseWord)
      let s = intercalate " " words
      pure (ItemStack s n)
      where

      parseWord :: Parser String
      parseWord = greedy $ some letter

qop :: (Quantity -> Quantity -> Quantity) -> ItemStack -> ItemStack -> ItemStack
qop f (ItemStack i1 q1) (ItemStack i2 q2)
   | i1 /= i2 = error "Mismatching stacks"
   | otherwise = ItemStack i1 (f q1 q2)

instance Num ItemStack where
   (+) = qop (+)
   (*) = qop (*)
   (-) = qop (-)
   abs = id
   signum = const 1
   fromInteger n = ItemStack "" $ fromInteger n
   