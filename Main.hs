
module CraftingHelper.Main (Recipe, ItemStack) where

import AbLib.Control.Parser
import AbLib.Control.ParserUtils
import AbLib.Data.Tree

import GHC.IO
import Data.List
import Data.Maybe
import Data.Char

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

----- PARSING -----

type Item = String
data ItemStack = ItemStack
   { item :: Item
   , quantity :: Quantity }

type Quantity = Rational

data Recipe = Recipe
   { inputs :: [ItemStack]
   , output :: ItemStack }

type RecipeBook = Map Item [Recipe]

instance Show Recipe where
   show (Recipe i o) = (intercalate " + " $ map show i) ++ " => " ++ show o

instance Show ItemStack where
   show (ItemStack s n) = show (fromRational n) ++ " " ++ s

instance Parse Recipe where
   parser = parseRecipe

parseRecipe :: Parser Recipe
parseRecipe = do
   inputs <- greedy parseItemStackList
   optional ws >> match "=>" >> optional ws
   output <- parseItemStack
   optional ws
   maybeRate <- optional parseRate
   let qMult = maybe 1 (/ quantity output) maybeRate
   let qAlt = \s -> s {quantity = qMult * quantity s}
   pure $ Recipe (map qAlt inputs) (qAlt output)

instance Parse ItemStack where
   parser = parseItemStack

parseItemStack :: Parser ItemStack
parseItemStack = do
   c <- peek next
   guard (c /= ' ')
   n <- parser -- rational
   words <- greedy $ some (ws >> parseWord)
   let s = intercalate " " words
   pure (ItemStack s n)

parseItemStackList :: Parser [ItemStack]
parseItemStackList = pure [] <|> do
   item1 <- parseItemStack
   items <- many (optional ws >> match "+" >> optional ws >> parseItemStack)
   pure (item1:items)

parseWord :: Parser String
parseWord = greedy $ some letter

parseRate :: Parser Rational
parseRate = match "@" >> optional ws >> parser

----- FILE READING -----

loadRecipes :: FilePath -> IO RecipeBook
loadRecipes file = do
   content <- fmap lines $ readFile file
   let recipes = preprocess content >>= return . parse :: [Recipe]
   pure $ foldr aux Map.empty recipes
   
   where
   preprocess :: [String] -> [String]
   preprocess lines = do
      line <- lines
      let trimmedLine = dropWhile isSpace line
      guard (not $ null trimmedLine)
      guard (head trimmedLine /= '#')
      pure trimmedLine
   
   aux :: Recipe -> RecipeBook -> RecipeBook
   aux r b = Map.alter (append r) (item $ output r) b
   
   append :: Recipe -> Maybe [Recipe] -> Maybe [Recipe]
   append r rs = Just (r : fromMaybe [] rs)


----- CRAFTING LOGIC -----

type CraftTree = Tree ItemStack

getRecipes :: Item -> RecipeBook -> Maybe [Recipe]
getRecipes targetItem book = simplifyOptions <$> Map.lookup targetItem book
   where
   simplifyOptions :: [Recipe] -> [Recipe]
   simplifyOptions rs = let
      zeroCost = filter (null . inputs) rs
      in if null zeroCost then rs else zeroCost

craft :: Item -> Quantity -> RecipeBook -> [CraftTree]
craft targetItem targetQuantity book = case getRecipes targetItem book of
   Nothing -> []
   Just recipes -> do
      recipe <- recipes
      let targetInputQuantity = \input -> (quantity input) * targetQuantity / (quantity $ output recipe)
      let craftInput = \input -> craft (item input) (targetInputQuantity input) book
      let inputTrees = map craftInput $ inputs recipe :: [[CraftTree]]
      craftedInputs <- if null inputTrees then [[]] else sequence inputTrees
      pure $ Tree (ItemStack targetItem targetQuantity) craftedInputs
