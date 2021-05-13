
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
   , quantity :: Rational }

type Rate = Rational

data Recipe = Recipe
   { inputs :: [ItemStack]
   , output :: ItemStack
   , rate :: Rate }

type RecipeBook = Map Item [Recipe]

instance Show Recipe where
   show (Recipe i o r) = let
      inputs = intercalate " + " $ map show i
      rate = " @ " ++ show r
      in inputs ++ " => " ++ show o ++ rate

instance Show ItemStack where
   show (ItemStack s n) = show n ++ " " ++ s

instance Parse Recipe where
   parser = parseRecipe

parseRecipe :: Parser Recipe
parseRecipe = do
   inputs <- greedy parseItemStackList
   optional ws >> match "=>" >> optional ws
   output <- parseItemStack
   optional ws
   rate <- pure 1 <|> parseRate
   pure (Recipe inputs output rate)

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

parseRate :: Parser Rate
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

data CraftItem = CraftItem Item Rate
instance Show CraftItem where
   show (CraftItem item rate) = item ++ " @" ++ show (fromRational rate)

type CraftTree = Tree CraftItem

getRecipes :: Item -> RecipeBook -> Maybe [Recipe]
getRecipes targetItem book = simplifyOptions <$> Map.lookup targetItem book
   where
   simplifyOptions :: [Recipe] -> [Recipe]
   simplifyOptions rs = let
      zeroCost = filter (null . inputs) rs
      in if null zeroCost then rs else zeroCost

craftAtRate :: Item -> Rate -> RecipeBook -> [CraftTree]
craftAtRate targetItem targetRate book = case getRecipes targetItem book of
   Nothing -> []
   Just recipes -> do
      recipe <- recipes
      let targetInputRate = \input -> (quantity input) * targetRate / (quantity $ output recipe)
      let craftInput = \input -> craftAtRate (item input) (targetInputRate input) book
      let inputTrees = map craftInput $ inputs recipe :: [[CraftTree]]
      craftedInputs <- if null inputTrees then [[]] else sequence inputTrees
      pure $ Tree (CraftItem targetItem targetRate) craftedInputs
