
module CraftingHelper.Main (Recipe, ItemStack) where

import CraftingHelper.Data.ItemStack
import CraftingHelper.Data.Recipe

import AbLib.Control.Parser
import AbLib.Control.ParserUtils
import AbLib.Data.Tree

import GHC.IO
import Data.Maybe
import Data.Char

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

type RecipeBook = Map Item [Recipe]
type CraftTree = Tree ItemStack

--------------------------------------------------------------------------------

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

getRecipes :: Item -> RecipeBook -> Maybe [Recipe]
getRecipes targetItem book = simplifyOptions <$> Map.lookup targetItem book
   where
   simplifyOptions :: [Recipe] -> [Recipe]
   simplifyOptions rs = let
      zeroCost = filter (null . inputs) rs
      in if null zeroCost then rs else zeroCost

craft :: ItemStack -> RecipeBook -> [CraftTree]
craft target book = case getRecipes (item target) book of
   Nothing -> []
   Just recipes -> do
      recipe <- recipes
      let targetInputQuantity = \input -> (quantity input) * (quantity target) / (quantity $ output recipe)
      let craftInput = \input -> craft (input {quantity = targetInputQuantity input}) book
      let inputTrees = map craftInput $ inputs recipe :: [[CraftTree]]
      craftedInputs <- if null inputTrees then [[]] else sequence inputTrees
      pure $ Tree target craftedInputs
