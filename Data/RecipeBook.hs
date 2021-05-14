
module CraftingHelper.Data.RecipeBook where

import CraftingHelper.Data.ItemStack
import CraftingHelper.Data.Recipe

import AbLib.Control.Parser

import Data.Char
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

type RecipeBook = Map Item [Recipe]

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

getRecipes :: Item -> RecipeBook -> [Recipe]
getRecipes targetItem book = simplifyOptions $ concat $ maybeToList $ Map.lookup targetItem book
   where
   simplifyOptions :: [Recipe] -> [Recipe]
   simplifyOptions rs = let
      zeroCost = filter (null . inputs) rs
      in if null zeroCost then rs else zeroCost
