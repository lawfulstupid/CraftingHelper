{-# LANGUAGE MultiParamTypeClasses #-}

module CraftingHelper.Data.RecipeBook (RecipeBook(..)) where

import CraftingHelper.Data.ItemStack
import CraftingHelper.Data.Recipe
import CraftingHelper.Data.RecipeRepo

import AbLib.Control.Parser

import Control.Monad
import Debug.Trace
import Data.Char
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

newtype RecipeBook = RB RecipeBook'
type RecipeBook' = Map Item [Recipe]

--------------------------------------------------------------------------------

instance RecipeRepo RecipeBook [] where
   make = loadRecipes
   getRecipe = getRecipes

loadRecipes :: FilePath -> IO RecipeBook
loadRecipes file = do
   content <- fmap lines $ readFile file
   let recipes = preprocess content >>= parseRecipes
   pure $ RB $ foldr aux Map.empty recipes
   
   where
   parseRecipes :: String -> [Recipe]
   parseRecipes line = do
      let rs = parseM line
      when (null rs) $ traceM ("INVALID RECIPE: " ++ line);
      rs
   
   preprocess :: [String] -> [String]
   preprocess lines = do
      line <- lines
      let trimmedLine = dropWhile isSpace line
      guard (not $ null trimmedLine)
      guard (head trimmedLine /= '#')
      pure trimmedLine
   
   aux :: Recipe -> RecipeBook' -> RecipeBook'
   aux r m = Map.alter (append r) (item $ output r) m
   
   append :: Recipe -> Maybe [Recipe] -> Maybe [Recipe]
   append r rs = Just $ simplifyRecipes (r : fromMaybe [] rs)

   simplifyRecipes :: [Recipe] -> [Recipe]
   simplifyRecipes rs = let
      zeroCost = filter (null . inputs) rs
      in if null zeroCost then rs else zeroCost

getRecipes :: Item -> RecipeBook -> [Recipe]
getRecipes targetItem (RB m) = concat $ maybeToList $ Map.lookup targetItem m
