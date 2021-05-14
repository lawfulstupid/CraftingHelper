{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CraftingHelper.Main (Recipe, ItemStack) where

import CraftingHelper.Data.ItemStack
import CraftingHelper.Data.Recipe

import AbLib.Control.Parser
import AbLib.Control.ParserUtils
import AbLib.Data.Tree
import AbLib.Data.List

import GHC.IO
import Data.Maybe
import Data.Char
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

type RecipeBook = Map Item [Recipe]
type CraftTree = Tree ItemStack

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


--------------------------------------------------------------------------------

type CraftStage = [ItemStack]
data StagePointer = StagePointer
   { stage :: Int
   , stack :: ItemStack }

craftStages :: Forest ItemStack -> [CraftStage]
craftStages = mapToLayers . pointersToMap . forestToPointers
   where
   insertStagePointer :: StagePointer -> Map String StagePointer -> Map String StagePointer
   insertStagePointer ptr = Map.alter (Just . maybe ptr (mergePointers ptr)) (item $ stack ptr)
   
   mergePointers :: StagePointer -> StagePointer -> StagePointer
   mergePointers (StagePointer stage1 stack1) (StagePointer stage2 stack2) = StagePointer (max stage1 stage2) (stack1 + stack2)
   
   mapToLayers :: Map String StagePointer -> [CraftStage]
   mapToLayers = map (map stack) . groupOn stage . sortOn stage . Map.elems

   pointersToMap :: [StagePointer] -> Map String StagePointer
   pointersToMap = foldr insertStagePointer Map.empty

   forestToPointers :: Forest ItemStack -> [StagePointer]
   forestToPointers [] = []
   forestToPointers forest = let
      thisLayer = map node forest
      nextForest = foldMap branches forest
      nextOutput = map (\ptr -> ptr {stage = stage ptr + 1}) $ forestToPointers nextForest
      in map (StagePointer 0) thisLayer ++ nextOutput
