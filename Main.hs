{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CraftingHelper.Main (Recipe, ItemStack) where

import CraftingHelper.Data

import AbLib.Data.Tree
import AbLib.Data.List
import AbLib.Data.Indexed

import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

type CraftTree = Tree ItemStack

craft :: (Monad m, RecipeRepo r m) => ItemStack -> r -> m CraftTree
craft target book = do
   recipe <- getRecipe (item target) book
   let targetInputQuantity = \input -> (quantity input) * (quantity target) / (quantity $ output recipe)
   let craftInput = \input -> craft (input {quantity = targetInputQuantity input}) book
   let inputTrees = map craftInput $ inputs recipe
   craftedInputs <- if null inputTrees then pure [] else sequence inputTrees
   pure $ Tree target craftedInputs

--------------------------------------------------------------------------------

type CraftStage = [ItemStack]

craftStages :: Forest ItemStack -> [CraftStage]
craftStages = mapToLayers . pointersToMap . forestToPointers
   where
   insertIndex :: Indexed ItemStack -> Map Item (Indexed ItemStack) -> Map Item (Indexed ItemStack)
   insertIndex idx = Map.alter (Just . maybe idx (mergeIndices max (+) idx)) (item $ value idx)
   
   mapToLayers :: Map Item (Indexed ItemStack) -> [CraftStage]
   mapToLayers = map (map value) . groupOn index . sortOn index . Map.elems

   pointersToMap :: [Indexed ItemStack] -> Map Item (Indexed ItemStack)
   pointersToMap = foldr insertIndex Map.empty

   forestToPointers :: Forest ItemStack -> [Indexed ItemStack]
   forestToPointers [] = []
   forestToPointers forest = let
      thisLayer = map node forest
      nextForest = foldMap branches forest
      nextOutput = map increment $ forestToPointers nextForest
      in map (Index 0) thisLayer ++ nextOutput
