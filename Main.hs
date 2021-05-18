{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CraftingHelper.Main (Recipe, ItemStack) where

import CraftingHelper.Data

import AbLib.Data.Tree
import AbLib.Data.List
import AbLib.Data.Indexed

import Control.Monad
import Data.List
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

type CraftTree = Tree ItemStack

craft :: (Monad m, RecipeRepo r m) => r -> ItemStack-> m CraftTree
craft book target = do
   recipe <- getRecipe (item target) book
   let targetInputQuantity = \input -> (quantity input) * (quantity target) / (quantity $ output recipe)
   let craftInput = \input -> craft book (input {quantity = targetInputQuantity input})
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

printStages :: Forest ItemStack -> IO ()
printStages forest = forM_ (zip [1..] $ reverse $ craftStages forest) $ \(stage, materials) -> do
   putStrLn ("STAGE " ++ show stage ++ ":")
   forM_ materials print
   putStrLn ""

fullPlan :: RecipeSelector -> [ItemStack] -> IO ()
fullPlan selector targets = do
   forest <- sequence $ map (craft selector) targets
   forest' <- removeTrivialBranches forest
   printStages forest'
   where
   
   removeTrivialBranches :: Forest ItemStack -> IO (Forest ItemStack)
   removeTrivialBranches trees = do
      list <- sequence $ map removeTrivialTree trees
      pure $ catMaybes list
   
   removeTrivialTree :: Tree ItemStack -> IO (Maybe (Tree ItemStack))
   removeTrivialTree (Tree x b) = do
      recipe <- getRecipe (item x) selector
      if null $ inputs recipe
      then pure Nothing
      else do
         b' <- removeTrivialBranches b
         pure $ Just $ Tree x b'
