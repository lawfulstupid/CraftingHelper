{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CraftingHelper.Data.RecipeSelector (RecipeSelector(..)) where

import CraftingHelper.Data.ItemStack
import CraftingHelper.Data.Recipe
import CraftingHelper.Data.RecipeRepo
import CraftingHelper.Data.RecipeBook

import AbLib.Data.List
import AbLib.Data.IO

import Control.Monad
import Data.IORef
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

newtype RecipeSelector = RS RecipeSelector'
type RecipeSelector' = Map Item (IORef RecipeSelectorUnit)
data RecipeSelectorUnit = RecipeSelectorUnit
   { options :: [Recipe]
   , selection :: Maybe Recipe }

--------------------------------------------------------------------------------

instance RecipeRepo RecipeSelector IO where
   make file = do
      book <- make file
      makeRecipeSelector book
   getRecipe item selector = do
      maybeRecipe <- selectRecipe item selector
      guard (isJust maybeRecipe)
      pure $ fromJust maybeRecipe

makeRecipeSelector :: RecipeBook -> IO RecipeSelector
makeRecipeSelector (RB m) = RS <$> (sequenceMap $ Map.map makeUnit m)
   where
   makeUnit :: [Recipe] -> IO (IORef RecipeSelectorUnit)
   makeUnit rs = newIORef $ RecipeSelectorUnit rs Nothing
   
   sequenceMap :: Monad m => Map k (m a) -> m (Map k a)
   sequenceMap m = Map.fromDistinctAscList <$> (sequence $ map sequence $ Map.assocs m)

selectRecipe :: Item -> RecipeSelector -> IO (Maybe Recipe)
selectRecipe = getFromMap
   where
   getFromMap :: Item -> RecipeSelector -> IO (Maybe Recipe)
   getFromMap item (RS m) = case Map.lookup item m of
      Nothing -> pure Nothing
      Just ref -> do
         unit <- readIORef ref
         recipe <- getFromUnit unit
         writeIORef ref $ RecipeSelectorUnit (options unit) recipe
         pure recipe

   getFromUnit :: RecipeSelectorUnit -> IO (Maybe Recipe)
   getFromUnit unit = case selection unit of
      Just r -> pure $ Just r
      Nothing -> getFromOptions (options unit)
      
   getFromOptions :: [Recipe] -> IO (Maybe Recipe)
   getFromOptions = \case
      [] -> pure Nothing
      [r] -> pure (Just r)
      recipes -> getFromUser recipes
   
   getFromUser :: [Recipe] -> IO (Maybe Recipe)
   getFromUser recipes = do
      forM_ (zip [1..] recipes) $ \(n,r) -> putStr ("(" ++ show n ++ ")  ") >> print r
      putStr "Selection: "
      k <- getKey
      let idx = if ('0' <= k && k <= '9') then read [k] - 1 else -1
      case recipes !? idx of
         Just r -> putStrLn (k:"\n") >> pure (Just r)
         Nothing -> putStrLn "invalid, try again\n" >> getFromUser recipes
