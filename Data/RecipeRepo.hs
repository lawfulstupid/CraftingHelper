{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module CraftingHelper.Data.RecipeRepo where

import CraftingHelper.Data.ItemStack
import CraftingHelper.Data.Recipe

class RecipeRepo r m | r -> m where
   make :: FilePath -> IO r
   getRecipe :: Item -> r -> m Recipe
