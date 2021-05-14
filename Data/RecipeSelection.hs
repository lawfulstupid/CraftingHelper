
module CraftingHelper.Data.RecipeSelection where

import CraftingHelper.Data.ItemStack
import CraftingHelper.Data.Recipe
import CraftingHelper.Data.RecipeBook

import AbLib.Data.List
import AbLib.Data.IO

import Control.Monad

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

type RecipeSelection = Map Item (IO Recipe)

--------------------------------------------------------------------------------

makeRecipeSelection :: RecipeBook -> RecipeSelection
makeRecipeSelection = Map.map selectRecipe
   where
   selectRecipe :: [Recipe] -> IO Recipe
   selectRecipe rs | length rs == 1 = pure (head rs)
   selectRecipe rs = do
      forM_ (zip [1..] rs) $ \(n,r) -> do
         putStr "("
         putStr (show n)
         putStr ")  "
         print r
      putStr "Selection: "
      k <- read . pure <$> getKey
      case rs !? (k - 1) of
         Just r -> pure r
         Nothing -> do {putStrLn "invalid, try again\n" >> selectRecipe rs}
