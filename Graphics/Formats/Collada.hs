module Graphics.Formats.Collada (load) where
import Data.Maybe (listToMaybe)
import Graphics.Formats.Collada.Objects
import Text.XML.HXT.Core

import qualified Data.Map as M -- for testing

-- | Loads a COLLADA file and returns an action that draws the model.
-- This will throw an IO exception if anything went wrong during the
-- process.
load :: FilePath -> IO (Maybe Model)
load f = fmap listToMaybe . runX $ 
         readDocument [] f >>> getChildren >>> parseCollada


-- testBase = "/Users/acowley/ros/pr2_common/pr2_description/meshes/base_v0/base.dae"
testBase = "/Users/acowley/ros/stacks/pr2_description/meshes/base_v0/base.dae"
test = load testBase >>= putStrLn . maybe "" (show . aux . modelDict)
  where aux = M.lookup "blinn2-fx" -- !!!
     -- aux = M.lookup "blinn2"
     -- aux = M.lookup "base2_M1K"
     -- aux = M.lookup "base2_M1KShape"