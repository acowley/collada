module Graphics.Formats.Collada (load) where
import Data.Maybe (listToMaybe)
import Graphics.Formats.Collada.Objects
import Text.XML.HXT.Core

-- | Loads a COLLADA file and returns an action that draws the model.
-- This will throw an IO exception if anything went wrong during the
-- process.
load :: FilePath -> IO (Maybe Model)
load f = fmap listToMaybe . runX $ 
         readDocument [] f >>> getChildren >>> parseCollada


