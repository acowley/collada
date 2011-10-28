module Graphics.Formats.Collada (load) where
import Graphics.Formats.Collada.Objects
import Text.XML.HXT.Core

-- | Loads a COLLADA file and returns an action that draws the model.
-- This will throw an IO exception if anything went wrong during the
-- process.
load :: FilePath -> IO (Model)
load f = runX $ readDocument [] f >>> parseCollada
