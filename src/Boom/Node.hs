module Boom.Node where

--import Prelude hiding (FilePath)
import Data.Text as T

data Node = Node{ name :: Text
                , instTemplate :: FilePath
                , volsTemplate :: [FilePath]
                }
          deriving (Eq, Show)
