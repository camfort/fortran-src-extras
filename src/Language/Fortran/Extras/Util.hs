module Language.Fortran.Extras.Util where

import qualified Data.Text as Text
import Data.Text ( Text )

tshow :: Show a => a -> Text
tshow = Text.pack . show

