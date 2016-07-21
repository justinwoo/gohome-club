module Models.TH where

import           Control.Lens        hiding (index)
import           Language.Haskell.TH

makeLessStupidLenses :: Name -> DecsQ
makeLessStupidLenses =
  makeLensesWith $ lensRules
    & lensField .~ \_ _ name -> [TopName $ mkName $ nameBase name ++ "L"]
