module TyCon where

import GhcPrelude
import Var

data TyCon
data TyConBndrVis

type TyConBinder = VarBndr TyVar TyConBndrVis

isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool
