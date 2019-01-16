module DataCon where

import GhcPrelude
import Var( TyVar, TyCoVar, TyVarBinder )
import Name( Name, NamedThing )
import {-# SOURCE #-} TyCon ( TyConBinder, TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Outputable ( Outputable, OutputableBndr )
import BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep ( Type, ThetaType, KnotTied, Kind )
import CoAxiom ( Role )

data DataCon
data DataConRep
data EqSpec

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConExTyCoVars :: DataCon -> [TyCoVar]
dataConUserTyVars :: DataCon -> [TyVar]
dataConUserTyVarBinders :: DataCon -> [TyVarBinder]
dataConSourceArity  :: DataCon -> Arity
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConInstOrigArgTys  :: DataCon -> [Type] -> [Type]
dataConStupidTheta :: DataCon -> ThetaType
dataConFullSig :: DataCon
               -> ([TyVar], [TyCoVar], [EqSpec], ThetaType, [Type], Type)
isUnboxedSumCon :: DataCon -> Bool

buildSynTyCon :: Name -> [KnotTied TyConBinder] -> Kind
              -> [Role] -> KnotTied Type -> TyCon

instance Eq DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
