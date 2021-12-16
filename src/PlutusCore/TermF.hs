{-# LANGUAGE DeriveFunctor #-}

-- | Modular representations of the PLC abstract syntax.
module PlutusCore.TermF (
    -- | * Constants
    DefaultConstantF(..),
    DefaultConstant,
    Constant(..),
    -- | * Common terms
    TermF(..),
    -- | * Untyped terms
    UntypedF(..),
    UntypedTermF,
    convertUPLC,
    -- | * Typed terms
    TypedF(..),
    TypedTermF,
    convertPLC
) where


-------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Fix
import Data.Functor.Sum
import Data.Text (Text)

import PlutusCore.Core qualified as PLC
import PlutusCore.Data (Data)
import PlutusCore.Default
import UntypedPlutusCore qualified as UPLC
import Data.Proxy

-------------------------------------------------------------------------------

-- | Represents default PLC constants.
data DefaultConstantF r
    = IntConst Integer
    | BoolConst Bool
    | TextConst Text
    | ByteStringConst ByteString
    | DataConst Data
    | UnitConst
    | ListConst [r]
    | PairConst r r

type DefaultConstant = Fix DefaultConstantF

class Constant a where
    constant :: a -> DefaultConstant

instance Constant Integer where
    constant = Fix . IntConst

instance Constant Bool where
    constant = Fix . BoolConst

instance Constant () where
    constant = const $ Fix UnitConst

instance Constant Text where
    constant = Fix . TextConst

instance Constant ByteString where
    constant = Fix . ByteStringConst

instance Constant a => Constant [a] where
    constant = Fix . ListConst . map constant

instance (Constant a, Constant b) => Constant (a,b) where
    constant (a,b) = Fix $ PairConst (constant a) (constant b)

instance Constant Data where
    constant = Fix . DataConst

-------------------------------------------------------------------------------

-- | Represents terms shared between variants of Plutus Core.
data TermF name const ann r
    = Var !ann !name
    | LamAbs !ann !name !r
    | Apply !ann !r !r
    | Constant !ann !const
    | Builtin !ann !DefaultFun
    | Error !ann
    deriving (Eq, Show, Functor)

-------------------------------------------------------------------------------

-- | Represents terms exclusive to Untyped Plutus Core.
data UntypedF ann r
    = Force !ann !r
    | Delay !ann !r
    deriving (Eq, Show, Functor)

-- | Represents terms of Untyped Plutus Core.
type UntypedTermF name const ann = Sum (TermF name const ann) (UntypedF ann)

-- | `convertUPLC` @term@ converts an Untyped Plutus Core @term@ to an
-- equivalent representation that is the sum of `TermF` and `UntypedF`.
convertUPLC
    :: UPLC.Term name DefaultUni DefaultFun ann
    -> Fix (UntypedTermF name DefaultConstant ann)
convertUPLC (UPLC.Var ann name) = Fix $ InL $ Var ann name
convertUPLC (UPLC.LamAbs ann name body) =
    Fix $ InL $ LamAbs ann name (convertUPLC body)
convertUPLC (UPLC.Apply ann fun arg) =
    Fix $ InL $ Apply ann (convertUPLC fun) (convertUPLC arg)
convertUPLC (UPLC.Builtin ann fun) = Fix $ InL $ Builtin ann fun
convertUPLC (UPLC.Constant ann (Some (ValueOf tag x))) =
    Fix $ InL $ Constant ann $ bring (Proxy :: Proxy Constant) tag (constant x)
convertUPLC (UPLC.Error ann) = Fix $ InL $ Error ann
convertUPLC (UPLC.Force ann term) = Fix $ InR $ Force ann (convertUPLC term)
convertUPLC (UPLC.Delay ann term) = Fix $ InR $ Delay ann (convertUPLC term)

-------------------------------------------------------------------------------

-- | Represents terms exclusive to Typed Plutus Core.
data TypedF tyname ann r
    = TyAbs ann tyname (PLC.Kind ann) r
    | TyInst ann r (PLC.Type tyname DefaultUni ann)
    | Unwrap ann r
    | IWrap ann (PLC.Type tyname DefaultUni ann) (PLC.Type tyname DefaultUni ann) r
    deriving (Show, Functor)

-- | Represents terms of Typed Plutus Core.
type TypedTermF tyname name const ann =
    ( Sum (TermF name const (Maybe (PLC.Type tyname DefaultUni ann), ann))
          (TypedF tyname ann)
    )

-- | `convertPLC` @term@ converts a Typed Plutus Core @term@ to an
-- equivalent representation that is the sum of `TermF` and `TypedF`.
convertPLC
    :: PLC.Term tyname name DefaultUni DefaultFun ann
    -> Fix (TypedTermF tyname name DefaultConstant ann)
convertPLC (PLC.Var ann name) = Fix $ InL $ Var (Nothing, ann) name
convertPLC (PLC.LamAbs ann name ty body) =
    Fix $ InL $ LamAbs (Just ty, ann) name (convertPLC body)
convertPLC (PLC.Apply ann fun arg) =
    Fix $ InL $ Apply (Nothing, ann) (convertPLC fun) (convertPLC arg)
convertPLC (PLC.Builtin ann fun) = Fix $ InL $ Builtin (Nothing, ann) fun
convertPLC (PLC.Constant ann (Some (ValueOf tag x))) =
    Fix $ InL $ Constant (Nothing, ann) $
        bring (Proxy :: Proxy Constant) tag (constant x)
convertPLC (PLC.Error ann ty) = Fix $ InL $ Error (Just ty, ann)
convertPLC (PLC.TyAbs ann ty k term) =
    Fix $ InR $ TyAbs ann ty k (convertPLC term)
convertPLC (PLC.TyInst ann term ty) =
    Fix $ InR $ TyInst ann (convertPLC term) ty
convertPLC (PLC.Unwrap ann term) =
    Fix $ InR $ Unwrap ann (convertPLC term)
convertPLC (PLC.IWrap ann ty0 ty1 term) =
    Fix $ InR $ IWrap ann ty0 ty1 (convertPLC term)

-------------------------------------------------------------------------------
