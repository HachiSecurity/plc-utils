{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module exports type and terms specific to testing Untyped Plutus Core
-- programs.
module PlutusCore.Test.UPLC (
    TestTerm,
    varE,
    lamAbsE,
    lamAbsManyE,
    applyE,
    applyManyE,
    constantE,
    builtinE,
    delayE,
    forceE
) where

-------------------------------------------------------------------------------

import Data.Some

import UntypedPlutusCore

import PlutusCore.Default
import PlutusCore.Test.Constant

-------------------------------------------------------------------------------

-- | Represents Untyped Plutus Core terms using `DefaultUni`, `DefaultFun`,
-- and with no annotations.
type TestTerm = Term Name DefaultUni DefaultFun ()

-- | `varE` @name@ constructs a variable term using @name@.
varE :: Name -> TestTerm
varE = Var ()

-- | `lamAbsE` @name body@ constructs an abstraction term which binds a
-- variable named @name@ and where the body is given by @body@.
lamAbsE :: Name -> TestTerm -> TestTerm
lamAbsE = LamAbs ()

-- | `lamAbsManyE` @names body@ constructs @names@-many abstractions which
-- bind variables named @names@ in order. The inner-most abstraction has
-- the body given by @body@. For example:
--
-- >>> lamAbsManyE [x,y] e
-- LamAbs () x (LamAbs () y e)
lamAbsManyE :: [Name] -> TestTerm -> TestTerm
lamAbsManyE vs b = foldr lamAbsE b vs

-- | `applyE` @funTerm argTerm@ constructs an application of @funTerm@
-- to @argTerm@.
infixl 5 `applyE`
applyE :: TestTerm -> TestTerm -> TestTerm
applyE = Apply ()

-- | `applyManyE` @funTerm argTerms@ applies @funTerm@ to @argTerms@
-- through @argTerms@-many applications. For example:
--
-- >>> applyManyE f [a,b,c]
-- Apply () (Apply () (Apply () f a) b) c
applyManyE :: TestTerm -> [TestTerm] -> TestTerm
applyManyE = foldl applyE

-- | `constantE` @value@ constructs a constant term for @value@, where the
-- type of constant is automatically derived from the Haskell type @a@.
constantE :: forall a. Tagged a => a -> TestTerm
constantE = Constant () . Some . ValueOf (uniTag @a)

-- | `builtinE` @fun@ constructs a @builtin@ term with the built-in
-- function identified by @fun@.
builtinE :: DefaultFun -> TestTerm
builtinE = Builtin ()

-- | `delayE` @term@ constructs a @delay@ term which delays evaluation
-- of @term@.
delayE :: TestTerm -> TestTerm
delayE = Delay ()

-- | `forceE` @term@ constructs a @force@ term which forces the value
-- of @term@.
forceE :: TestTerm -> TestTerm
forceE = Force ()

-------------------------------------------------------------------------------
