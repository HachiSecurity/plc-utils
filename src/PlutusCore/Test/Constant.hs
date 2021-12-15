{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module exports utility types and functions for writing PLC tests.
module PlutusCore.Test.Constant (
    Tagged(..)
) where

-------------------------------------------------------------------------------

import Data.ByteString qualified as BS

import PlutusCore.Data
import PlutusCore.Default

-------------------------------------------------------------------------------

-- | This class represents types which can be used as constants in Plutus Core.
-- It feels like it should exist in the plutus library somewhere, but I
-- couldn't find it; if there is something like this there, replace this.
class Tagged a where
    -- | The tag for a constant value of type @a@. Meant to be used with type
    -- applications such as @uniTag \@Integer@.
    uniTag :: DefaultUni (Esc a)

instance (Tagged a, Tagged b) => Tagged (a,b) where
    uniTag = DefaultUniPair (uniTag @a) (uniTag @b)

instance Tagged a => Tagged [a] where
    uniTag = DefaultUniList (uniTag @a)

instance Tagged () where
    uniTag = DefaultUniUnit

instance Tagged Integer where
    uniTag = DefaultUniInteger

instance Tagged Bool where
    uniTag = DefaultUniBool

instance Tagged BS.ByteString where
    uniTag = DefaultUniByteString

instance Tagged Data where
    uniTag = DefaultUniData

-------------------------------------------------------------------------------
