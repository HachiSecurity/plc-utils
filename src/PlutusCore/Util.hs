{-# LANGUAGE DeriveFunctor #-}

-- | Utility functions for parsing and deserialising both, typed and untyped,
-- Plutus Core programs.
module PlutusCore.Util (
    -- * Types
    DefaultError,
    UntypedProgram,
    TypedProgram,
    LoadProgramError(..),
    -- * Untyped Plutus Core
    deserialiseUntyped,
    parseUntyped,
    -- * Typed Plutus Core
    deserialiseTyped,
    parseTyped,
    -- * General interface
    loadProgram,
    loadProgramFromFile
) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Except

import Data.Bifunctor
import Data.ByteString.Lazy as BSL

import Flat

import PlutusCore.Core as PLC
import PlutusCore.Error
import PlutusCore.Lexer
import PlutusCore.Parser as PLC
import PlutusCore.Quote

import UntypedPlutusCore as UPLC
import UntypedPlutusCore.Parser as UPLC

-------------------------------------------------------------------------------

-- | The type of PLC parsing errors for the default universe and default
-- built-in functions.
type DefaultError = Error DefaultUni DefaultFun

-- | The type of Untyped PLC programs for the default universe and default
-- built-in functions.
type UntypedProgram = UPLC.Program Name DefaultUni DefaultFun

-- | The type of Typed PLC programs for the default universe and default
-- built-in functions.
type TypedProgram = PLC.Program TyName Name DefaultUni DefaultFun

-- | Represents errors that may arise when parsing or deserialising
-- Plutus Core programs.
data LoadProgramError a
    -- | An error that occurred while deserialising a PLC program.
    = DecodeProgramError DecodeException
    -- | An error that occurred while parsing a PLC program.
    | ParseProgramError (DefaultError a)
    deriving (Eq, Show, Functor)

-------------------------------------------------------------------------------

-- | `deserialiseUntyped` is a type-specialised version of `unflat` for
-- deserialising a `UntypedProgram`.
deserialiseUntyped
  :: BSL.ByteString
  -> Either (LoadProgramError ()) (UntypedProgram ())
deserialiseUntyped = first DecodeProgramError <$> unflat

-- | `parseUntyped` @fileData@ parses an untyped PLC program from @fileData@.
parseUntyped
  :: BSL.ByteString
  -> Either (LoadProgramError AlexPosn) (UntypedProgram AlexPosn)
parseUntyped =
    first ParseProgramError . runQuote . runExceptT . UPLC.parseProgram

-- | `loadUntyped` @deserialise fileData@ parses or deserialises an untyped PLC
-- program from the data given by @fileData@ depending on the value of
-- @deserialise@.
loadUntyped
    :: Bool -> BSL.ByteString
    -> Either (LoadProgramError ()) (UntypedProgram ())
loadUntyped True xs = deserialiseUntyped xs
loadUntyped False xs = bimap void void $ parseUntyped xs

-------------------------------------------------------------------------------

-- | `deserialiseTyped` is a type-specialised version of `unflat` for
-- deserialising a `TypedProgram`.
deserialiseTyped
    :: BSL.ByteString
    -> Either (LoadProgramError ()) (TypedProgram ())
deserialiseTyped = first DecodeProgramError <$> unflat

-- | `parseTyped` @fileData@ parses a typed PLC program from @fileData@.
parseTyped
  :: BSL.ByteString
  -> Either (LoadProgramError AlexPosn) (TypedProgram AlexPosn)
parseTyped =
    first ParseProgramError . runQuote . runExceptT . PLC.parseProgram

-- | `loadTyped` @deserialise fileData@ parses or deserialises a typed PLC
-- program from the data given by @fileData@ depending on the value of
-- @deserialise@ and returns the program with its types erased.
loadTyped
    :: Bool -> BSL.ByteString
    -> Either (LoadProgramError ()) (UntypedProgram ())
loadTyped True = fmap UPLC.eraseProgram . deserialiseTyped
loadTyped False = fmap UPLC.eraseProgram . bimap void void . parseTyped

-------------------------------------------------------------------------------

-- | `loadProgram` @typed deserialise fileData@ parses or deserialises a PLC
-- program from the data given by @fileData@ depending on the value of
-- @deserialise@. The PLC program is assumed to be untyped, unless @typed@ is
-- set to `True`.
loadProgram
    :: Bool -> Bool -> BSL.ByteString
    -> Either (LoadProgramError ()) (UntypedProgram ())
loadProgram True = loadTyped
loadProgram False = loadUntyped

-- | `loadProgramFromFile` @typed deserialise filePath@ loads the contents of
-- the file named by @filePath@ and passes the contents on to `loadProgram`
-- with the options given by @typed@ and @deserialise@.
loadProgramFromFile
    :: Bool -> Bool -> FilePath
    -> IO (Either (LoadProgramError ()) (UntypedProgram ()))
loadProgramFromFile typed deserialise fp =
    loadProgram typed deserialise <$> BSL.readFile fp

-------------------------------------------------------------------------------
