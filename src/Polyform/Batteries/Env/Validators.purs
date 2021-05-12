-- | You can use `Batteries.Integer.validator` or `Batteries.Number.validator`
-- | directly and other `String` based validators as field value validators.
module Polyform.Batteries.Env.Validators
  ( boolean
  , Field
  , FieldValue
  , MissingValue
  , optional
  , optionalValue
  , Pure
  , required
  , value
  ) where

import Prelude
import Data.Identity (Identity)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Polyform.Batteries (Validator, invalid) as Batteries
import Polyform.Batteries.Env.Types (Env, Key, Value, Validator, fromValidator)
import Polyform.Validator (liftFn, liftFnMV, liftFnV, runValidator)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type Pure e a b
  = Validator Identity e a b

type Field m e b
  = Batteries.Validator m e (Maybe String) b

type FieldValue m e b
  = Batteries.Validator m e String b

_missingValue = Proxy ∷ Proxy "missingValue"

type MissingValue e
  = ( missingValue ∷ Unit | e )

value ∷ ∀ e m. Applicative m ⇒ Field m (MissingValue + e) String
value =
  liftFnV
    $ case _ of
        Just v → pure v
        Nothing → Batteries.invalid _missingValue msg unit
  where
  msg _ = "Missing value"

required ∷
  ∀ a m errs.
  Monad m ⇒
  Key →
  FieldValue m (MissingValue + errs) a →
  Validator m (MissingValue + errs) Env a
required name fieldValidator =
  fromValidator
    name
    (fieldValidator <<< value <<< liftFn (Map.lookup name))

optional ∷
  ∀ a m errs.
  Monad m ⇒
  Key →
  FieldValue m (errs) a →
  Validator m (errs) Env (Maybe a)
optional name fieldValidator = fromValidator name (optionalValue fieldValidator <<< liftFn (Map.lookup name))

optionalValue ∷ ∀ b e m. Monad m ⇒ FieldValue m e b → Field m e (Maybe b)
optionalValue fieldValidator =
  liftFnMV \mv → case mv of
    Nothing → pure (pure Nothing)
    Just v → runValidator (Just <$> fieldValidator) v

-- | Some ad hoc encoding for booleans and arrays.
_booleanExpected = Proxy ∷ Proxy "booleanExpected"

type BooleanExpected e
  = ( booleanExpected ∷ Value | e )

boolean ∷ ∀ e m. Applicative m ⇒ FieldValue m ( booleanExpected ∷ Value | e ) Boolean
boolean =
  liftFnV case _ of
    "true" → pure true
    "false" → pure false
    v → Batteries.invalid _booleanExpected msg v
  where
  msg v = "Boolean expected but got: " <> v
