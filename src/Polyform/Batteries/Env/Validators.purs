-- | You can use `Batteries.Integer.validator` or `Batteries.Number.validator`
-- | directly and other `String` based validators as field value validator like:
module Polyform.Batteries.Env.Validators
  ( array
  , arrayOf
  , boolean
  , Field
  , FieldValue
  , MissingValue
  , optional
  , optionalValue
  , required
  , value
  )
  where

import Prelude

import Data.Array (singleton) as Array
import Data.Bifunctor (lmap)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex (split) as String.Regex
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Validation.Semigroup (V)
import Data.Variant (inj)
import Polyform.Batteries (Validator, Errors, invalid) as Batteries
import Polyform.Batteries.Env.Types (Env, Key, Validator, Value, fromValidator)
import Polyform.Validator (liftFn, liftFnMV, liftFnV, lmapValidator, runValidator)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

type Field m e b = Batteries.Validator m e (Maybe String) b
type FieldValue m e b = Batteries.Validator m e String b

_missingValue = SProxy ∷ SProxy "missingValue"

type MissingValue e = (missingValue ∷ Unit | e)

value ∷ ∀ e m. Applicative m ⇒ Field m (MissingValue + e) String
value = liftFnV $ case _ of
    Just v → pure v
    Nothing → Batteries.invalid _missingValue unit

required
  ∷ ∀ a m errs
  . Monad m
  ⇒ Key
  → FieldValue m (MissingValue + errs) a
  → Validator m (MissingValue + errs) Env a
required name fieldValidator =
  fromValidator
    name
    (fieldValidator <<< value <<< liftFn (Map.lookup name))

optional
  ∷ ∀ a m errs
  . Monad m
  ⇒ Key
  → FieldValue m (errs) a
  → Validator m (errs) Env (Maybe a)
optional name fieldValidator =
  fromValidator name (optionalValue fieldValidator <<< liftFn (Map.lookup name))

optionalValue ∷ ∀ b e m. Monad m ⇒ FieldValue m e b → Field m e (Maybe b)
optionalValue fieldValidator = liftFnMV \mv → case mv of
  Nothing → pure (pure Nothing)
  Just v → runValidator (Just <$> fieldValidator) v


-- | Some ad hoc opt in encoding for booleans and arrays.
_booleanExpected = SProxy ∷ SProxy "booleanExpected"

type BooleanExpected e = (booleanExpected ∷ Value | e)

boolean ∷ ∀ e m. Applicative m ⇒ FieldValue m (booleanExpected ∷ Value | e) Boolean
boolean = liftFnV case _ of
  "true" → pure true
  "false" → pure false
  v → Batteries.invalid _booleanExpected v

-- | Source: https://stackoverflow.com/a/35040702/194614
-- | TODO: make separator configurable.
separatorRegex ∷ Regex
separatorRegex = unsafeRegex """(?<!(?<![^\\]\\(?,\\{2}){0,10})\\)""" mempty

array ∷ ∀ e m. Monad m ⇒ FieldValue m e (Array String)
array = liftFn case _ of
  "" → []
  v → String.Regex.split separatorRegex v


_invalidItems = SProxy ∷ SProxy "invalidItems"

type ItemErrors fe = { index ∷ Int, error ∷ Batteries.Errors fe }
type InvalidItems fe e = (invalidItems ∷ Array (ItemErrors fe) | e)

arrayOf
  ∷ ∀ m e fe a
  . Monad m
  ⇒ FieldValue m fe a
  → FieldValue m (InvalidItems fe + e) (Array a)
arrayOf v = array >>> liftFnMV av
  where
    ep idx error = { index:  idx, error }

    -- | Run every validator by prefixing its error path with index.
    -- | Validator results in `m (V e a)` so we need traverse here.
    f ∷ Array String → m (Array (V (ItemErrors fe) a))
    f = traverseWithIndex \idx → runValidator (lmapValidator (ep idx) v)


    av ∷ Array String → m (V (Batteries.Errors (InvalidItems fe + e)) (Array a))
    av = f >>> map liftErrs
      where
      liftErrs ∷ Array (V (ItemErrors fe) a) → V (Batteries.Errors (InvalidItems fe + e)) (Array a)
      liftErrs = map (lmap Array.singleton) >>> sequence >>> lmap (inj _invalidItems >>> Array.singleton)

