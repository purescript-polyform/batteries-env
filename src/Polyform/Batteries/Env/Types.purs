module Polyform.Batteries.Env.Types where

import Prelude

import Data.Array (singleton) as Array
import Data.Map (Map)
import Polyform.Batteries (Dual, Validator, Errors) as Batteries
import Polyform.Dual (hoistParser) as Dual
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Polyform.Validator.Dual
import Type.Prelude (SProxy(..))

type Key = String

type Value = String

type Env = Map Key Value

type Errors :: Row Type -> Type
type Errors errs = Array { key ∷ Key, errors ∷ Batteries.Errors errs }

type Validator :: (Type -> Type) -> Row Type -> Type -> Type -> Type
type Validator m errs i o = Polyform.Validator m (Errors errs) i o

_env = SProxy ∷ SProxy "env"

fromValidator ∷ ∀ errs m i. Monad m ⇒ Key → Batteries.Validator m errs i ~> Validator m errs i
fromValidator key = lmapValidator (Array.singleton <<< { key, errors: _ })

type Dual :: (Type -> Type) -> Row Type -> Type -> Type -> Type
type Dual m errs i o = Polyform.Validator.Dual.Dual m (Errors errs) i o

fromDual ∷ ∀ errs i m. Monad m ⇒ Key → Batteries.Dual m errs i ~> Dual m errs i
fromDual name = Dual.hoistParser (fromValidator name)

