module Polyform.Batteries.Env.Types where

import Prelude

import Data.Array (singleton) as Array
import Data.Map (Map)
import Polyform.Batteries (Dual, Validator, Error) as Batteries
import Polyform.Dual (hoistParser) as Dual
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Polyform.Validator.Dual
import Type.Prelude (SProxy(..))

type Key = String

type Value = String

type Env = Map Key Value

type Errors (errs ∷ # Type) = Array { key ∷ Key, errors ∷ Array (Batteries.Error errs) }

type Validator m (errs ∷ # Type) i o = Polyform.Validator m (Errors errs) i o

_env = SProxy ∷ SProxy "env"

fromValidator ∷ ∀ errs m i. Monad m ⇒ Key → Batteries.Validator m errs i ~> Validator m errs i
fromValidator key = lmapValidator (Array.singleton <<< { key, errors: _ })

type Dual m (errs ∷ # Type) i o = Polyform.Validator.Dual.Dual m (Errors errs) i o

fromDual ∷ ∀ errs i m. Monad m ⇒ Key → Batteries.Dual m errs i ~> Dual m errs i
fromDual name = Dual.hoistParser (fromValidator name)

