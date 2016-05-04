module Data.Store where

import Prelude
import Control.Monad.Eff (foreachE, Eff)
import Control.Monad.State (execState, modify, State)

type Subscriber s = s -> Eff () Unit

newtype Store s a = Store
  { state       :: s
  , updater     :: s -> a -> s
  , subscribers :: Array (Subscriber s)
  }

type StoreState s a = State (Store s a) Unit

subscribe :: forall s a. Subscriber s -> StoreState s a
subscribe f = modify \(Store s) -> Store s { subscribers = s.subscribers ++ [f] } :: Store s a

update :: forall s a. a -> StoreState s a
update a = modify \(Store s) -> Store s { state = s.updater s.state a } :: Store s a

dispatch :: forall s a. Store s a -> a -> Eff () (Store s a)
dispatch s a = do
  let next = execState (update a) s
  case next of
    Store n -> foreachE n.subscribers \sub -> sub n.state
  return next

create :: forall s a. s -> (s -> a -> s) -> Store s a
create s f = Store
  { state       : s
  , updater     : f
  , subscribers : []
  }
