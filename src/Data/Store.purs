module Data.Store where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Prelude (map, Unit, ($))
import Signal (runSignal, foldp, Signal)
import Signal.Channel (CHANNEL, subscribe, Channel, channel, send)

type StoreEff e = Eff (channel :: CHANNEL | e) Unit

type Store s a e =
  { dispatch  :: a -> StoreEff e
  , subscribe :: (s -> StoreEff e) -> StoreEff e
  }

create :: forall s a e. s -> a -> (a -> s -> s) -> Store s a e
create initialState initialAction update = { dispatch: dispatch, subscribe: subscribe' }
  where
  dispatch   a = send actionChannel a
  subscribe' f = runSignal $ map f storeSignal

  actionChannel :: Channel a
  actionChannel = unsafePerformEff $ channel initialAction

  storeSignal :: Signal s
  storeSignal = foldp update initialState (subscribe actionChannel)
