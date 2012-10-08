module String(
  join,
  joinWith,
  reverseJoinWith
) where

import Data.List(intercalate)

newtype ReverseState state value =
  ReverseState {
    runReverseState :: state -> (value, state)
  }

evalReverseState :: b -> ReverseState b a -> a
evalReverseState state function =
  fst (runReverseState function state)

instance Monad (ReverseState state) where
    return value = ReverseState $ (,) value
    ReverseState runner >>= function =
      ReverseState $ \state ->
        let
          (oldValue, oldState) = runner newState
          (newValue, newState) = runReverseState (function oldValue) state
        in
          (newValue, oldState)

newtype State state value =
  State {
    runState :: state -> (value, state)
  }

evalState :: b -> State b a -> a
evalState state function =
  fst (runState function state)

instance Monad (State state) where
  return value = State $ (,) value
  State runner >>= function =
    State $ \state ->
      let
        (oldValue, oldState) = runner state
        (newValue, newState) = runState (function oldValue) oldState
      in
        (newValue, newState)

get :: ReverseState value value
get = ReverseState $ \state -> (state, state)

modify :: (state -> state) -> ReverseState state ()
modify function = ReverseState $ \state -> ((), function state)

getState :: State value value
getState = State $ \state -> (state, state)

modifyState :: (state -> state) -> State state ()
modifyState function = State $ \state -> ((), function state)

join :: [String] -> String
join = concat

joinWith :: String -> [String] -> String
joinWith = intercalate

reverseJoinWith :: [String] -> String -> String
reverseJoinWith [] _ = ""
reverseJoinWith (t : ts) separator = evalState "" $ do
  modifyState (t++)
  _ <- mapM (\element -> modifyState (\text ->
    showString element $ showString separator text
    )) ts
  txt <- getState
  return txt

