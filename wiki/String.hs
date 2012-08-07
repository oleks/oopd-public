module String(
  join,
  joinWith
) where

import Text.Show(showString)

newtype ReverseState state value =
  ReverseState {
    runReverseState :: state -> (value, state)
  }

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

get = ReverseState $ \state -> (state, state)
modify function = ReverseState $ \state -> ((), function state)
put = modify . const

getState = State $ \state -> (state, state)
modifyState function = State $ \state -> ((), function state)
putState = modify . const

join :: [String] -> String
join strings = evalReverseState "" $ do
  text <- get
  mapM (\string -> modify (string++)) strings
  return text

joinWith :: [String] -> String -> String
joinWith [] _ = ""
joinWith (head:tail) separator = evalReverseState "" $ do
  text <- get
  modify (head++)
  mapM (\element -> modify (\text ->
    showString separator $ showString element text
    )) tail
  return text

