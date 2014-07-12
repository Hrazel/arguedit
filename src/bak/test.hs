eachsec [] = []
eachsec [h] = [h]
eachsec (h:_:ls) = h:eachsec ls

import Control.Monad.State.Lazy

data ProgramState = PS [Int] -- this is what our state "is"
foo :: Int -> String -> State (Int, ProgramState)
foo = ...

initialState = PS [] 
doSomething = runState (foo 2 "blah") initialState

