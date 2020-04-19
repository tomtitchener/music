
module Main where

import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Driver
import Lily
import Types

main :: IO ()
main =  liftIO $ execStateT (runReaderT (runDriver exComb) initEnv) initState >>= print . show

printLily :: ToLily a => a -> Driver ()
printLily l = liftF $ DoAction (WriteLily l) ()

randomize :: ToLily a => [a] -> Driver [a]
randomize ls = liftF $ DoActionThen (RandomizeList ls) id

exAction :: Driver ()
exAction = printLily (Note C COct QDur Accent Forte False)

exComb :: Driver ()
exComb = randomize [C,D,E,F,G,A,B] >>= mapM_ printLily

