
module Main where

import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Driver
import Types

main :: IO ()
main =  liftIO $ execStateT (runReaderT (runDriver example) initEnv) initState >>= print . show

example :: Driver ()
example = liftF $ DoAction (WriteLily (Note C COct QDur Accent Forte False)) ()
