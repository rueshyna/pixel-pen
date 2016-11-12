module PixelPen.Handler where

import qualified SDL
--
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (unless)
--

eventHandle :: (MonadIO m) => [SDL.Event -> Bool] -> (SDL.Event -> m ()) -> SDL.Event -> m Bool
eventHandle fs op e
  | quit = return True
  | otherwise = op e >> return False
       where quit = any (\f -> f e) fs

eventHandle_ :: (MonadIO m) => [SDL.Event -> Bool] -> (a -> SDL.Event -> m a) -> a -> SDL.Event -> m (Bool, a)
eventHandle_ fs op a e
  | quit = return (True, a)
  | otherwise = op a e >>= (\a' -> return (False, a'))
       where quit = any (\f -> f e) fs

update :: (MonadIO m) => (SDL.Event -> m Bool) -> m ()
update h = do
    me <- SDL.pollEvent
    maybe (update h) (\e -> do
          b <- h e
          unless b (update h)) me
    return ()

update_ :: (MonadIO m) => a -> (SDL.Event -> m (Bool, a)) -> m ()
update_ a h = do
    me <- SDL.pollEvent
    maybe (update_ a h) (\e -> do
          (b, a') <- h e
          unless b (update_ a' h)) me

checkDefaultQuit :: [SDL.Event -> Bool]
checkDefaultQuit = [(== SDL.QuitEvent) . SDL.eventPayload]
