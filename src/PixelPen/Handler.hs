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

eventHandle_ :: (MonadIO m) => [SDL.Event -> Bool] -> (SDL.Event -> s ->  m s) -> (SDL.Event -> s -> m ()) -> SDL.Event -> s -> m (Bool, s)
eventHandle_ fs upd op e s
  | quit = return (True, s)
  | otherwise = do
                  s' <- upd e s
                  op e s'
                  return (False, s')
       where quit = any (\f -> f e) fs

update :: (MonadIO m) => (SDL.Event -> m Bool) -> m ()
update h = do
    me <- SDL.pollEvent
    maybe (update h) (\e -> do
          b <- h e
          unless b (update h)) me
    return ()

update_ :: (MonadIO m) => s -> (SDL.Event -> s ->  m (Bool, s)) -> m ()
update_ s h = do
    me <- SDL.pollEvent
    maybe (update_ s h) (\e -> do
          (b, s') <- h e s
          unless b (update_ s' h)) me

checkDefaultQuit :: [SDL.Event -> Bool]
checkDefaultQuit = [(== SDL.QuitEvent) . SDL.eventPayload]
