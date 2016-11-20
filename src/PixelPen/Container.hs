{-# LANGUAGE OverloadedStrings #-}
module PixelPen.Container where

import qualified SDL
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)

sdlInitVideo :: (MonadIO m) => (() -> m (), () -> m ())
sdlInitVideo = (const $ SDL.initialize [SDL.InitVideo], const SDL.quit)

window :: (MonadIO m) => ((T.Text, SDL.WindowConfig) -> m SDL.Window, SDL.Window -> m ())
window = (uncurry SDL.createWindow, SDL.destroyWindow)

defaultWindow :: (MonadIO m) => (T.Text -> m SDL.Window, SDL.Window -> m ())
defaultWindow = (flip SDL.createWindow SDL.defaultWindow, SDL.destroyWindow)

surface :: (MonadIO m) => (SDL.Window -> m SDL.Surface, SDL.Surface -> m())
surface = (SDL.getWindowSurface, SDL.freeSurface)

bmpSurface :: (MonadIO m) => (FilePath -> m SDL.Surface, SDL.Surface -> m())
bmpSurface = (SDL.loadBMP, SDL.freeSurface)


optBmpSurface :: (MonadIO m) => ((SDL.Surface, FilePath) -> m SDL.Surface, SDL.Surface -> m())
optBmpSurface = (uncurry optLoadBmpPic, SDL.freeSurface)

-- In fact, SDL converts color mode in every blitting if
-- the color mode of source surface doesn't match
-- the color mode of target surface.
-- To avoid those converting, a simple way is to
-- align their color mode whenever we load an image.
optLoadBmpPic :: (MonadIO m) => SDL.Surface -> FilePath -> m SDL.Surface
optLoadBmpPic sf path = do
   imgSf <- SDL.loadBMP path
   -- get the color mode of given surface
   spf <- SDL.surfaceFormat sf
   -- align the color mode of image surface
   SDL.convertSurface imgSf spf
      <* SDL.freeSurface imgSf
   -- equals to the following lines
   -- optSf <- SDL.convertSurface imgSf spf
   -- SDL.freeSurface imgSf
   -- return optSf

(^.^) :: (MonadIO m) => (a -> m b, b -> m ()) -> a -> (b -> m c) -> m ()
(^.^) (x,y) i f = do
   c <- x i
   f c
   y c

class Loadable l where
  load :: MonadIO m => (a -> m b) -> l a -> m (l b)
  unload :: MonadIO m => (a -> m ()) -> l a -> m ()

(^.^/) :: (Traversable t, Loadable l, MonadIO m)
       => (a -> m b, b -> m ())
       -> t (l a)
       -> (t (l b) -> m d)
       -> m ()
(^.^/) (x,y) i op = do
  c <- sequence $ fmap (load x) i
  op c
  mapM_ (unload y) c

(^.^.) :: (MonadIO m) => (a -> m b, b -> m ()) -> a -> s -> (b -> s -> m d) -> m ()
(^.^.) (x,y) i s f = do
   c <- x i
   f c s
   y c
