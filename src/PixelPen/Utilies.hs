module PixelPen.Utilies where

import qualified SDL

getKeycode :: SDL.Event -> Maybe SDL.Keycode
getKeycode e = getKeycode' payload
       where payload = SDL.eventPayload e

getKeycode' :: SDL.EventPayload -> Maybe SDL.Keycode
getKeycode' (SDL.KeyboardEvent ked) = Just $ SDL.keysymKeycode $ SDL.keyboardEventKeysym ked
getKeycode' _ = Nothing
