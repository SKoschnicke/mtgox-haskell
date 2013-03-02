module Connection.WebSocket
(
frame,
unframe
)
where

import qualified Data.ByteString.Lazy.Char8 as LC

-- | Adds a websocket frame.
frame :: LC.ByteString -> LC.ByteString
frame = ((flip LC.snoc) '\xff') . (LC.cons '\x00')

-- | Removes a websocket frame.
unframe :: LC.ByteString -> LC.ByteString
unframe = LC.init . LC.tail
