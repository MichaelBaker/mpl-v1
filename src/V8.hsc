module V8 where

import Mpl.Prelude
import Foreign.Ptr           (Ptr)
import Control.Exception     (bracket)
import Data.Text             (Text)
import Data.Text.Encoding    (encodeUtf8, decodeUtf8)
import Data.ByteString.Char8 (useAsCString, packCString)
import Foreign.C.String      (CString)

data Platform
data Isolate
data Context

foreign import ccall createDefaultPlatform :: IO (Ptr Platform)
foreign import ccall deletePlatform        :: Ptr Platform -> IO ()
foreign import ccall shutdownPlatform      :: IO ()
foreign import ccall initializePlatform    :: Ptr Platform -> IO ()
foreign import ccall initialize            :: IO ()
foreign import ccall newIsolate            :: IO (Ptr Isolate)
foreign import ccall disposeIsolate        :: Ptr Isolate -> IO ()
foreign import ccall evalStringInContext   :: Ptr Isolate -> CString -> IO CString
foreign import ccall freeString            :: CString -> IO ()

withPlatform = bracket createPlatform destroyPlatform
  where createPlatform = do
          platform <- createDefaultPlatform
          initializePlatform(platform)
          return platform

        destroyPlatform platform = do
          shutdownPlatform
          deletePlatform platform

withIsolate = bracket newIsolate disposeIsolate

withContext :: (Ptr Isolate -> IO a) -> IO a
withContext handler = do
  withPlatform $ \_ -> do
    initialize
    withIsolate $ \isolate -> do
      handler isolate

eval :: Ptr Isolate -> Text -> IO Text
eval isolate text = do
  let byteString = encodeUtf8 text
  useAsCString byteString $ \cString -> do
    bracket
      (evalStringInContext isolate cString)
      freeString
      cStringToText

cStringToText cString = do
  byteString <- packCString cString
  let text = decodeUtf8 byteString
  return text
