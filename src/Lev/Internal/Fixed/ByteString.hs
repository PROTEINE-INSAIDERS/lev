{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lev.Internal.Fixed.ByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (create, toForeignPtr)
import Data.Data (Proxy (Proxy))
import GHC.TypeLits (KnownNat)
import Lev.Internal.Fixed.Core
import UnliftIO (MonadUnliftIO (withRunInIO), Exception, throwIO)
import Data.Data (Typeable)
import Control.Monad (when)
import Foreign (plusPtr, withForeignPtr)

{-# INLINE encode #-}
encode :: forall m a. (MonadUnliftIO m, SerializeConstraint m 0 a, Serialize m 0 a, KnownNat (SizeOf a)) => a -> m ByteString
encode a = do
  let size = sizeOf (Proxy :: Proxy a)
  withRunInIO $ \run -> create size $ \ptr -> run $ runPoke (poke @m @0 a) ptr

{-
-- | Throws a 'PeekException' about an attempt to read too many bytes.
tooManyBytes :: Int -> Int -> String -> IO void
tooManyBytes needed remaining ty =
    throwIO $ PeekException remaining $ T.pack $
        "Attempted to read too many bytes for " ++
        ty ++
        ". Needed " ++
        show needed ++ ", but only " ++
        show remaining ++ " remain."

(show (typeRep (Proxy :: Proxy a)))
-}

data PeekException = TooManyBytesException deriving (Show, Typeable )

instance Exception PeekException {- where
#if MIN_VERSION_base(4,8,0)
    displayException (PeekException offset msg) =
        "Exception while peeking, " ++
        show offset ++
        " bytes from end: " ++
        T.unpack msg
#endif
-}
decode :: forall m a. (MonadUnliftIO m, SerializeConstraint m 0 a, Serialize m 0 a, KnownNat (SizeOf a)) => ByteString -> m a
decode bs = do 
  let (ptr, off, len) = toForeignPtr bs
      size = sizeOf (Proxy :: Proxy a)
  when (size > len) $ throwIO TooManyBytesException
  withRunInIO $ \run -> withForeignPtr ptr $ \ptr' -> run $ runPeek (peek @m @0) (ptr' `plusPtr` off)
