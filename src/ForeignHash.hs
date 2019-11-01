-- N.B. this is a little faster than the Haskell code
module ForeignHash ( foreignFileHash ) where

import           Data.Coerce           (coerce)
import           Data.Word             (Word64)
import           Foreign.C.String      (CString, withCString)
import           Foreign.C.Types       (CInt (..), CULLong (..))
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek)

foreign import ccall ph_dct_imagehash :: CString -> Ptr CULLong -> IO CInt

-- | Doesn't work with @.gif@ files
--
-- This will throw an exception on failure.
foreignFileHash :: FilePath -> IO Word64
foreignFileHash fp = withCString fp $ \cstr ->
    alloca $ \hashPtr -> do
        res <- ph_dct_imagehash cstr hashPtr
        check res
        coerce <$> peek hashPtr

    where check (-1) = error ("Hash of file " ++ fp ++ " failed.")
          check 0    = pure ()
          check _    = error "This should never happen."
