module PerceptualHash ( fileHash ) where

import           Data.Word             (Word64)
import           Foreign.C.String      (CString, withCString)
import           Foreign.C.Types       (CInt (..))
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek)

foreign import ccall ph_dct_imagehash :: CString -> Ptr Word64 -> IO CInt

fileHash :: FilePath -> IO Word64
fileHash fp = withCString fp $ \cstr ->
    alloca $ \hashPtr -> do
        res <- ph_dct_imagehash cstr hashPtr
        check res
        peek hashPtr

    where check (-1) = error ("Hash of file " ++ fp ++ " failed.")
          check 0    = pure ()
          check _    = error "This should never happen."
