module Export ()  where

import           Control.Monad    ((<=<))
import           Data.Word        (Word64)
import           Foreign.C.String
import           PerceptualHash

-- | Hash an image at a given filepath
hs_phash :: CString -> IO Word64
hs_phash = fmap go . fileHash <=< peekCString
    where go Left{}    = 0
          go (Right x) = x

foreign export ccall hs_phash :: CString -> IO Word64
