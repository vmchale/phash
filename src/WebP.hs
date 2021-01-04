module WebP ( readWebP
            ) where

import           Codec.Picture.WebP (decodeRgb8)
import qualified Data.ByteString    as BS

-- readWebP :: FilePath -> Image RSS RGB Word8
readWebP = fmap decodeRgb8 . BS.readFile
