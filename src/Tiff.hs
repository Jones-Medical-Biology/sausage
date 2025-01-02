module Tiff ( parseTiff ) where

import Codec.Picture
import Codec.Picture.Metadata

-- parse the first 8 bytes to get the byte order and the offset to the first IFD
parseHeader :: 

-- follow the offsets to IFDs

--- image data
--- exif metadata
--- xmp metadata
--- custom metadata tags

-- extract metadata tags

-- 0x3C, 0x3F, 0x78, 0x6D, 0x6C
