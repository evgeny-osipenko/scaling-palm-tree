module Data.Json.Types
    ( None (..)
    , Null (..)
    , Ucs16String (..)
    , FromUcs16 (..)
    , ToUcs16 (..)
    )
where

import Data.Bits
import Data.String
import Data.Word
import qualified System.IO.Unsafe
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as Vector.Mutable
import qualified Data.Text.Foreign as Text.Foreign

data None = None
  deriving (Show, Read, Eq, Ord)

data Null = Null
  deriving (Show, Read, Eq, Ord)

newtype Ucs16String = Ucs16String (Vector.Vector Word16)
  deriving (Eq, Ord)

instance IsString Ucs16String where
    fromString = collect 0 []
      where
        collect len ws [] = build len ws
        collect len ws (c : cs)
            | fromEnum c < 0x10000 =
                collect (len + 1) (toEnum (fromEnum c) : ws) cs
            | fromEnum c < 0x110000 = do
                let s = fromEnum c - 0x10000
                let hi = (s `shiftR` 10) + 0xd800
                let lo = (s .&. 0x3ff) + 0xdc00
                collect (len + 2) (toEnum lo : toEnum hi : ws) cs
            | otherwise = error "impossible string"
        build len ws =
            Ucs16String $ Vector.create $ do
                buf <- Vector.Mutable.unsafeNew len
                write buf len ws
                pure buf
        write _ len []
            | len == 0 = pure ()
            | otherwise = error "bad"
        write buf len (w : ws)
            | len > 0 = do
                let pos = len - 1
                Vector.Mutable.unsafeWrite buf pos w
                write buf pos ws
            | otherwise = error "bad"

instance Show Ucs16String where
    showsPrec _ (Ucs16String v) r0 =
        '\"' : chars (Vector.toList v) r0
      where
        chars [] r = '\"' : r
        chars (c : cs) r =
            case c of
                0x09 -> '\\' : 't' : chars cs r
                0x0a -> '\\' : 'n' : chars cs r
                0x0d -> '\\' : 'r' : chars cs r
                0x22 -> '\\' : '"' : chars cs r
                0x5c -> '\\' : '\\' : chars cs r
                _ | 32 <= c && c < 128 -> toEnum (fromEnum c) : chars cs r
                _ -> '\\' : 'u' : hex4 c (chars cs r)
        hex4 c r =
            hexd (c `shiftR` 12) :
            hexd ((c `shiftR` 8) .&. 0xf) :
            hexd ((c `shiftR` 4) .&. 0xf) :
            hexd (c .&. 0xf) :
            r
        hexd c
            | c < 10 = toEnum (fromEnum (c + 48))
            | otherwise = toEnum (fromEnum (c + 87))

{--}

class FromUcs16 a where
    fromUcs16String :: Ucs16String -> Maybe a

instance FromUcs16 Ucs16String where
    fromUcs16String = Just

instance FromUcs16 Text.Text where
    fromUcs16String = tryDecodeUtf16

tryDecodeUtf16 :: Ucs16String -> Maybe Text.Text
tryDecodeUtf16 (Ucs16String src)
    | isValidFrom 0 =
        System.IO.Unsafe.unsafePerformIO $
            Vector.unsafeWith src $ \ptr ->
                Just <$> Text.Foreign.fromPtr ptr (toEnum len)
    | otherwise = Nothing
  where
    len = Vector.length src
    isValidFrom i
        | i >= len = True
        | otherwise = do
            case Vector.unsafeIndex src i of
                wa | wa < 0xd800 -> isValidFrom (i + 1)
                wa | wa < 0xdc00 -> isValidSurrogateFrom (i + 1)
                wa | wa < 0xe000 -> False
                _ | otherwise -> isValidFrom (i + 1)
    isValidSurrogateFrom i
        | i >= len = False
        | otherwise = do
            case Vector.unsafeIndex src i of
                wa | 0xdc00 <= wa && wa < 0xe000 -> isValidFrom (i + 1)
                _ | otherwise -> False

{--}

class ToUcs16 a where
    toUcs16String :: a -> Ucs16String

instance ToUcs16 Ucs16String where
    toUcs16String = id

instance ToUcs16 Text.Text where
    toUcs16String = encodeUtf16

encodeUtf16 :: Text.Text -> Ucs16String
encodeUtf16 text = do
    Ucs16String $ System.IO.Unsafe.unsafePerformIO $ do
        arr <- Vector.Mutable.unsafeNew (Text.Foreign.lengthWord16 text)
        Vector.Mutable.unsafeWith arr $ \ptr -> do
            Text.Foreign.unsafeCopyToPtr text ptr
        Vector.unsafeFreeze arr
