module Data.Json.Grammar
    ( parseJsonFile
    , skipValue
    , parseNull
    , parseBool
    , parseArray
    , parseObject
    , parseNumber
    , parseString
    )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import qualified Data.Json.Types as Types
import qualified Data.Attoparsec.ByteString as Atp
import qualified Data.ByteString as Bs
import qualified Data.Char as Char
import qualified Data.Scientific as Scientific
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as Vector.Mutable

pattern Ord :: (Integral a) => Char -> a
pattern Ord x <- ((Char.chr . fromIntegral) -> x)
  where
    Ord x = fromIntegral (Char.ord x)
{-# COMPLETE Ord :: Word8 #-}
{-# COMPLETE Ord :: Word16 #-}

pattern Chr :: (Integral a) => a -> Char
pattern Chr x <- ((fromIntegral . Char.ord) -> x)
  where
    Chr x = Char.chr (fromIntegral x)
{-# COMPLETE Chr #-}

skipWord8 :: Atp.Parser ()
skipWord8 = Atp.skip (const True)

skipWhitespace :: Atp.Parser ()
skipWhitespace = do
    Atp.skipWhile (\w -> w == 0x09 || w == 0x0a || w == 0x0d || w == 0x20)

skipValue :: Atp.Parser ()
skipValue = do
    mbA <- Atp.peekWord8
    case mbA of
        Just (Ord '[') -> do
            skipWord8
            skipArrayContents
        Just (Ord '{') -> do
            skipWord8
            skipObjectContents
        Just (Ord '"') -> do
            skipWord8
            skipStringContents
        Just a | canBelongToAtom a -> do
            Atp.skipWhile canBelongToAtom
        _ ->
            fail "Value expected"
  where
    canBelongToAtom w
        = (Ord '0' <= w && w <= Ord '9')
        || (Ord 'a' <= w && w <= Ord 'z')
        || (Ord 'A' <= w && w <= Ord 'Z')
        || (w == Ord '.')
        || (w == Ord '-')
        || (w == Ord '+')

parseJsonFile :: Atp.Parser a -> Atp.Parser a
parseJsonFile vp = do
    skipWhitespace
    v <- vp Atp.<?> "$"
    skipWhitespace
    Atp.endOfInput
    pure v

parseNull :: Atp.Parser ()
parseNull =
    void (Atp.string "null")
        <|> fail "Null expected"

parseBool :: Atp.Parser Bool
parseBool =
    (Atp.string "true" >> pure True)
        <|> (Atp.string "false" >> pure False)
        <|> fail "Boolean expected"

parseArray :: s -> (s -> Atp.Parser s) -> Atp.Parser s
parseArray st0 parseElemFunc = do
    mbA <- Atp.peekWord8
    case mbA of
        Just (Ord '[') ->
            skipWord8
        _ ->
            fail "Array expected"
    skipWhitespace
    b <- Atp.peekWord8'
    case b of
        Ord ']' -> do
            skipWord8
            pure st0
        _ -> do
            eatElem (0 :: Int) st0
  where
    eatElem index st1 = do
        st2 <- parseElemFunc st1 Atp.<?> ("[" <> show index <> "]")
        skipWhitespace
        a <- Atp.peekWord8'
        case a of
            Ord ']' -> do
                skipWord8
                pure st2
            Ord ',' -> do
                skipWord8
                skipWhitespace
                eatElem (index + 1) st2
            _ -> do
                fail "Invalid array"

skipArrayContents :: Atp.Parser ()
skipArrayContents = do
    skipWhitespace
    b <- Atp.peekWord8'
    case b of
        Ord ']' -> do
            skipWord8
        _ -> do
            skipElem (0 :: Int)
  where
    skipElem index = do
        skipValue Atp.<?> ("[" <> show index <> "]")
        skipWhitespace
        a <- Atp.peekWord8'
        case a of
            Ord ']' -> do
                skipWord8
            Ord ',' -> do
                skipWord8
                skipWhitespace
                skipElem (index + 1)
            _ -> do
                fail "Invalid array"

parseObject :: s -> (Types.Ucs16String -> s -> Atp.Parser s) -> Atp.Parser s
parseObject st0 parseMemberFunc = do
    mbA <- Atp.peekWord8
    case mbA of
        Just (Ord '{') ->
            skipWord8
        _ ->
            fail "Object expected"
    skipWhitespace
    b <- Atp.peekWord8'
    case b of
        Ord '}' -> do
            skipWord8
            pure st0
        _ -> do
            eatMember st0
  where
    eatMember st1 = do
        a <- Atp.peekWord8'
        unless (a == Ord '"') (fail "Invalid object")
        key <- parseString
        skipWhitespace
        b <- Atp.peekWord8'
        case b of
            Ord ':' -> do
                skipWord8
            _ -> do
                fail "Invalid object"
        skipWhitespace
        st2 <- parseMemberFunc key st1 Atp.<?> ("[" <> show key <> "]")
        skipWhitespace
        c <- Atp.peekWord8'
        case c of
            Ord '}' -> do
                skipWord8
                pure st2
            Ord ',' -> do
                skipWord8
                skipWhitespace
                eatMember st2
            _ -> do
                fail "Invalid object"

skipObjectContents :: Atp.Parser ()
skipObjectContents = do
    skipWhitespace
    b <- Atp.peekWord8'
    case b of
        Ord '}' -> do
            skipWord8
        _ -> do
            skipMember
  where
    skipMember = do
        a <- Atp.peekWord8'
        unless (a == Ord '"') (fail "Invalid object")
        key <- parseString
        skipWhitespace
        b <- Atp.peekWord8'
        case b of
            Ord ':' -> do
                skipWord8
            _ -> do
                fail "Invalid object"
        skipWhitespace
        skipValue Atp.<?> ("[" <> show key <> "]")
        skipWhitespace
        c <- Atp.peekWord8'
        case c of
            Ord '}' -> do
                skipWord8
            Ord ',' -> do
                skipWord8
                skipWhitespace
                skipMember
            _ -> do
                fail "Invalid object"

parseNumber :: Atp.Parser Scientific.Scientific
parseNumber = do
    isPositive <- parseSign
    intPart <- Atp.takeWhile isDigit
    fracPart <-
        (Atp.word8 (Ord '.') >> Atp.takeWhile isDigit)
        <|> pure ""
    when (Bs.null intPart && Bs.null fracPart) badNumber
    let mag = foldDigits (foldDigits 0 intPart) fracPart
    es <- parseExpPart
    checkNextByte
    pure $
        Scientific.scientific
            (applySign isPositive mag)
            (es - Bs.length fracPart)
  where
    parseSign = do
        mbA <- Atp.peekWord8
        case mbA of
            Just a | Ord '0' <= a && a <= Ord '9' -> pure True
            Just (Ord '.') -> pure True
            Just (Ord '-') -> Atp.anyWord8 >> pure False
            Just (Ord '+') -> Atp.anyWord8 >> pure True
            _ -> fail "Number expected"
    parseExpPart = do
        mbA <- Atp.peekWord8
        if mbA == Just (Ord 'e') || mbA == Just (Ord 'E')
            then do
                skipWord8
                isPositive <-
                    (Atp.word8 (Ord '+') >> pure True)
                    <|> (Atp.word8 (Ord '-') >> pure False)
                    <|> pure True
                ds <- Atp.takeWhile isDigit
                when (Bs.null ds) badNumber
                pure (applySign isPositive (foldDigits 0 ds))
            else
                pure 0
    checkNextByte = do
        mbA <- Atp.peekWord8
        let isInvalidNext = case mbA of
                Just a ->
                    (Ord '0' <= a && a <= Ord '9')
                    || (Ord 'a' <= a && a <= Ord 'z')
                    || (Ord 'A' <= a && a <= Ord 'Z')
                    || (a == Ord '-')
                    || (a == Ord '+')
                    || (a == Ord '.')
                Nothing -> False
        when isInvalidNext badNumber
    isDigit w = Ord '0' <= w && w <= Ord '9'
    foldDigits :: (Num a) => a -> Bs.ByteString -> a
    foldDigits = Bs.foldl' (\x d -> x * 10 + (fromIntegral d - 48))
    applySign s v = if s then v else -v
    badNumber = fail "Invalid number"

data PendingUcs16
    = PendingUcs16 {-# UNPACK #-} !Int !UnboxedWord16List
  deriving (Show)

data UnboxedWord16List
    = WC !UnboxedWord16List {-# UNPACK #-} !Word16
    | WN
  deriving (Show)

parseString :: Atp.Parser Types.Ucs16String
parseString = do
    mbA <- Atp.peekWord8
    case mbA of
        Just (Ord '"') -> do
            skipWord8
            buildUcs16String <$> parseStringContents 0 WN
        _ -> fail "String expected"

buildUcs16String :: PendingUcs16 -> Types.Ucs16String
buildUcs16String (PendingUcs16 totalLen buf) = do
    Types.Ucs16String $ Vector.create $ do
        arr <- Vector.Mutable.unsafeNew totalLen
        write arr totalLen buf
        pure arr
  where
    write _ len WN
        | len == 0 = pure ()
        | otherwise = error "invalid PendingUcs16"
    write arr len (WC xs w)
        | len > 0 = do
            let pos = len - 1
            Vector.Mutable.unsafeWrite arr pos w
            write arr pos xs
        | otherwise = error "invalid PendingUcs16"

parseStringContents :: Int -> UnboxedWord16List -> Atp.Parser PendingUcs16
parseStringContents len buf = do
    a <- Atp.peekWord8'
    case a of
        Ord '"' -> do
            skipWord8
            pure (PendingUcs16 len buf)
        Ord '\\' -> do
            skipWord8
            parseStringEscape len buf
        _ | a < 32 -> bad a
        _ | a < 0x80 -> do
            skipWord8
            parseStringContents (len + 1) (WC buf (fromIntegral a))
        _ | a < 0xc0 -> bad a
        _ | a < 0xe0 -> do
            skipWord8
            b <- Atp.anyWord8
            unless (isCont b) (bad b)
            let x
                    = (fromIntegral a `shiftL` 6)
                    + fromIntegral b
                    - 0x3080 :: Word16
            parseStringContents (len + 1) (WC buf x)
        _ | a < 0xf0 -> do
            skipWord8
            b <- Atp.anyWord8
            unless (isCont b) (bad b)
            c <- Atp.anyWord8
            unless (isCont c) (bad c)
            let x
                    = (fromIntegral a `shiftL` 12)
                    + (fromIntegral b `shiftL` 6)
                    + fromIntegral c
                    - 0x2080 :: Word16
            parseStringContents (len + 1) (WC buf x)
        _ | a < 0xf8 -> do
            skipWord8
            b <- Atp.anyWord8
            unless (isCont b) (bad b)
            c <- Atp.anyWord8
            unless (isCont c) (bad c)
            d <- Atp.anyWord8
            unless (isCont d) (bad d)
            let z
                    = (fromIntegral a `shiftL` 18)
                    + (fromIntegral b `shiftL` 12)
                    + (fromIntegral c `shiftL` 6)
                    + fromIntegral d
                    - 0x3c92080 :: Word32
            unless (z < 0x100000) $
                fail ("Invalid code point: " <> showHex (z + 0x10000))
            let zu
                    = fromIntegral (z `shiftR` 10)
                    + 0xd800 :: Word16
            let zl
                    = fromIntegral (z .&. 0x3ff)
                    + 0xdc00 :: Word16
            parseStringContents (len + 2) (WC (WC buf zu) zl)
        _ -> bad a
  where
    bad w = fail ("Invalid symbol in a string literal: " <> show (Chr w))
    isCont w = w >= 0x80 && w < 0xc0

skipStringContents :: Atp.Parser ()
skipStringContents = do
    Atp.skipWhile isStringInternal
    a <- Atp.peekWord8'
    case a of
        Ord '"' -> do
            skipWord8
        Ord '\\' -> do
            skipWord8
            b <- Atp.peekWord8'
            when (b == Ord '"' || b == Ord '\\') skipWord8
            skipStringContents
        _ -> bad a
  where
    isStringInternal (Ord '\\') = False
    isStringInternal (Ord '"') = False
    isStringInternal w = w >= 0x20
    bad w = fail ("Invalid symbol in a string literal: " <> show (Chr w))

parseStringEscape :: Int -> UnboxedWord16List -> Atp.Parser PendingUcs16
parseStringEscape len buf = do
    a <- Atp.peekWord8'
    case a of
        Ord '"' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x22)
        Ord '\\' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x5c)
        Ord '/' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x2f)
        Ord 'b' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x08)
        Ord 'f' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x0c)
        Ord 'n' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x0a)
        Ord 'r' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x0d)
        Ord 't' -> do
            skipWord8
            parseStringContents (len + 1) (WC buf 0x09)
        Ord 'u' -> do
            skipWord8
            b <- hexDigit
            c <- hexDigit
            d <- hexDigit
            e <- hexDigit
            let x
                    = (b `shiftL` 12)
                    + (c `shiftL` 8)
                    + (d `shiftL` 4)
                    + e :: Word16
            parseStringContents (len + 1) (WC buf x)
        _ -> fail ("Invalid escape sequence: " <> show (Chr a))
  where
    hexDigit = do
        x <- Atp.peekWord8'
        case x of
            _ | x >= Ord '0' && x <= Ord '9' ->
                Atp.anyWord8 >> pure (fromIntegral x - 48)
            _ | x >= Ord 'a' && x <= Ord 'f' ->
                Atp.anyWord8 >> pure (fromIntegral x - 87)
            _ | x >= Ord 'A' && x <= Ord 'F' ->
                Atp.anyWord8 >> pure (fromIntegral x - 55)
            _ -> fail ("Invalid hex digit: " <> show (Chr x))

showHex :: (Integral a, Ord a) => a -> String
showHex x
    | x == 0 = "0x0"
    | x < 0 = "-0x" <> reverse (digits (negate x))
    | otherwise = "0x" <> reverse (digits x)
  where
    digits y
        | y <= 0 = ""
        | otherwise = do
            let d = y `mod` 16
            let c = if d < 10 then d + 48 else d + 87
            Char.chr (fromIntegral c) : digits (y `div` 16)
