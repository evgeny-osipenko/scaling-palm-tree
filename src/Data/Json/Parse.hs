module Data.Json.Parse
    ( jsonParse
    , Parse (..)
    , Grammar.skipValue
    , Grammar.parseNull
    , Grammar.parseBool
    , Grammar.parseArray
    , Grammar.parseObject
    , Grammar.parseNumber
    , Grammar.parseString
    , Atp.Parser
    )
where

import Control.Applicative
import Data.Int
import Data.Word
import qualified Data.Json.Grammar as Grammar
import qualified Data.Json.Types as Types
import qualified Data.Attoparsec.ByteString as Atp
import qualified Data.ByteString as Bs
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Scientific as Scientific

{--}

jsonParse :: forall a. (Parse a) => Bs.ByteString -> Either String a
jsonParse source =
    go (Atp.parse (Grammar.parseJsonFile (parse @a)) source)
  where
    go (Atp.Fail _ ctx msg) = do
        let msg' = maybe msg id (List.stripPrefix "Failed reading: " msg)
        Left ("Failed to parse " <> concat ctx <> ": " <> msg')
    go (Atp.Done _ x) =
        Right x
    go (Atp.Partial cont) = go (cont "")

class Parse a where
    parse :: Atp.Parser a
    parseList :: Atp.Parser [a]
    parseList = do
        buf <-
            Grammar.parseArray id $ \st -> do
                x <- parse @a
                pure (st . (x :))
        pure (buf [])
    defaultParse :: Maybe a
    defaultParse = Nothing

{--}

instance Parse Types.None where
    parse = Grammar.parseNull >> pure Types.None
    defaultParse = Just Types.None

instance Parse Types.Null where
    parse = Grammar.parseNull >> pure Types.Null

instance Parse Bool where
    parse = Grammar.parseBool

{--}

instance Parse Integer where
    parse = do
        x <- Grammar.parseNumber
        if Scientific.base10Exponent x > 1024
            then fail "Integer: Number is too large"
            else case Scientific.floatingOrInteger x :: Either Double Integer of
                Left _ -> fail "Integer: Value is outside of domain"
                Right r -> pure r

parseBoundedIntegral :: (Integral a, Bounded a) => String -> Atp.Parser a
parseBoundedIntegral typeName = do
    s <- Grammar.parseNumber
    case Scientific.toBoundedInteger s of
        Just x -> pure x
        Nothing -> fail $ typeName <> ": Value is outside of domain"

instance Parse Int where
    parse = parseBoundedIntegral "Int"

instance Parse Int8 where
    parse = parseBoundedIntegral "Int8"

instance Parse Int16 where
    parse = parseBoundedIntegral "Int16"

instance Parse Int32 where
    parse = parseBoundedIntegral "Int32"

instance Parse Int64 where
    parse = parseBoundedIntegral "Int64"

instance Parse Word where
    parse = parseBoundedIntegral "Word"

instance Parse Word8 where
    parse = parseBoundedIntegral "Word8"

instance Parse Word16 where
    parse = parseBoundedIntegral "Word16"

instance Parse Word32 where
    parse = parseBoundedIntegral "Word32"

instance Parse Word64 where
    parse = parseBoundedIntegral "Word64"

{--}

parseRealFloat :: (RealFloat a) => Atp.Parser a
parseRealFloat =
    (Atp.string "null" >> pure (0/0))
    <|> (Atp.string "\"+inf\"" >> pure (1/0))
    <|> (Atp.string "\"-inf\"" >> pure (-1/0))
    <|> (Scientific.toRealFloat <$> Grammar.parseNumber)

instance Parse Float where
    parse = parseRealFloat

instance Parse Double where
    parse = parseRealFloat

instance Parse Scientific.Scientific where
    parse = Grammar.parseNumber

{--}

instance Parse Types.Ucs16String where
    parse = Grammar.parseString

instance Parse Text.Text where
    parse = do
        jkey <- Grammar.parseString
        case Types.fromUcs16String jkey of
            Nothing -> fail "Text: Invalid UTF-16"
            Just text -> pure text

instance Parse Char where
    parse = do
        t <- parse @Text.Text
        case Text.uncons t of
            Just (c, "") -> pure c
            _ -> fail "Char: One character expected"
    parseList = Text.unpack <$> parse @Text.Text

{--}

instance (Parse a) => Parse [a] where
    parse = parseList @a

{--}

data TupleParsingState a b c d
    = TupleZero
    | TupleOne a
    | TupleTwo a b

instance (Parse a, Parse b) => Parse (a, b) where
    parse = do
        buf <-
            Grammar.parseArray TupleZero $ \case
                TupleZero ->
                    TupleOne <$> parse @a
                TupleOne a ->
                    TupleTwo a <$> parse @b
                _ ->
                    fail "Tuple2: Too many elements"
        case buf of
            TupleTwo a b -> pure (a, b)
            _ -> fail "Tuple2: Not enough elements"

{--}

instance (Parse a) => Parse (Maybe a) where
    parse =
        (Just <$> parse @a) <|> (Grammar.parseNull >> pure Nothing)
    defaultParse = Just Nothing

instance (Parse a, Parse b) => Parse (Either a b) where
    parse =
        (Left <$> parse @a) <|> (Right <$> parse @b)

instance (Types.FromUcs16 a, Ord a, Parse b) => Parse (Map.Map a b) where
    parse =
        Grammar.parseObject Map.empty $ \jkey m -> do
            case Types.fromUcs16String @a jkey of
                Nothing ->
                    fail "Map: Invalid UTF-16 in member key"
                Just tkey ->
                    case Map.lookup tkey m of
                        Just _ ->
                            fail ("Map: Duplicate key: " <> show jkey)
                        Nothing -> do
                            value <- parse @b
                            pure (Map.insert tkey value m)
