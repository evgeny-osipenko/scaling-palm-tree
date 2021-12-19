module Data.Json.Render
    ( jsonRenderGeneralized
    , jsonRender
    , jsonRenderStructured
    , RendererBackend (..)
    , renderNull
    , renderBool
    , renderScientific
    , renderString
    , element
    , member
    , PackedRenderer
    , StructuredRenderer
    , Render (..)
    )
where

import Data.Bits
import Data.Int
import Data.String
import Data.Word
import qualified Data.Json.Types as Types
import qualified Data.ByteString.Lazy as Bs.Lazy
import qualified Data.ByteString.Builder as Builder
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Scientific as Scientific
import qualified Data.ByteString.Builder.Scientific as Scientific
import qualified Data.Vector.Storable as Vector

jsonRenderGeneralized ::
    (RendererBackend r, Render a) =>
    (r -> Builder.Builder) ->
    a ->
    Bs.Lazy.ByteString
jsonRenderGeneralized runRenderer x =
    Builder.toLazyByteString (runRenderer (render x))

jsonRender ::
    (Render a) =>
    a ->
    Bs.Lazy.ByteString
jsonRender =
    jsonRenderGeneralized
        (\(PackedRenderer mbB) -> maybe "null" id mbB)

jsonRenderStructured ::
    (Render a) =>
    a ->
    Bs.Lazy.ByteString
jsonRenderStructured =
    jsonRenderGeneralized
        (\(StructuredRenderer mbBf) -> maybe "null" ($ 0) mbBf)

{--}

class (Monoid (Array r), Monoid (Object r)) => RendererBackend r where
    data Array r
    data Object r
    renderNone :: r
    renderAtom :: Builder.Builder -> r
    renderArray :: Array r -> r
    renderObject :: Object r -> r
    basicElement :: r -> Array r
    basicMember :: Types.Ucs16String -> r -> Object r

renderNull :: (RendererBackend r) => r
renderNull = renderAtom "null"

renderBool :: (RendererBackend r) => Bool -> r
renderBool False = renderAtom "false"
renderBool True = renderAtom "true"

renderScientific :: (RendererBackend r) => Scientific.Scientific -> r
renderScientific n
    | 0 <= e && e < 20 =
        renderAtom (Builder.integerDec (Scientific.coefficient n * 10 ^ e))
    | otherwise =
        renderAtom (Scientific.scientificBuilder n)
  where
    e = Scientific.base10Exponent n

renderString :: (RendererBackend r) => Types.Ucs16String -> r
renderString x = renderAtom ("\"" <> stringContentsBuilder x <> "\"")

element :: (RendererBackend r, Render a) => a -> Array r
element x = basicElement (render x)

member :: (RendererBackend r, Render a) => Types.Ucs16String -> a -> Object r
member k x = basicMember k (render x)

{--}

newtype PackedRenderer = PackedRenderer (Maybe Builder.Builder)

instance RendererBackend PackedRenderer where
    newtype Array PackedRenderer = PackedArray (Maybe Builder.Builder)
    newtype Object PackedRenderer = PackedObject (Maybe Builder.Builder)
    renderNone = PackedRenderer Nothing
    renderAtom b = PackedRenderer (Just b)
    renderArray (PackedArray Nothing) = PackedRenderer (Just "[]")
    renderArray (PackedArray (Just b)) =
        PackedRenderer (Just ("[" <> b <> "]"))
    renderObject (PackedObject Nothing) = PackedRenderer (Just "{}")
    renderObject (PackedObject (Just b)) =
        PackedRenderer (Just ("{" <> b <> "}"))
    basicElement (PackedRenderer Nothing) = PackedArray (Just "null")
    basicElement (PackedRenderer (Just b)) = PackedArray (Just b)
    basicMember _ (PackedRenderer Nothing) = PackedObject Nothing
    basicMember k (PackedRenderer (Just b)) =
        PackedObject (Just ("\"" <> stringContentsBuilder k <> "\":" <> b))

instance Semigroup (Array PackedRenderer) where
    PackedArray Nothing <> PackedArray Nothing =
        PackedArray Nothing
    PackedArray (Just a) <> PackedArray Nothing =
        PackedArray (Just a)
    PackedArray Nothing <> PackedArray (Just b) =
        PackedArray (Just b)
    PackedArray (Just a) <> PackedArray (Just b) =
        PackedArray (Just (a <> "," <> b))

instance Monoid (Array PackedRenderer) where
    mempty = PackedArray Nothing

instance Semigroup (Object PackedRenderer) where
    PackedObject Nothing <> PackedObject Nothing =
        PackedObject Nothing
    PackedObject (Just a) <> PackedObject Nothing =
        PackedObject (Just a)
    PackedObject Nothing <> PackedObject (Just b) =
        PackedObject (Just b)
    PackedObject (Just a) <> PackedObject (Just b) =
        PackedObject (Just (a <> "," <> b))

instance Monoid (Object PackedRenderer) where
    mempty = PackedObject Nothing

{--}

newtype StructuredRenderer =
    StructuredRenderer (Maybe (Int -> Builder.Builder))

indent :: Int -> Builder.Builder
indent depth
    | depth <= 0 = mempty
    | otherwise = indent (depth - 1) <> "    "

instance RendererBackend StructuredRenderer where
    newtype Array StructuredRenderer =
        StructuredArray (Maybe (Int -> Builder.Builder))
    newtype Object StructuredRenderer =
        StructuredObject (Maybe (Int -> Builder.Builder))
    renderNone = StructuredRenderer Nothing
    renderAtom b = StructuredRenderer (Just (\_ -> b))
    renderArray (StructuredArray Nothing) = renderAtom "[]"
    renderArray (StructuredArray (Just bf)) =
        StructuredRenderer (Just arrayStructured)
      where
        arrayStructured depth =
            "[\n" <>
            indent (depth + 1) <> bf (depth + 1) <> "\n"
            <> indent depth <> "]"
    renderObject (StructuredObject Nothing) = renderAtom "{}"
    renderObject (StructuredObject (Just bf)) =
        StructuredRenderer (Just objectStructured)
      where
        objectStructured depth =
            "{\n" <>
            indent (depth + 1) <> bf (depth + 1) <> "\n"
            <> indent depth <> "}"
    basicElement (StructuredRenderer Nothing) =
        StructuredArray (Just (\_ -> "null"))
    basicElement (StructuredRenderer (Just bf)) =
        StructuredArray (Just bf)
    basicMember _ (StructuredRenderer Nothing) = StructuredObject Nothing
    basicMember k (StructuredRenderer (Just bf)) =
        StructuredObject (Just memberStructured)
      where
        memberStructured depth =
            "\"" <> stringContentsBuilder k <> "\": " <> bf depth

joinStructuredBlocks ::
    (Int -> Builder.Builder) ->
    (Int -> Builder.Builder) ->
    (Int -> Builder.Builder)
joinStructuredBlocks ba bb depth =
    ba depth <> ",\n" <>
    indent depth <> bb depth

instance Semigroup (Array StructuredRenderer) where
    StructuredArray Nothing <> StructuredArray Nothing =
        StructuredArray Nothing
    StructuredArray (Just a) <> StructuredArray Nothing =
        StructuredArray (Just a)
    StructuredArray Nothing <> StructuredArray (Just b) =
        StructuredArray (Just b)
    StructuredArray (Just a) <> StructuredArray (Just b) =
        StructuredArray (Just (joinStructuredBlocks a b))

instance Monoid (Array StructuredRenderer) where
    mempty = StructuredArray Nothing

instance Semigroup (Object StructuredRenderer) where
    StructuredObject Nothing <> StructuredObject Nothing =
        StructuredObject Nothing
    StructuredObject (Just a) <> StructuredObject Nothing =
        StructuredObject (Just a)
    StructuredObject Nothing <> StructuredObject (Just b) =
        StructuredObject (Just b)
    StructuredObject (Just a) <> StructuredObject (Just b) =
        StructuredObject (Just (joinStructuredBlocks a b))

instance Monoid (Object StructuredRenderer) where
    mempty = StructuredObject Nothing

{--}

stringContentsBuilder :: Types.Ucs16String -> Builder.Builder
stringContentsBuilder (Types.Ucs16String v) =
    runCBufN (Vector.foldMap ccbuf v)
        (\r -> r)
        (\r w -> r <> "\\u" <> Builder.word16HexFixed w)

ccbuf :: Word16 -> CBuf
ccbuf w
    | w < 0xd800 = CBuf
        { runCBufN = \retN _ ->
            retN (utf8c (fromEnum w))
        , runCBufW = \retN _ prev ->
            retN (escapec (fromEnum prev) <> utf8c (fromEnum w))
        }
    | w < 0xdc00 = CBuf
        { runCBufN = \_ retW ->
            retW mempty w
        , runCBufW = \_ retW prev ->
            retW (escapec (fromEnum prev)) w
        }
    | w < 0xe000 = CBuf
        { runCBufN = \retN _ ->
            retN (escapec (fromEnum w))
        , runCBufW = \retN _ prev -> do
            let i = (fromEnum prev `shiftL `10) + fromEnum w - 0x35fdc00
            retN (utf8c i)
        }
    | otherwise = CBuf
        { runCBufN = \retN _ ->
            retN (utf8c (fromEnum w))
        , runCBufW = \retN _ prev ->
            retN (escapec (fromEnum prev) <> utf8c (fromEnum w))
        }
  where
    utf8c 0x08 = "\\b"
    utf8c 0x09 = "\\t"
    utf8c 0x0a = "\\n"
    utf8c 0x0c = "\\f"
    utf8c 0x0d = "\\r"
    utf8c 0x22 = "\\\""
    utf8c 0x5c = "\\\\"
    utf8c i | i < 0x20 = escapec i
    utf8c i | otherwise = Builder.charUtf8 (toEnum i)
    escapec i = "\\u" <> Builder.word16HexFixed (toEnum i)

data CBuf = CBuf
    { runCBufN ::
        forall q.
        (Builder.Builder -> q) ->
        (Builder.Builder -> Word16 -> q) ->
        q
    , runCBufW ::
        forall q.
        (Builder.Builder -> q) ->
        (Builder.Builder -> Word16 -> q) ->
        Word16 -> q
    }

instance Semigroup CBuf where
    ca <> cb = CBuf
        { runCBufN = \retN retW ->
            runCBufN ca
                ( \ar ->
                    runCBufN cb
                        (\br -> retN $! ar <> br)
                        (\br -> retW $! ar <> br)
                )
                ( \ar ->
                    runCBufW cb
                        (\br -> retN $! ar <> br)
                        (\br -> retW $! ar <> br)
                )
        , runCBufW = \retN retW ->
            runCBufW ca
                ( \ar ->
                    runCBufN cb
                        (\br -> retN $! ar <> br)
                        (\br -> retW $! ar <> br)
                )
                ( \ar ->
                    runCBufW cb
                        (\br -> retN $! ar <> br)
                        (\br -> retW $! ar <> br)
                )
        }

instance Monoid CBuf where
    mempty = CBuf
        { runCBufN = \retN _ -> retN mempty
        , runCBufW = \_ retW -> retW mempty
        }

{--}

class Render a where
    render :: (RendererBackend r) => a -> r
    renderList :: (RendererBackend r) => [a] -> r
    renderList = renderArray . foldMap element

{--}

instance Render Types.None where
    render Types.None = renderNone

instance Render Types.Null where
    render Types.Null = renderNull

instance Render Bool where
    render = renderBool

{--}

instance Render Integer where
    render = renderAtom . Builder.integerDec

instance Render Int where
    render = renderAtom . Builder.intDec

instance Render Int8 where
    render = renderAtom . Builder.int8Dec

instance Render Int16 where
    render = renderAtom . Builder.int16Dec

instance Render Int32 where
    render = renderAtom . Builder.int32Dec

instance Render Int64 where
    render = renderAtom . Builder.int64Dec

instance Render Word where
    render = renderAtom . Builder.wordDec

instance Render Word8 where
    render = renderAtom . Builder.word8Dec

instance Render Word16 where
    render = renderAtom . Builder.word16Dec

instance Render Word32 where
    render = renderAtom . Builder.word32Dec

instance Render Word64 where
    render = renderAtom . Builder.word64Dec

{--}

renderRealFloat ::
    (RendererBackend r, RealFloat a) =>
    (a -> Builder.Builder) ->
    a ->
    r
renderRealFloat e x
    | isNaN x = renderNull
    | isInfinite x =
        if x > 0
            then renderAtom "\"+inf\""
            else renderAtom "\"-inf\""
    | otherwise = renderAtom (e x)

instance Render Float where
    render = renderRealFloat Builder.floatDec

instance Render Double where
    render = renderRealFloat Builder.doubleDec

instance Render Scientific.Scientific where
    render = renderScientific

{--}

instance Render Types.Ucs16String where
    render = renderString

instance Render Text.Text where
    render = renderString . Types.toUcs16String

instance Render Char where
    render = render . Text.singleton
    renderList = renderString . fromString

{--}

instance (Render a) => Render [a] where
    render = renderList @a

{--}

instance (Render a, Render b) => Render (a, b) where
    render (a, b) =
        renderArray (element a <> element b)

{--}

instance (Render a) => Render (Maybe a) where
    render = maybe renderNone render

instance (Render a, Render b) => Render (Either a b) where
    render = either render render

instance (Types.ToUcs16 a, Render b) => Render (Map.Map a b) where
    render m =
        renderObject $
            Map.foldMapWithKey
                (\k x -> member (Types.toUcs16String k) x)
                m
