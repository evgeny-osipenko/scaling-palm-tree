module Data.Json.Value
    ( Value (..)
    )
where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as Bs.Lazy.Char8
import qualified Data.Json.Render as Render
import qualified Data.Json.Parse as Parse
import qualified Data.Json.Types as Types
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq

data Value
    = Null
    | Bool Bool
    | Number Scientific.Scientific
    | String Types.Ucs16String
    | Array (Seq.Seq Value)
    | Object (Seq.Seq (Types.Ucs16String, Value))
  deriving (Eq)

instance Show Value where
    showsPrec _ v after =
        "[json|" <> Bs.Lazy.Char8.unpack (Render.jsonRender v) <> "|]" <> after

instance Render.Render Value where
    render Null = Render.renderNull
    render (Bool b) = Render.renderBool b
    render (Number n) = Render.renderScientific n
    render (String s) = Render.renderString s
    render (Array xs) =
        Render.renderArray (foldMap Render.element xs)
    render (Object kvs) =
        Render.renderObject (foldMap (uncurry Render.member) kvs)

instance Parse.Parse Value where
    parse =
        (Parse.parseNull >> pure Null) <|>
        (Bool <$> Parse.parseBool) <|>
        (Number <$> Parse.parseNumber) <|>
        (String <$> Parse.parseString) <|>
        (Array <$> parseValueArray) <|>
        (Object <$> parseValueObject) <|>
        fail "Invalid JSON"
      where
        parseValueArray =
            Parse.parseArray mempty $ \st ->
                fmap
                    (\x -> st Seq.|> x)
                    (Parse.parse @Value)
        parseValueObject =
            Parse.parseObject mempty $ \k st ->
                fmap
                    (\x -> st Seq.|> (k, x))
                    (Parse.parse @Value)
