{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.Either
import Test.Hspec
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bs.Lazy
import qualified Data.Json.Types as Types
import qualified Data.Json.Parse as Parse
import qualified Data.Json.Render as Render
import qualified Data.Json.Value as Value
import qualified Records

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parser" $ do
        describe "parses atoms" $ do
            it "null" $ do
                shouldParse Types.Null " null "
                shouldNotParse @Value.Value "Null"
                shouldNotParse @Value.Value "NULL"
            it "booleans" $ do
                shouldParse False "\t\nfalse"
                shouldParse True "true\n\r\t"
                shouldNotParse @Value.Value "True"
                shouldNotParse @Value.Value "False"
            it "invalid symbols" $ do
                shouldNotParse @Value.Value ""
                shouldNotParse @Value.Value "-"
        describe "parses numbers" $ do
            it "integers" $ do
                shouldParse (1 :: Integer) "1"
                shouldParse (-2 :: Integer) "-2"
                shouldParse (10 :: Integer) "1e1"
                shouldParse (123 :: Integer) "12.30e1"
                shouldParse (156 :: Integer) "156.00"
                shouldNotParse @Integer "0.5"
                shouldNotParse @Integer "1e-1"
            it "floating" $ do
                shouldParse (1 :: Double) "1"
                shouldParse (-2 :: Double) "-2"
                shouldParse (1.56e80 :: Double) "1.56e80"
                shouldParse (-1.56e80 :: Double) "-1.56e80"
                shouldParse (1.56e80 :: Double) "1.56e+80"
                shouldParse (1.56e-80 :: Double) "1.56e-80"
                shouldParse (-1.56e-80 :: Double) "-1.56e-80"
            it "malformed numbers" $ do
                shouldNotParse @Value.Value "1.2.3"
                shouldNotParse @Value.Value "-   5"
                shouldNotParse @Value.Value "1f"
                shouldNotParse @Value.Value "1e2.3"
                shouldNotParse @Value.Value "1e2e3"
                shouldNotParse @Value.Value "1.5-"
            it "NaN and infinity" $ do
                {-
                    NaNs never compare equal, even with identical
                bit representations
                -}
                shouldSatisfy
                    (Parse.jsonParse @Double "null")
                    (either (const False) isNaN)
                shouldParse (1/0 :: Double) "\"+inf\""
                shouldParse (-1/0 :: Double) "\"-inf\""
        describe "parses strings" $ do
            it "basic" $ do
                shouldParse
                    ("" :: Types.Ucs16String)
                    "\"\""
                shouldParse
                    ("12ab" :: Types.Ucs16String)
                    "\"12ab\""
                shouldNotParse
                    @Types.Ucs16String
                    "\"12ab"
                shouldNotParse
                    @Types.Ucs16String
                    "\"\n\""
                shouldNotParse
                    @Types.Ucs16String
                    "\"\0\""
            it "UTF-8" $ do
                shouldParse
                    ("123\x44e\x732b\x1f914" :: Types.Ucs16String)
                    "\"123\209\142\231\140\171\240\159\164\148\""
            it "UTF-16 escape sequences" $ do
                shouldParse
                    ("123\x44e\x732b\x1f914" :: Types.Ucs16String)
                    "\"12\\u0033\\u044e\\u732b\\ud83e\\udd14\""
                shouldNotParse
                    @Types.Ucs16String
                    "\"\\u\""
                shouldNotParse
                    @Types.Ucs16String
                    "\"\\u123\""
                shouldNotParse
                    @Types.Ucs16String
                    "\"\\u000g\""
            it "unpaired UTF-16 surrogate code points" $ do
                shouldParse
                    ("\xdd14\xd83e\x1f914\xdd14\xd83e" :: Types.Ucs16String)
                    "\"\\udd14\\ud83e\\ud83e\\udd14\\udd14\\ud83e\""
            it "escape sequences" $ do
                shouldParse
                    ("\"\\/\x08\x0c\x0a\x0d\x09" :: Types.Ucs16String)
                    "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""
            it "decoding UTF-16 into Text" $ do
                shouldParse
                    ("" :: Text.Text)
                    "\"\""
                shouldParse
                    ("12ab" :: Text.Text)
                    "\"12ab\""
                shouldParse
                    ("123\x44e\x732b\x1f914" :: Text.Text)
                    "\"123\209\142\231\140\171\240\159\164\148\""
                shouldParse
                    ("123\x44e\x732b\x1f914" :: Text.Text)
                    "\"12\\u0033\\u044e\\u732b\\ud83e\\udd14\""
                shouldParse
                    ("\"\\/\x08\x0c\x0a\x0d\x09" :: Text.Text)
                    "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""
                shouldNotParse
                    @Text.Text
                    "\"\\udd14\\ud83e\""
        describe "parses arrays" $ do
            it "of numbers" $ do
                shouldParse
                    [1 :: Integer, 2, 10, -100]
                    " [  1  ,  2\n\t, 10  ,   -100  ] "
                shouldParse
                    [1 :: Integer, 2, 10, -100]
                    "[1,2,10,-100]"
                shouldParse
                    [1 :: Integer]
                    "[ 1 ]"
                shouldParse
                    [1 :: Integer]
                    "[1]"
                shouldParse
                    ([] :: [Integer])
                    "[  ]"
                shouldParse
                    ([] :: [Integer])
                    "[]"
            it "heterogeneous" $ do
                shouldParse
                    [Value.Null, Value.String "asd", Value.Number 10]
                    "[null, \"asd\", 10]"
            it "nested heterogeneous" $ do
                shouldParse
                    [ Value.Null
                    , Value.String "asd"
                    , Value.Array $ Seq.fromList
                        [ Value.Array Seq.empty
                        , Value.String "fgh"
                        , Value.Bool True
                        ]
                    , Value.Number 10
                    ]
                    "[null, \"asd\", [[], \"fgh\", true], 10]"
        describe "parses objects" $ do
            it "simple" $ do
                shouldParse
                    ( Value.Object $ Seq.fromList
                        [ ("one", Value.Null)
                        , ("two", Value.String "asd")
                        , ("three", Value.Array $ Seq.fromList
                            [ Value.Array Seq.empty
                            , Value.String "fgh"
                            , Value.Bool True
                            ]
                          )
                        , ("four", Value.Number 10)
                        ]
                    )
                    "{\
                    \    \"one\": null, \
                    \    \"two\": \"asd\", \
                    \    \"three\": [[], \"fgh\", true], \
                    \    \"four\":10 \
                    \}"
            it "nested" $ do
                shouldParse
                    ( Value.Object $ Seq.fromList
                        [ ("one", Value.Null)
                        , ("two", Value.Object $ Seq.fromList
                            [ ("na", Value.String "asd")
                            , ("nb", Value.Object Seq.empty)
                            ]
                          )
                        , ("three", Value.Array $ Seq.fromList
                            [ Value.Array $ Seq.fromList
                                [ Value.Object $ Seq.fromList
                                    [ ("ea", Value.String "eav")
                                    , ("a", Value.String "av")
                                    ]
                                ]
                            , Value.String "fgh"
                            , Value.Bool True
                            ]
                          )
                        , ("four", Value.Number 10)
                        ]
                    )
                    "{\
                    \    \"one\": null, \
                    \    \"two\": {\"na\": \"asd\", \"nb\":{}}, \
                    \    \"three\": [ \
                    \        [{\"ea\":\"eav\", \"a\" : \"av\"}], \
                    \        \"fgh\", \
                    \        true \
                    \    ], \
                    \    \"four\":10 \
                    \}"
            it "with repeated fields" $ do
                shouldParse
                    ( Value.Object $ Seq.fromList
                        [ ("one", Value.Null)
                        , ("one", Value.Array Seq.empty)
                        , ("two", Value.String "asd")
                        , ("four", Value.Number 10)
                        , ("two", Value.Object Seq.empty)
                        , ("one", Value.Bool False)
                        ]
                    )
                    "{\
                    \    \"one\": null, \
                    \    \"one\": [], \
                    \    \"two\": \"asd\", \
                    \    \"four\": 10, \
                    \    \"two\": {}, \
                    \    \"one\": false \
                    \}"
            it "malformed" $ do
                shouldNotParse
                    @Value.Value
                    "{"
                shouldNotParse
                    @Value.Value
                    "{]"
                shouldNotParse
                    @Value.Value
                    "{,\"a\":0}"
                shouldNotParse
                    @Value.Value
                    "{\"a\":0,,\"b\":1}"
                shouldNotParse
                    @Value.Value
                    "{\"a\":0  \"b\":1}"
    describe "Renderer" $ do
        it "renders atoms" $ do
            shouldRender
                Types.Null
                "null"
            shouldRender
                False
                "false"
            shouldRender
                True
                "true"
        describe "renders numbers" $ do
            it "integers" $ do
                shouldRender
                    (1 :: Integer)
                    "1"
                shouldRender
                    (-2 :: Integer)
                    "-2"
                shouldRender
                    (12345 :: Integer)
                    "12345"
            it "floating" $ do
                shouldRender
                    (1 :: Double)
                    "1.0"
                shouldRender
                    (-2 :: Double)
                    "-2.0"
                shouldRender
                    (1.56e80 :: Double)
                    "1.56e80"
                shouldRender
                    (1.56e-80 :: Double)
                    "1.56e-80"
                shouldRender
                    (-1.56e80 :: Double)
                    "-1.56e80"
                shouldRender
                    (-1.56e-80 :: Double)
                    "-1.56e-80"
            it "NaN and infinity" $ do
                shouldRender
                    (0/0 :: Double)
                    "null"
                shouldRender
                    (1/0 :: Double)
                    "\"+inf\""
                shouldRender
                    (-1/0 :: Double)
                    "\"-inf\""
        describe "renders strings" $ do
            it "basic" $ do
                shouldRender
                    ("" :: Text.Text)
                    "\"\""
                shouldRender
                    ("12ab" :: Text.Text)
                    "\"12ab\""
            it "Unicode" $ do
                shouldRender
                    ("123\x44e\x732b\x1f914" :: Text.Text)
                    "\"123\209\142\231\140\171\240\159\164\148\""
            it "unpaired UTF-16 surrogate code points" $ do
                shouldRender
                    ("\xdd14\xd83e\x1f914\xdd14\xd83e" :: Types.Ucs16String)
                    "\"\\udd14\\ud83e\240\159\164\148\\udd14\\ud83e\""
            it "escape sequences" $ do
                shouldRender
                    ("\"\\/\x08\x0c\x0a\x0d\x09" :: Text.Text)
                    "\"\\\"\\\\/\\b\\f\\n\\r\\t\""
                shouldRender
                    ("\x00\x01\x1f" :: Text.Text)
                    "\"\\u0000\\u0001\\u001f\""
        it "renders arrays" $ do
            shouldRenderStructured
                [Value.Null, Value.String "asd", Value.Number 10]
                "[null,\"asd\",10]"
                "[\n\
                \    null,\n\
                \    \"asd\",\n\
                \    10\n\
                \]"
            shouldRenderStructured
                [ Value.Null
                , Value.String "asd"
                , Value.Array $ Seq.fromList
                    [ Value.Array Seq.empty
                    , Value.String "fgh"
                    , Value.Bool True
                    ]
                , Value.Number 10
                ]
                "[null,\"asd\",[[],\"fgh\",true],10]"
                "[\n\
                \    null,\n\
                \    \"asd\",\n\
                \    [\n\
                \        [],\n\
                \        \"fgh\",\n\
                \        true\n\
                \    ],\n\
                \    10\n\
                \]"
        it "renders objects" $ do
            shouldRenderStructured
                ( Value.Object $ Seq.fromList
                    [ ("one", Value.Null)
                    , ("two", Value.String "asd")
                    , ("three", Value.Array $ Seq.fromList
                        [ Value.Array Seq.empty
                        , Value.String "fgh"
                        , Value.Bool True
                        ]
                      )
                    , ("four", Value.Number 10)
                    ]
                )
                "{\"one\":null,\
                \\"two\":\"asd\",\
                \\"three\":[[],\"fgh\",true],\
                \\"four\":10}"
                "{\n\
                \    \"one\": null,\n\
                \    \"two\": \"asd\",\n\
                \    \"three\": [\n\
                \        [],\n\
                \        \"fgh\",\n\
                \        true\n\
                \    ],\n\
                \    \"four\": 10\n\
                \}"
            shouldRenderStructured
                ( Value.Object $ Seq.fromList
                    [ ("one", Value.Null)
                    , ("two", Value.Object $ Seq.fromList
                        [ ("na", Value.String "asd")
                        , ("nb", Value.Object Seq.empty)
                        ]
                      )
                    , ("three", Value.Array $ Seq.fromList
                        [ Value.Array $ Seq.fromList
                            [ Value.Object $ Seq.fromList
                                [ ("ea", Value.String "eav")
                                , ("a", Value.String "av")
                                ]
                            ]
                        , Value.String "fgh"
                        , Value.Bool True
                        ]
                      )
                    , ("four", Value.Number 10)
                    ]
                )
                "{\"one\":null,\
                \\"two\":{\"na\":\"asd\",\"nb\":{}},\
                \\"three\":[[{\"ea\":\"eav\",\"a\":\"av\"}],\"fgh\",true],\
                \\"four\":10}"
                "{\n\
                \    \"one\": null,\n\
                \    \"two\": {\n\
                \        \"na\": \"asd\",\n\
                \        \"nb\": {}\n\
                \    },\n\
                \    \"three\": [\n\
                \        [\n\
                \            {\n\
                \                \"ea\": \"eav\",\n\
                \                \"a\": \"av\"\n\
                \            }\n\
                \        ],\n\
                \        \"fgh\",\n\
                \        true\n\
                \    ],\n\
                \    \"four\": 10\n\
                \}"
            shouldRenderStructured
                ( Value.Object $ Seq.fromList
                    [ ("one", Value.Null)
                    , ("one", Value.Array Seq.empty)
                    , ("two", Value.String "asd")
                    , ("four", Value.Number 10)
                    , ("two", Value.Object Seq.empty)
                    , ("one", Value.Bool False)
                    ]
                )
                "{\"one\":null,\
                \\"one\":[],\
                \\"two\":\"asd\",\
                \\"four\":10,\
                \\"two\":{},\
                \\"one\":false\
                \}"
                "{\n\
                \    \"one\": null,\n\
                \    \"one\": [],\n\
                \    \"two\": \"asd\",\n\
                \    \"four\": 10,\n\
                \    \"two\": {},\n\
                \    \"one\": false\n\
                \}"
    describe "Person record" $ do
        describe "parsing" $ do
            it "all fields, userdata is present" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        (Just 20)
                        Types.Null
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"age\": 20,\
                    \    \"userdata\": null\
                    \}"
                shouldParse
                    ( Records.Person
                        (Records.Uuid 30 40)
                        "Other name"
                        Records.CategoryAdmin
                        (Just 50)
                        [1::Integer, 2, 3]
                    )
                    "{   \"uuid\": [30, 40],\
                    \    \"name\": \"Other name\",\
                    \    \"category\": \"Admin\",\
                    \    \"age\": 50,\
                    \    \"userdata\": [1, 2, 3]\
                    \}"
            it "unknown fields added" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        (Just 20)
                        Types.Null
                    )
                    "{   \"unknown 1\": {},\
                    \    \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"unknown 2\": [10, null, true],\
                    \    \"category\": \"Member\",\
                    \    \"age\": 20,\
                    \    \"userdata\": null,\
                    \    \"unknown 3\": {\"k\":\"v\", \"k2\":\"v2\"}\
                    \}"
            it "order of fields changed" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        (Just 20)
                        Types.Null
                    )
                    "{   \"category\": \"Member\",\
                    \    \"uuid\": [10, 20],\
                    \    \"userdata\": null,\
                    \    \"age\": 20,\
                    \    \"name\": \"Name\"\
                    \}"
            it "age is null" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        Types.Null
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"age\": null,\
                    \    \"userdata\": null\
                    \}"
            it "age is omitted" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        Types.Null
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"userdata\": null\
                    \}"
            it "usedata is Maybe Integer and present" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        (Just (5 :: Integer))
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"userdata\": 5\
                    \}"
            it "usedata is Maybe Integer and is null" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        (Nothing @Integer)
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"userdata\": null\
                    \}"
            it "usedata is Maybe Integer and omitted" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        (Nothing @Integer)
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\"\
                    \}"
            it "usedata is None and is null" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        Types.None
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"userdata\": null\
                    \}"
            it "usedata is None and is omitted" $ do
                shouldParse
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        Types.None
                    )
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\"\
                    \}"
        describe "rejecting malformed" $ do
            it "mandatory field missing" $ do
                shouldNotParse
                    @(Records.Person Value.Value)
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"age\": 20,\
                    \    \"userdata\": null\
                    \}"
            it "mandatory field repeated" $ do
                shouldNotParse
                    @(Records.Person Value.Value)
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"age\": 20,\
                    \    \"uuid\": [30, 40],\
                    \    \"userdata\": null\
                    \}"
            it "optional field repeated" $ do
                shouldNotParse
                    @(Records.Person Value.Value)
                    "{   \"uuid\": [10, 20],\
                    \    \"age\": 20,\
                    \    \"category\": \"Member\",\
                    \    \"age\": null,\
                    \    \"name\": \"Name\",\
                    \    \"userdata\": null\
                    \}"
            it "field value type mismatch" $ do
                shouldNotParse
                    @(Records.Person Value.Value)
                    "{   \"uuid\": \"is a string\",\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"age\": 20,\
                    \    \"userdata\": null\
                    \}"
            it "uuid has too many elements" $ do
                shouldNotParse
                    @(Records.Person Value.Value)
                    "{   \"uuid\": [10, 20, 30],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"age\": 20,\
                    \    \"userdata\": null\
                    \}"
            it "uuid has too few elements" $ do
                shouldNotParse
                    @(Records.Person Value.Value)
                    "{   \"uuid\": [10],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"Member\",\
                    \    \"age\": 20,\
                    \    \"userdata\": null\
                    \}"
            it "invalid enumeration value" $ do
                shouldNotParse
                    @(Records.Person Value.Value)
                    "{   \"uuid\": [10, 20],\
                    \    \"name\": \"Name\",\
                    \    \"category\": \"not a category\",\
                    \    \"age\": 20,\
                    \    \"userdata\": null\
                    \}"
        describe "rendering" $ do
            it "all fields, userdata is present" $ do
                shouldRenderStructured
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        (Just 20)
                        Types.Null
                    )
                    "{\
                    \\"uuid\":[10,20],\
                    \\"name\":\"Name\",\
                    \\"category\":\"Member\",\
                    \\"age\":20,\
                    \\"userdata\":null\
                    \}"
                    "{\n\
                    \    \"uuid\": [\n\
                    \        10,\n\
                    \        20\n\
                    \    ],\n\
                    \    \"name\": \"Name\",\n\
                    \    \"category\": \"Member\",\n\
                    \    \"age\": 20,\n\
                    \    \"userdata\": null\n\
                    \}"
                shouldRenderStructured
                    ( Records.Person
                        (Records.Uuid 30 40)
                        "Other name"
                        Records.CategoryAdmin
                        (Just 50)
                        [1::Integer, 2, 3]
                    )
                    "{\
                    \\"uuid\":[30,40],\
                    \\"name\":\"Other name\",\
                    \\"category\":\"Admin\",\
                    \\"age\":50,\
                    \\"userdata\":[1,2,3]\
                    \}"
                    "{\n\
                    \    \"uuid\": [\n\
                    \        30,\n\
                    \        40\n\
                    \    ],\n\
                    \    \"name\": \"Other name\",\n\
                    \    \"category\": \"Admin\",\n\
                    \    \"age\": 50,\n\
                    \    \"userdata\": [\n\
                    \        1,\n\
                    \        2,\n\
                    \        3\n\
                    \    ]\n\
                    \}"
            it "age is Nothing" $ do
                shouldRenderStructured
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        Types.Null
                    )
                    "{\
                    \\"uuid\":[10,20],\
                    \\"name\":\"Name\",\
                    \\"category\":\"Member\",\
                    \\"userdata\":null\
                    \}"
                    "{\n\
                    \    \"uuid\": [\n\
                    \        10,\n\
                    \        20\n\
                    \    ],\n\
                    \    \"name\": \"Name\",\n\
                    \    \"category\": \"Member\",\n\
                    \    \"userdata\": null\n\
                    \}"
            it "usedata is Maybe Integer and present" $ do
                shouldRenderStructured
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        (Just (5 :: Integer))
                    )
                    "{\
                    \\"uuid\":[10,20],\
                    \\"name\":\"Name\",\
                    \\"category\":\"Member\",\
                    \\"userdata\":5\
                    \}"
                    "{\n\
                    \    \"uuid\": [\n\
                    \        10,\n\
                    \        20\n\
                    \    ],\n\
                    \    \"name\": \"Name\",\n\
                    \    \"category\": \"Member\",\n\
                    \    \"userdata\": 5\n\
                    \}"
            it "usedata is Maybe Integer and is Nothing" $ do
                shouldRenderStructured
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        (Nothing @Integer)
                    )
                    "{\
                    \\"uuid\":[10,20],\
                    \\"name\":\"Name\",\
                    \\"category\":\"Member\"\
                    \}"
                    "{\n\
                    \    \"uuid\": [\n\
                    \        10,\n\
                    \        20\n\
                    \    ],\n\
                    \    \"name\": \"Name\",\n\
                    \    \"category\": \"Member\"\n\
                    \}"
            it "usedata is None" $ do
                shouldRenderStructured
                    ( Records.Person
                        (Records.Uuid 10 20)
                        "Name"
                        Records.CategoryMember
                        Nothing
                        Types.None
                    )
                    "{\
                    \\"uuid\":[10,20],\
                    \\"name\":\"Name\",\
                    \\"category\":\"Member\"\
                    \}"
                    "{\n\
                    \    \"uuid\": [\n\
                    \        10,\n\
                    \        20\n\
                    \    ],\n\
                    \    \"name\": \"Name\",\n\
                    \    \"category\": \"Member\"\n\
                    \}"

shouldParse ::
    (HasCallStack, Parse.Parse a, Eq a, Show a) =>
    a ->
    Bs.ByteString ->
    IO ()
shouldParse x b = (Parse.jsonParse b) `shouldBe` Right x

shouldNotParse ::
    forall a.
    (HasCallStack, Parse.Parse a, Show a) =>
    Bs.ByteString ->
    IO ()
shouldNotParse b = (Parse.jsonParse @a b) `shouldSatisfy` isLeft

shouldRender ::
    (HasCallStack, Render.Render a) =>
    a ->
    Bs.Lazy.ByteString ->
    IO ()
shouldRender x b = do
    shouldRenderStructured x b b

shouldRenderStructured ::
    (HasCallStack, Render.Render a) =>
    a ->
    Bs.Lazy.ByteString ->
    Bs.Lazy.ByteString ->
    IO ()
shouldRenderStructured x bp bs = do
    Render.jsonRender x `shouldBe` bp
    Render.jsonRenderStructured x `shouldBe` bs
