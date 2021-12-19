module Records
    ( Uuid (..)
    , UserCategory (..)
    , Person (..)
    )
where

import Data.Word
import qualified Data.Validation as Validation
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Json.Render as Render
import qualified Data.Json.Parse as Parse

data Uuid = Uuid Word64 Word64
  deriving (Show, Eq)

instance Render.Render Uuid where
    render (Uuid a b) =
        Render.renderArray $
            Render.element a <>
            Render.element b

data UuidParsingState
    = UuidParsingStateZero
    | UuidParsingStateOne Word64
    | UuidParsingStateTwo Word64 Word64

instance Parse.Parse Uuid where
    parse = do
        pst <-
            Parse.parseArray UuidParsingStateZero $ \case
                UuidParsingStateZero ->
                    UuidParsingStateOne <$> Parse.parse
                UuidParsingStateOne a ->
                    UuidParsingStateTwo a <$> Parse.parse
                _ -> fail "Uuid: Too many elements"
        case pst of
            UuidParsingStateTwo a b ->
                pure (Uuid a b)
            _ ->
                fail "Uuid: Not enough elements"

data UserCategory
    = CategoryGuest
    | CategoryMember
    | CategoryModerator
    | CategoryAdmin
  deriving (Show, Eq)

instance Render.Render UserCategory where
    render CategoryGuest = Render.renderString "Guest"
    render CategoryMember = Render.renderString "Member"
    render CategoryModerator = Render.renderString "Moderator"
    render CategoryAdmin = Render.renderString "Admin"

instance Parse.Parse UserCategory where
    parse = do
        s <- Parse.parseString
        case s of
            "Guest" -> pure CategoryGuest
            "Member" -> pure CategoryMember
            "Moderator" -> pure CategoryModerator
            "Admin" -> pure CategoryAdmin
            other -> fail $ "UserCategory: Invalid enum value " <> show other

data Person a
    = Person
        { uuid :: Uuid
        , name :: Text.Text
        , category :: UserCategory
        , age :: Maybe Int
        , userdata :: a
        }
  deriving (Show, Eq)

instance (Render.Render a) => Render.Render (Person a) where
    render p =
        Render.renderObject $
            Render.member "uuid" (uuid p) <>
            Render.member "name" (name p) <>
            Render.member "category" (category p) <>
            Render.member "age" (age p) <>
            Render.member "userdata" (userdata p)

data FieldParsingState a
    = NoValue
    | DefaultValue a
    | ExplicitValue a

initialFieldState :: (Parse.Parse a) => FieldParsingState a
initialFieldState =
    case Parse.defaultParse of
        Nothing -> NoValue
        Just x -> DefaultValue x

validateField ::
    String -> FieldParsingState a -> Validation.Validation String a
validateField e NoValue = Validation.Errors [e]
validateField _ (DefaultValue x) = Validation.Valid x
validateField _ (ExplicitValue x) = Validation.Valid x

data PersonParsingState a
    = PersonParsingState
        { _uuid :: FieldParsingState Uuid
        , _name :: FieldParsingState Text.Text
        , _category :: FieldParsingState UserCategory
        , _age :: FieldParsingState (Maybe Int)
        , _userdata :: FieldParsingState a
        }

updateParsingState ::
    (Parse.Parse a) =>
    String ->
    String ->
    (st -> FieldParsingState a) ->
    (FieldParsingState a -> st -> st) ->
    st ->
    Parse.Parser st
updateParsingState typeName fieldName get set state = do
    case get state of
        ExplicitValue _ ->
            fail $ typeName <> ": Duplicate field: " <> fieldName
        _ -> do
            x <- Parse.parse
            pure (set (ExplicitValue x) state)

instance (Parse.Parse a) => Parse.Parse (Person a) where
    parse = do
        let initialState =
                PersonParsingState
                    initialFieldState
                    initialFieldState
                    initialFieldState
                    initialFieldState
                    initialFieldState
        pst <-
            Parse.parseObject initialState $ \key st -> do
                case key of
                    "uuid" ->
                        updateParsingState
                            "Person"
                            "uuid"
                            _uuid
                            (\x o -> o {_uuid = x})
                            st
                    "name" ->
                        updateParsingState
                            "Person"
                            "name"
                            _name
                            (\x o -> o {_name = x})
                            st
                    "category" ->
                        updateParsingState
                            "Person"
                            "category"
                            _category
                            (\x o -> o {_category = x})
                            st
                    "age" ->
                        updateParsingState
                            "Person"
                            "age"
                            _age
                            (\x o -> o {_age = x})
                            st
                    "userdata" ->
                        updateParsingState
                            "Person"
                            "userdata"
                            _userdata
                            (\x o -> o {_userdata = x})
                            st
                    _ -> do
                        Parse.skipValue
                        pure st
        let eiResult =
                Validation.runValidation $
                    Person
                        <$> validateField "uuid" (_uuid pst)
                        <*> validateField "name" (_name pst)
                        <*> validateField "category" (_category pst)
                        <*> validateField "age" (_age pst)
                        <*> validateField "userdata" (_userdata pst)
        case eiResult of
            Right result -> pure result
            Left missingFields ->
                fail $
                    "Person: Missing fields: " <>
                    List.intercalate "," missingFields
