module Main exposing (main)

import Cons
import Html exposing (Html, button, div, form, h1, input, label, li, option, select, text, textarea)
import Html.Attributes exposing (class, for, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Validation exposing ((*>), (<$>), (<*>), (>>=))
import Validation.Validator exposing (inList, isEmail, maxLength, minLength, notBlank, notInList)


main : Program Never Model Message
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }


type alias Model =
    { formValues : FormValues
    , formErrors :
        { username : List FormError
        , email : List FormError
        , password : List FormError
        , role : List FormError
        , notes : List FormError
        }
    }


type alias FormValues =
    { username : String
    , email : String
    , password : String
    , role : String
    , notes : String
    }


type Message
    = Submit
    | InputName String
    | InputEmail String
    | InputPassword String
    | InputRole String
    | InputNotes String


type FormError
    = IsBlank
    | IsTooShort Int
    | IsTooLong Int
    | BlacklistedUsername
    | InvalidEmail
    | InvalidRole


showFormError : FormError -> String
showFormError e =
    case e of
        IsBlank ->
            "This field cannot be blank."

        IsTooShort n ->
            "Must be at least " ++ toString n ++ " characters."

        IsTooLong n ->
            "Must be at most " ++ toString n ++ " characters."

        BlacklistedUsername ->
            "Username is blacklisted."

        InvalidEmail ->
            "Not a valid email address."

        InvalidRole ->
            "Not a valid role."


initialModel : Model
initialModel =
    { formValues =
        { username = ""
        , email = ""
        , password = ""
        , role = ""
        , notes = ""
        }
    , formErrors =
        { username = []
        , email = []
        , password = []
        , role = []
        , notes = []
        }
    }


update : Message -> Model -> Model
update msg m =
    let
        formErrors =
            m.formErrors

        formValues =
            m.formValues

        validUsername value =
            notBlank IsBlank value
                *> minLength 3 IsTooShort value
                *> maxLength 8 IsTooLong value
                *> notInList BlacklistedUsername [ "root", "wheel" ] value

        validEmail value =
            notBlank IsBlank value
                *> isEmail InvalidEmail value

        validPassword value =
            notBlank IsBlank value
                *> minLength 8 IsTooShort value
                *> maxLength 32 IsTooLong value

        validRole value =
            notBlank IsBlank value
                *> inList InvalidRole [ "user", "admin" ] value

        validNotes value =
            maxLength 140 IsTooLong value

        validForm =
            FormValues
                <$> validUsername m.formValues.username
                <*> validEmail m.formValues.email
                <*> validPassword m.formValues.password
                <*> validRole m.formValues.role
                <*> validNotes m.formValues.notes

        errors =
            Validation.transform Cons.toList (always [])
    in
    case msg of
        Submit ->
            m

        InputName value ->
            { m
                | formValues = { formValues | username = value }
                , formErrors = { formErrors | username = errors (validUsername value) }
            }

        InputEmail value ->
            { m
                | formValues = { formValues | email = value }
                , formErrors = { formErrors | email = errors (validEmail value) }
            }

        InputPassword value ->
            { m
                | formValues = { formValues | password = value }
                , formErrors = { formErrors | password = errors (validPassword value) }
            }

        InputRole value ->
            { m
                | formValues = { formValues | role = value }
                , formErrors = { formErrors | role = errors (validRole value) }
            }

        InputNotes value ->
            { m
                | formValues = { formValues | notes = value }
                , formErrors = { formErrors | notes = errors (validNotes value) }
            }


view : Model -> Html Message
view m =
    div [ class "page" ]
        [ h1 [ class "page-label" ]
            [ text "Validation Demo" ]
        , form
            [ class "form", onSubmit Submit ]
            [ renderField "Username" m.formErrors.username <|
                input [ class "input", type_ "text", onInput InputName, value m.formValues.username ] []
            , renderField "Email" m.formErrors.email <|
                input [ class "input", type_ "email", onInput InputEmail, value m.formValues.email ] []
            , renderField "Password" m.formErrors.password <|
                input [ class "input", type_ "password", onInput InputPassword, value m.formValues.password ] []
            , renderField "Role" m.formErrors.role <|
                select [ class "input", onInput InputRole ]
                    [ option [] []
                    , option [ value "user" ] [ text "User" ]
                    , option [ value "admin" ] [ text "Admin" ]
                    ]
            , renderField "Notes" m.formErrors.notes <|
                textarea [ class "input", onInput InputNotes ] []
            , button [ class "button", type_ "submit" ] [ text "Create User" ]
            ]
        ]


renderField : String -> List FormError -> Html msg -> Html msg
renderField name errors inputHtml =
    div [ class "field-content" ]
        [ div [ class "field" ]
            [ label [ class "label", for name ] [ text name ]
            , inputHtml
            ]
        , div [ class "form_validator-errors" ]
            (List.map renderError errors)
        ]


renderError : FormError -> Html msg
renderError error =
    li [ class "form_validator-error" ] [ text <| showFormError error ]
