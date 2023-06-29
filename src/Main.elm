module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Html exposing (Html, button, div, input, table, td, text, tr)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


initialModel : Model
initialModel =
    { cards = Array.initialize 3 (always emptyCard)
    }


type alias Card =
    { questions : Array Question
    }


type alias Question =
    { guess : Maybe Int
    , answer : Maybe Int
    }


type alias Model =
    { cards : Array Card
    }


emptyCard : Card
emptyCard =
    Card (Array.initialize 7 (always emptyQuestion))


emptyQuestion : Question
emptyQuestion =
    Question (Just 100) (Just 100)


score : Question -> Maybe Int
score question =
    case ( question.guess, question.answer ) of
        ( Just guessValue, Just answerValue ) ->
            if guessValue == answerValue then
                Just -10

            else
                (guessValue - answerValue) |> Basics.abs |> Just

        _ ->
            Nothing



-- UPDATE


type Msg
    = Guess Int Int String
    | FillAnswer Int Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess cardIndex questionIndex input ->
            ( model, Cmd.none )

        FillAnswer cardIndex questionIndex input ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "0-100"
    , body =
        [ table [] (indexedMap cardView model.cards |> Array.toList |> List.concat)
        ]
    }


cardView : Int -> Card -> List (Html Msg)
cardView cardIndex card =
    indexedMap (questionView cardIndex) card.questions |> Array.toList


questionView : Int -> Int -> Question -> Html Msg
questionView cardIndex questionIndex question =
    let
        cardNumber =
            1 + (questionIndex + cardIndex * 7)
    in
    tr []
        [ td [] [ cardNumber |> String.fromInt |> text ]
        , td [] [ guessInput cardIndex questionIndex question ]
        , td [] [ answerInput cardIndex questionIndex question ]
        , td [] [ score question |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]


guessInput : Int -> Int -> Question -> Html Msg
guessInput cardIndex questionIndex question =
    input
        [ value (question.guess |> Maybe.map String.fromInt |> Maybe.withDefault "")
        , onInput (Guess cardIndex questionIndex)
        ]
        []


answerInput : Int -> Int -> Question -> Html Msg
answerInput cardIndex questionIndex question =
    input
        [ value (question.answer |> Maybe.map String.fromInt |> Maybe.withDefault "")
        , onInput (FillAnswer cardIndex questionIndex)
        ]
        []
