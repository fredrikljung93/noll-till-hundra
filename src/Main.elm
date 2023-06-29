module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Html exposing (Html, button, div, input, table, td, text, tr)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode


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
    { guess : String
    , answer : String
    }


type alias Model =
    { cards : Array Card
    }


emptyCard : Card
emptyCard =
    Card (Array.initialize 7 (always emptyQuestion))


emptyQuestion : Question
emptyQuestion =
    Question "" ""


score : Question -> Maybe Int
score question =
    case ( String.toInt question.guess, String.toInt question.answer ) of
        ( Just guessValue, Just answerValue ) ->
            if not (numberInLegalRange guessValue && numberInLegalRange answerValue) then
                Nothing

            else if guessValue == answerValue then
                Just -10

            else
                (guessValue - answerValue) |> Basics.abs |> Just

        _ ->
            Nothing


numberInLegalRange : Int -> Bool
numberInLegalRange n =
    n >= 0 && n <= 100



-- UPDATE


type Msg
    = Guess Int Int String
    | FillAnswer Int Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess cardIndex questionIndex input ->
            let
                maybeCard : Maybe Card
                maybeCard =
                    Array.get cardIndex model.cards

                maybeQuestion : Maybe Question
                maybeQuestion =
                    maybeCard |> Maybe.andThen (\card -> Array.get questionIndex card.questions)
            in
            case ( maybeQuestion, maybeCard ) of
                ( Just question, Just card ) ->
                    let
                        newQuestion : Question
                        newQuestion =
                            { question | guess = input }

                        newCard : Card
                        newCard =
                            { card | questions = Array.set questionIndex newQuestion card.questions }
                    in
                    ( { model | cards = Array.set cardIndex newCard model.cards }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FillAnswer cardIndex questionIndex input ->
            let
                maybeCard : Maybe Card
                maybeCard =
                    Array.get cardIndex model.cards

                maybeQuestion : Maybe Question
                maybeQuestion =
                    maybeCard |> Maybe.andThen (\card -> Array.get questionIndex card.questions)
            in
            case ( maybeQuestion, maybeCard ) of
                ( Just question, Just card ) ->
                    let
                        newQuestion : Question
                        newQuestion =
                            { question | answer = input }

                        newCard : Card
                        newCard =
                            { card | questions = Array.set questionIndex newQuestion card.questions }
                    in
                    ( { model | cards = Array.set cardIndex newCard model.cards }
                    , Cmd.none
                    )

                _ ->
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
        [ value question.guess
        , onInput (Guess cardIndex questionIndex)
        , type_ "number"
        ]
        []


answerInput : Int -> Int -> Question -> Html Msg
answerInput cardIndex questionIndex question =
    input
        [ value question.answer
        , onChange (FillAnswer cardIndex questionIndex)
        ]
        []


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string
