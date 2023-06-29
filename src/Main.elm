module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Events exposing (onClick)


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
        , td [] [ question.guess |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        , td [] [ question.answer |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        , td [] [ question.answer |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]
