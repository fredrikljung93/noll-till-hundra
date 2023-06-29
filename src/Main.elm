module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


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
    Question Nothing Nothing


initialModel : Model
initialModel =
    { cards = Array.initialize 3 (always emptyCard)
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


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


view : Model -> Browser.Document Msg
view model =
    { title = "0-100"
    , body =
        [ div [] (indexedMap cardView model.cards |> Array.toList)
        ]
    }


cardView : Int -> Card -> Html Msg
cardView cardIndex card =
    div []
        (indexedMap (questionView cardIndex) card.questions |> Array.toList)


questionView : Int -> Int -> Question -> Html Msg
questionView cardIndex questionIndex question =
    let
        cardNumber =
            1 + (questionIndex + cardIndex * 7)
    in
    div []
        [ cardNumber |> String.fromInt |> text
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
