module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Html exposing (Html, input, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


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
        [ table []
            [ headerRow
            , tbody [] (indexedMap cardView model.cards |> Array.toList |> List.concat)
            ]
        , totalSum model
        ]
    }


headerRow : Html Msg
headerRow =
    thead []
        [ tr []
            [ th [] [ Html.text "" ]
            , th [] [ Html.text "Din gissning 0-100" ]
            , th [] [ Html.text "Facit" ]
            , th [] [ Html.text "Diff/Poäng" ]
            ]
        ]


cardView : Int -> Card -> List (Html Msg)
cardView cardIndex card =
    (indexedMap (questionView cardIndex) card.questions |> Array.toList)
        ++ [ partlySumView card
           ]


partlySum : Card -> Maybe Int
partlySum card =
    card.questions |> Array.toList |> List.map score |> convertList |> Maybe.map List.sum


partlySumView : Card -> Html Msg
partlySumView card =
    tr []
        [ td [] []
        , td [] [ text "Delsumma" ]
        , td [] [ partlySum card |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]


totalSum : Model -> Html Msg
totalSum model =
    let
        maybeSum : Maybe Int
        maybeSum =
            model.cards |> Array.toList |> List.map partlySum |> convertList |> Maybe.map List.sum
    in
    table []
        [ tr []
            [ td [] [ strong [] [ text "Total summa" ] ]
            , td [] [ strong [] [ maybeSum |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ] ]
            ]
        ]


questionView : Int -> Int -> Question -> Html Msg
questionView cardIndex questionIndex question =
    let
        cardNumber =
            1 + (questionIndex + cardIndex * 7)
    in
    tr []
        [ td [] [ cardNumber |> String.fromInt |> text ]
        , td [] [ numberInput question.guess (Guess cardIndex questionIndex) ]
        , td [] [ numberInput question.answer (FillAnswer cardIndex questionIndex) ]
        , td [] [ score question |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput valueString msg =
    input
        [ value valueString
        , onInput msg
        , type_ "number"
        , Html.Attributes.min "0"
        , Html.Attributes.max "100"
        ]
        []


convertList : List (Maybe Int) -> Maybe (List Int)
convertList maybeList =
    case maybeList of
        [] ->
            Just []

        maybeValue :: rest ->
            case maybeValue of
                Just value ->
                    case convertList rest of
                        Just convertedList ->
                            Just (value :: convertedList)

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
