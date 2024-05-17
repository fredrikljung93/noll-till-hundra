module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Html exposing (Html, div, input, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (colspan, type_, value)
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


calculateScore : Question -> Maybe Int
calculateScore question =
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
                        nextGuess : String
                        nextGuess =
                            if String.length input <= 3 then
                                input

                            else
                                question.guess

                        newQuestion : Question
                        newQuestion =
                            { question | guess = nextGuess }

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
                        nextAnswer : String
                        nextAnswer =
                            if String.length input <= 3 then
                                input

                            else
                                question.answer

                        newQuestion : Question
                        newQuestion =
                            { question | answer = nextAnswer }

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
        [ div
            [ Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "flex-direction" "column"
            ]
            [ table
                [ Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "border-collapse" "collapse"
                ]
                [ headerRow
                , tbody [] ((indexedMap cardView model.cards |> Array.toList |> List.concat) ++ [ totalSum model ])
                ]
            ]
        ]
    }


headerRow : Html Msg
headerRow =
    thead [ Html.Attributes.style "font-size" "2em" ]
        [ tr []
            [ th [ colspan 2 ] [ Html.text "Din gissning 0-100" ]
            , th [ colspan 1, Html.Attributes.style "width" numberColumnWidth ] [ Html.text "Facit" ]
            , th [ colspan 1, Html.Attributes.style "width" numberColumnWidth ] [ Html.text "Diff/Poäng" ]
            ]
        ]


numberColumnWidth : String
numberColumnWidth =
    "30%"


cardView : Int -> Card -> List (Html Msg)
cardView cardIndex card =
    (indexedMap (questionView cardIndex) card.questions |> Array.toList)
        ++ [ partlySumView card
           ]


partlySum : Card -> Maybe Int
partlySum card =
    card.questions |> Array.toList |> List.map calculateScore |> convertList |> Maybe.map List.sum


partlySumView : Card -> Html Msg
partlySumView card =
    tr []
        [ td [ Html.Attributes.style "font-size" "3em", Html.Attributes.colspan 3 ] [ text "Delsumma" ]
        , td [ Html.Attributes.style "font-size" "5em", Html.Attributes.style "font-weight" "bold", Html.Attributes.style "text-align" "right" ] [ partlySum card |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]


totalSum : Model -> Html Msg
totalSum model =
    let
        maybeSum : Maybe Int
        maybeSum =
            model.cards |> Array.toList |> List.map partlySum |> convertList |> Maybe.map List.sum
    in
    tr []
        [ td [ colspan 3, Html.Attributes.style "font-size" "5em" ] [ strong [] [ text "Total summa" ] ]
        , td [ colspan 1, Html.Attributes.style "font-size" "5em", Html.Attributes.style "text-align" "right", Html.Attributes.style "width" numberColumnWidth ] [ strong [] [ maybeSum |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ] ]
        ]


questionView : Int -> Int -> Question -> Html Msg
questionView cardIndex questionIndex question =
    let
        cardNumber =
            1 + (questionIndex + cardIndex * 7)

        maybeScore : Maybe Int
        maybeScore =
            calculateScore question

        answerColor =
            maybeScore |> Maybe.map colorForScore |> Maybe.withDefault "white"
    in
    tr []
        [ td [ Html.Attributes.style "font-size" "5em", Html.Attributes.style "text-align" "right", Html.Attributes.style "font-weight" "bold" ] [ cardNumber |> String.fromInt |> text ]
        , td [ Html.Attributes.style "width" numberColumnWidth ] [ numberInput question.guess (Guess cardIndex questionIndex) (cardIndex * 10 + 1) ]
        , td [] [ numberInput question.answer (FillAnswer cardIndex questionIndex) (cardIndex * 10 + 2) ]
        , td [ Html.Attributes.style "font-size" "5em", Html.Attributes.style "text-align" "right", Html.Attributes.style "background-color" answerColor ] [ maybeScore |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]


colorForScore : Int -> String
colorForScore n =
    "green"



-- TODO


numberInput : String -> (String -> Msg) -> Int -> Html Msg
numberInput valueString msg tabIndex =
    input
        [ value valueString
        , onInput msg
        , type_ "number"
        , Html.Attributes.min "0"
        , Html.Attributes.max "100"
        , Html.Attributes.maxlength 3
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "box-sizing" "border-box"
        , Html.Attributes.style "font-size" "5em"
        , Html.Attributes.style "text-align" "right"
        , Html.Attributes.tabindex tabIndex
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
