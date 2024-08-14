module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Css
import Html.Styled exposing (Html, div, input, strong, table, tbody, td, text, th, thead, tr)
import Html.Styled.Attributes as Attributes exposing (colspan, type_, value)
import Html.Styled.Events as Events exposing (onInput)
import String exposing (toInt)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = document
        , update = update
        , subscriptions = always Sub.none
        }


document : Model -> Browser.Document Msg
document model =
    { title = "Noll till hundra"
    , body =
        [ Html.Styled.toUnstyled <| styledView model
        ]
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


isLegalNumber : String -> Bool
isLegalNumber s =
    String.toInt s |> Maybe.map numberInLegalRange |> Maybe.withDefault False



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


styledView : Model -> Html Msg
styledView model =
    div
        [ Attributes.css
            [ Css.height (Css.vh 100)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.flexDirection Css.column
            ]
        ]
        [ table
            [ Attributes.css
                [ Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.borderCollapse Css.collapse
                ]
            ]
            [ headerRow
            , tbody [] ((indexedMap cardView model.cards |> Array.toList |> List.concat) ++ [ totalSum model ])
            ]
        ]


headerRow : Html Msg
headerRow =
    thead [ Attributes.css [ Css.fontSize (Css.em 2) ] ]
        [ tr []
            [ th [ colspan 2 ] [ text "Din gissning 0-100" ]
            , th
                [ colspan 1
                , Attributes.css [ Css.width numberColumnWidth ]
                ]
                [ text "Facit" ]
            , th
                [ colspan 1
                , Attributes.css [ Css.width numberColumnWidth ]
                ]
                [ text "Diff/Poäng" ]
            ]
        ]


numberColumnWidth : Css.Pct
numberColumnWidth =
    Css.pct 30


cardView : Int -> Card -> List (Html Msg)
cardView cardIndex card =
    (indexedMap (questionView cardIndex) card.questions |> Array.toList)
        ++ [ partlySumView card
           ]


partlySum : Card -> Maybe Int
partlySum card =
    card.questions
        |> Array.toList
        |> List.map calculateScore
        |> convertList
        |> Maybe.map List.sum


partlySumView : Card -> Html Msg
partlySumView card =
    tr []
        [ td
            [ Attributes.css [ Css.fontSize (Css.em 5) ]
            , Attributes.colspan 3
            ]
            [ text "Delsumma" ]
        , td
            [ Attributes.css [ Css.fontSize (Css.em 5), Css.textAlign Css.right ]
            ]
            [ partlySum card |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]


totalSum : Model -> Html Msg
totalSum model =
    let
        maybeSum : Maybe Int
        maybeSum =
            model.cards
                |> Array.toList
                |> List.map partlySum
                |> convertList
                |> Maybe.map List.sum
    in
    tr []
        [ td
            [ colspan 3
            , Attributes.css [ Css.fontSize (Css.em 5) ]
            ]
            [ strong [] [ text "Total summa" ] ]
        , td
            [ colspan 1
            , Attributes.css
                [ Css.fontSize (Css.em 5)
                , Css.textAlign Css.right
                , Css.width numberColumnWidth
                ]
            ]
            [ strong []
                [ maybeSum
                    |> Maybe.map String.fromInt
                    |> Maybe.withDefault ""
                    |> text
                ]
            ]
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
            maybeScore |> Maybe.map colorForScore |> Maybe.withDefault (Css.rgb 0 0 0)
    in
    tr []
        [ td
            [ Attributes.css
                [ Css.fontSize (Css.em 5)
                , Css.textAlign Css.right
                , Css.fontWeight Css.bold
                ]
            ]
            [ cardNumber |> String.fromInt |> text ]
        , td [ Attributes.css [ Css.width numberColumnWidth ] ]
            [ numberInput question.guess (Guess cardIndex questionIndex) (cardIndex * 10 + 1)
            ]
        , td [] [ numberInput question.answer (FillAnswer cardIndex questionIndex) (cardIndex * 10 + 2) ]
        , td
            [ Attributes.css
                [ Css.fontSize (Css.em 5)
                , Css.textAlign Css.right
                , Css.color answerColor
                , Css.textShadow4 (Css.px 1) (Css.px 0) (Css.px 0) (Css.rgb 0 0 0)
                ]
            ]
            [ maybeScore |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        ]


colorForScore n =
    let
        clamp min max val =
            if val < min then
                min

            else if val > max then
                max

            else
                val

        nClamped =
            clamp 0 100 n

        red : Int
        red =
            if nClamped <= 50 then
                round (255 * (toFloat nClamped / 50))

            else
                255

        green : Int
        green =
            if nClamped <= 50 then
                255

            else
                round (255 * (1 - (toFloat (nClamped - 50) / 50)))

        blue : Int
        blue =
            0
    in
    if n == -10 then
        Css.rgba 0 180 0 100

    else
        Css.rgba red green blue 0.7


numberInput : String -> (String -> Msg) -> Int -> Html Msg
numberInput valueString msg tabIndex =
    let
        textColor =
            if valueString == "" || isLegalNumber valueString then
                Css.rgb 0 0 0

            else
                Css.rgb 220 0 0
    in
    input
        [ value valueString
        , onInput msg
        , type_ "number"
        , Attributes.min "0"
        , Attributes.max "100"
        , Attributes.maxlength 3
        , Attributes.css
            [ Css.width (Css.pct 100)
            , Css.boxSizing Css.borderBox
            , Css.fontSize (Css.em 5)
            , Css.textAlign Css.right
            , Css.color textColor
            ]
        , Attributes.tabindex tabIndex
        ]
        []


convertList : List (Maybe Int) -> Maybe (List Int)
convertList maybeList =
    case maybeList of
        [] ->
            Just []

        maybeValue :: rest ->
            maybeValue
                |> Maybe.andThen
                    (\value ->
                        convertList rest
                            |> Maybe.map (\convertedList -> value :: convertedList)
                    )
