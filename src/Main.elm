module Main exposing (main)

import Array exposing (Array, indexedMap)
import Browser
import Css
import Css.Global
import Html.Styled exposing (Html, div, input, strong, table, tbody, td, text, th, thead, tr)
import Html.Styled.Attributes as Attributes exposing (colspan, type_, value)
import Html.Styled.Events exposing (onInput)


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
    let
        themeProperties =
            propertiesForTheme model.theme
    in
    { title = "Noll till hundra"
    , body =
        [ Html.Styled.toUnstyled <| styledView model themeProperties
        , Html.Styled.toUnstyled <| bodyStyles themeProperties
        ]
    }


bodyStyles : ThemeProperties -> Html msg
bodyStyles themeProperties =
    Css.Global.global
        [ Css.Global.body
            [ Css.backgroundColor themeProperties.backgroundColor
            , Css.color themeProperties.textColor
            ]
        ]



-- THEMES


propertiesForTheme : Theme -> ThemeProperties
propertiesForTheme theme =
    case theme of
        Light ->
            lightTheme

        Dark ->
            darkTheme


type Theme
    = Light
    | Dark


type alias ThemeProperties =
    { backgroundColor : Css.Color
    , primaryColor : Css.Color
    , textColor : Css.Color
    , errorTextColor : Css.Color
    }


lightTheme : ThemeProperties
lightTheme =
    { backgroundColor = Css.rgb 255 255 255
    , primaryColor = Css.rgb 220 220 220
    , textColor = Css.rgb 0 0 0
    , errorTextColor = Css.rgb 220 0 0
    }


darkTheme : ThemeProperties
darkTheme =
    { backgroundColor = Css.rgb 0 0 0
    , primaryColor = Css.rgb 50 50 50
    , textColor = Css.rgb 200 200 200
    , errorTextColor = Css.rgb 220 0 0
    }



-- MODEL


initialModel : Model
initialModel =
    { cards = Array.initialize 3 (always emptyCard)
    , theme = Dark
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
    , theme : Theme
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


styledView : Model -> ThemeProperties -> Html Msg
styledView model themeProperties =
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
                , Css.maxWidth (Css.pct 100)
                ]
            ]
            [ headerRow themeProperties
            , tbody [] ((indexedMap (cardView themeProperties) model.cards |> Array.toList |> List.concat) ++ [ totalSum model ])
            ]
        ]


headerRow : ThemeProperties -> Html Msg
headerRow themeProperties =
    thead
        []
        [ tr
            [ Attributes.css
                [ Css.minHeight (Css.em 4)
                , Css.height (Css.em 4)
                , Css.maxHeight (Css.em 4)
                , Css.backgroundColor themeProperties.primaryColor
                , Css.fontSize (Css.em 2)
                ]
            ]
            [ th
                [ Attributes.css
                    [ cardNumberWidth
                    , Css.displayFlex
                    , Css.justifyContent Css.center
                    , Css.alignItems Css.center
                    , Css.height (Css.pct 100)
                    ]
                ]
                [ burgerMenuIcon themeProperties
                ]
            , th [ Attributes.css [ guessInputWidth ] ]
                [ text "Ditt svar 0-100"
                ]
            , th
                [ colspan 1
                , Attributes.css [ correctAnswerWidth ]
                ]
                [ text "Facit" ]
            , th
                [ colspan 1
                , Attributes.css [ diffWidth ]
                ]
                [ text "Diff/Poäng" ]
            ]
        ]


burgerMenuIcon : ThemeProperties -> Html msg
burgerMenuIcon themeProperties =
    let
        lineColor =
            themeProperties.textColor

        lineWidth =
            Css.rem 2.5

        lineThickness =
            Css.rem 0.3
    in
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.center
            , Css.cursor Css.pointer
            , Css.width lineWidth
            , Css.height (Css.rem 3)
            , Css.marginLeft (Css.rem 1)
            , Css.marginRight (Css.rem 1)
            ]
        , Attributes.id "burgermenu"
        ]
        [ div [ Attributes.css [ Css.backgroundColor lineColor, Css.height lineThickness, Css.marginBottom (Css.px 5) ] ] []
        , div [ Attributes.css [ Css.backgroundColor lineColor, Css.height lineThickness, Css.marginBottom (Css.px 5) ] ] []
        , div [ Attributes.css [ Css.backgroundColor lineColor, Css.height lineThickness ] ] []
        ]


cardNumberWidth : Css.Style
cardNumberWidth =
    Css.width (Css.vw 7)


guessInputWidth : Css.Style
guessInputWidth =
    numberColumnWidth


correctAnswerWidth : Css.Style
correctAnswerWidth =
    numberColumnWidth


diffWidth : Css.Style
diffWidth =
    numberColumnWidth


numberColumnWidth : Css.Style
numberColumnWidth =
    Css.width (Css.vw 31)


cardView : ThemeProperties -> Int -> Card -> List (Html Msg)
cardView themeProperties cardIndex card =
    (indexedMap (questionView themeProperties cardIndex) card.questions |> Array.toList)
        ++ [ partlySumView themeProperties card
           ]


partlySum : Card -> Maybe Int
partlySum card =
    card.questions
        |> Array.toList
        |> List.map calculateScore
        |> convertList
        |> Maybe.map List.sum


partlySumView : ThemeProperties -> Card -> Html Msg
partlySumView themeProperties card =
    tr
        [ Attributes.css
            [ Css.minHeight (Css.em 7)
            , Css.height (Css.em 7)
            , Css.maxHeight (Css.em 7)
            , Css.backgroundColor themeProperties.primaryColor
            ]
        ]
        [ td
            [ Attributes.css [ Css.fontSize (Css.em 2), Css.textAlign Css.right ]
            , Attributes.colspan 3
            ]
            [ text "Delsumma" ]
        , td
            [ Attributes.css [ Css.fontSize (Css.em 5), Css.textAlign Css.right ]
            ]
            [ partlySum card |> Maybe.map String.fromInt |> Maybe.withDefault " " |> text ]
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
    tr
        [ Attributes.css [ Css.minHeight (Css.em 7), Css.height (Css.em 7) ]
        ]
        [ td
            [ colspan 3
            , Attributes.css [ Css.fontSize (Css.em 3), Css.textAlign Css.right ]
            ]
            [ strong [] [ text "Total summa" ] ]
        , td
            [ colspan 1
            , Attributes.css
                [ Css.fontSize (Css.em 5)
                , Css.textAlign Css.right
                , diffWidth
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


backgroundColorForCard : ThemeProperties -> Int -> Css.Color
backgroundColorForCard themeProperties cardNumber =
    if
        (cardNumber >= 9 && cardNumber < 14 && modBy 2 cardNumber == 1)
            || ((cardNumber < 8 || cardNumber > 14) && modBy 2 cardNumber == 0)
    then
        themeProperties.primaryColor

    else
        themeProperties.backgroundColor


questionView : ThemeProperties -> Int -> Int -> Question -> Html Msg
questionView themeProperties cardIndex questionIndex question =
    let
        cardNumber : Int
        cardNumber =
            1 + (questionIndex + cardIndex * 7)

        maybeScore : Maybe Int
        maybeScore =
            calculateScore question

        answerColor =
            maybeScore |> Maybe.map colorForScore |> Maybe.withDefault (Css.rgb 0 0 0)
    in
    tr [ Attributes.css [ Css.backgroundColor (backgroundColorForCard themeProperties cardNumber) ] ]
        [ td
            [ Attributes.css
                [ Css.fontSize (Css.em 5)
                , Css.textAlign Css.right
                , Css.fontWeight Css.bold
                , cardNumberWidth
                ]
            ]
            [ cardNumber |> String.fromInt |> text ]
        , td [ Attributes.css [ guessInputWidth ] ]
            [ numberInput themeProperties question.guess (Guess cardIndex questionIndex) (cardIndex * 10 + 1)
            ]
        , td [] [ numberInput themeProperties question.answer (FillAnswer cardIndex questionIndex) (cardIndex * 10 + 2) ]
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


numberInput : ThemeProperties -> String -> (String -> Msg) -> Int -> Html Msg
numberInput themeProperties valueString msg tabIndex =
    let
        textColor =
            if valueString == "" || isLegalNumber valueString then
                themeProperties.textColor

            else
                themeProperties.errorTextColor
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
            , Css.backgroundColor (Css.rgba 0 0 0 0)
            , Css.borderWidth (Css.px 0)
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
