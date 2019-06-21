module Main exposing (Model, Msg(..), Operation(..), calculate, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



{- TO DO

   add to model prevNumber param
-}


main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { number : String
    , prevNumber : String
    , result : Float
    , operation : Operation
    , activeOperation : Operation
    , screen : String
    }


init : Model
init =
    { number = ""
    , prevNumber = ""
    , result = 0
    , operation = NoOp
    , activeOperation = NoOp
    , screen = "0"
    }



-- UPDATE


calculate : Float -> Operation -> String -> Float
calculate result operation number =
    let
        floatNumber =
            case String.toFloat number of
                Just float ->
                    float

                Nothing ->
                    result
    in
    case operation of
        Addition ->
            result + floatNumber

        Substraction ->
            result - floatNumber

        Multiplication ->
            result * floatNumber

        Division ->
            result / floatNumber

        NoOp ->
            0.0

        ShowResult ->
            result


type Msg
    = EnterDigit String
    | EnterOperation Operation
    | Reset
    | ChangeSign
    | Persent


type Operation
    = Addition
    | Substraction
    | Multiplication
    | Division
    | ShowResult
    | NoOp


update : Msg -> Model -> Model
update msg ({ number, prevNumber, result, screen, operation, activeOperation } as model) =
    let
        resultCalc =
            calculate result operation number

        floatNumber num =
            case String.toFloat num of
                Just float ->
                    float

                Nothing ->
                    0

        checkIfNumIsEmpty num =
            case number of
                "" ->
                    case num of
                        "." ->
                            "0" ++ num

                        _ ->
                            number ++ num

                _ ->
                    number ++ num

        checkOperation =
            case operation of
                NoOp ->
                    floatNumber number

                _ ->
                    resultCalc
    in
    case msg of
        EnterDigit num ->
            { model
                | number = checkIfNumIsEmpty num
                , screen = checkIfNumIsEmpty num
                , activeOperation = NoOp
            }

        EnterOperation enteredOperation ->
            case enteredOperation of
                ShowResult ->
                    { model
                        | result =
                            case number of
                                "" ->
                                    calculate result operation prevNumber

                                _ ->
                                    calculate result operation number
                        , screen =
                            String.fromFloat
                                (case number of
                                    "" ->
                                        calculate result operation prevNumber

                                    _ ->
                                        calculate result operation number
                                )
                        , activeOperation = ShowResult

                        {-
                           , number = "" исправляет 2+3=+6, но ломает 2+3==

                           2+= = = = работает неправильно
                        -}
                    }

                _ ->
                    case activeOperation of
                        ShowResult ->
                            { model
                                | result = result
                                , screen = String.fromFloat result
                                , number = ""
                            }

                        _ ->
                            case number of
                                "" ->
                                    { model
                                        | operation = enteredOperation
                                        , activeOperation = enteredOperation
                                    }

                                _ ->
                                    { model {- из-за number=""не работает 2+= = = -}
                                        | number = ""
                                        , prevNumber = number
                                        , result = checkOperation
                                        , operation = enteredOperation
                                        , activeOperation = enteredOperation
                                        , screen = String.fromFloat checkOperation
                                    }

        ChangeSign ->
            { model
                | number = String.fromFloat (-1 * floatNumber number)
                , result = -1 * result
                , screen = String.fromFloat (-1 * floatNumber screen)
                , activeOperation = NoOp
            }

        Persent ->
            { model
                | number = String.fromFloat (floatNumber number / 100)
                , result = result / 100
                , screen = String.fromFloat (floatNumber screen / 100)
                , activeOperation = NoOp
            }

        {-
           , number = "" исправляет 2+3=+6, но ломает 2+3==

           2+= = = = работает неправильно
        -}
        Reset ->
            { model
                | number = ""
                , result = 0
                , operation = NoOp
                , screen = "0"
                , activeOperation = NoOp
            }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "result" ]
            [ text model.screen ]
        , div
            [ class "row" ]
            [ button [ class "other", onClick <| Reset ] [ text "AC" ]
            , button [ class "other", onClick <| ChangeSign ] [ text "+/-" ]
            , button [ class "other", onClick <| Persent ] [ text "%" ]
            , button
                [ classList [ ( "active-operation", model.activeOperation == Division ) ]
                , class "operations"
                , onClick <| EnterOperation Division
                ]
                [ text "/"
                ]
            ]
        , div [ class "row" ]
            [ button [ class "numbers", onClick <| EnterDigit "7" ] [ text "7" ]
            , button [ class "numbers", onClick <| EnterDigit "8" ] [ text "8" ]
            , button [ class "numbers", onClick <| EnterDigit "9" ] [ text "9" ]
            , button
                [ classList [ ( "active-operation", model.activeOperation == Multiplication ) ]
                , class "operations"
                , onClick <| EnterOperation Multiplication
                ]
                [ text "x"
                ]
            ]
        , div [ class "row" ]
            [ button [ class "numbers", onClick <| EnterDigit "4" ] [ text "4" ]
            , button [ class "numbers", onClick <| EnterDigit "5" ] [ text "5" ]
            , button [ class "numbers", onClick <| EnterDigit "6" ] [ text "6" ]
            , button
                [ classList [ ( "active-operation", model.activeOperation == Substraction ) ]
                , class "operations"
                , onClick <| EnterOperation Substraction
                ]
                [ text "-"
                ]
            ]
        , div [ class "row" ]
            [ button [ class "numbers", onClick <| EnterDigit "1" ] [ text "1" ]
            , button [ class "numbers", onClick <| EnterDigit "2" ] [ text "2" ]
            , button [ class "numbers", onClick <| EnterDigit "3" ] [ text "3" ]
            , button
                [ classList [ ( "active-operation", model.activeOperation == Addition ) ]
                , class "operations"
                , onClick <| EnterOperation Addition
                ]
                [ text "+" ]
            ]
        , div [ class "row" ]
            [ button [ class "numbers big-button", onClick <| EnterDigit "0" ] [ text "0" ]
            , button [ class "numbers", onClick <| EnterDigit "." ] [ text "." ]
            , button [ class "operations", onClick <| EnterOperation ShowResult ] [ text "=" ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
