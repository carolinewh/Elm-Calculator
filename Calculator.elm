{--
Caroline Whitman
Final Project Software
Calculator.elm
--}

port module Calculator exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import List exposing (..)
import String exposing (..)


main =
  Html.beginnerProgram
  {
    model = model,
    view = view,
    update = update
  }

-- MODEL

type alias Model =
  {
    -- inputStr is the string that tracks what the user just inputted
    inputStr : String,
    -- displayStr is the string that is shown on the calculator "screen"
    displayStr : String,
    -- funcType represents the operation the user wishes to carry out
    funcType : Float -> Float -> Float,
    -- input1 will hold the first number the user inputs
    input1 : Float,
    -- isSecondInput indicates whether the user has indicated the operator
    -- and is entering a second number
    isSecondInput : Bool
  }

model : Model
model =
  {
    inputStr = "",
    displayStr = "",
    funcType = (\x y -> y),
    input1 = 0,
    isSecondInput = False
  }


{--
Originally I had written out functions for add, subtract, multiply,
and divide. However, I realized this could be done in a cleaner fashion
using anonymous functions.
Source: https://github.com/tensor-programming/elm-tutorial-6/blob/master/calc.elm
--}
-- creates skeleton for calculator operations
type alias Calculator =
  {
    add : Float -> Float -> Float,
    subtract : Float -> Float -> Float,
    multiply : Float -> Float -> Float,
    divide : Float -> Float -> Float
  }

calc : Calculator
calc =
    {
      add = (\x y -> x + y)
      , subtract = (\x y -> x - y)
      , multiply = (\x y -> x * y)
      , divide = (\x y -> x / y)
    }

-- takes a String as an input and returns its corresponding float value.
-- on error, returns 0
parseFloat : String -> Float
parseFloat str = Result.withDefault 0 (String.toFloat str)

-- UPDATE

type Msg = Clear
        | Add
        | Subtract
        | Multiply
        | Divide
        | Equals
        | Number Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Clear ->
      -- resets the calculator
      {
        model |
        inputStr = "",
        displayStr = "",
        funcType = (\x y -> y),
        input1 = 0,
        isSecondInput = False
      }
    Add ->
      setOperation model calc.add "+"
    Subtract ->
      setOperation model calc.subtract "-"
    Multiply ->
      setOperation model calc.multiply "x"
    Divide ->
      setOperation model calc.divide "÷"
    Equals ->
      equal model
    Number num ->
      updateDisplay model num

equal : Model -> Model
equal model =
  {
    model |
    input1 = 0,
    inputStr = "",
    displayStr = evaluate model
  }

-- calculates the result of the expression the user inputted
evaluate : Model -> String
evaluate model =
  toString (model.funcType model.input1 (parseFloat model.inputStr))

updateDisplay : Model -> Int -> Model
updateDisplay model num =
  if model.isSecondInput then
  {
    model |
      -- concatenates current inputStr with the number just inputted so the
      -- calculator can support multidigit calculations
    inputStr = model.inputStr ++ (toString num),
    displayStr = model.displayStr ++ (toString num)
  } else
    {
      model |
      input1 = parseFloat (model.inputStr),
      inputStr = model.inputStr ++ (toString num),
      displayStr = model.displayStr ++ (toString num)
    }

setOperation : Model -> (Float -> Float -> Float) -> String -> Model
setOperation model func op =
  {
    model |
    inputStr = "",
    displayStr = model.inputStr ++ op,
    funcType = func,
    input1 = (parseFloat model.inputStr),
    isSecondInput = True
  }

-- VIEW

-- helper function to add calculator buttons
addButton : String -> Msg -> Html Msg
addButton btnName action =
  button [class "button", onClick action] [text btnName]


{--
I had no idea how to link a css file to an Elm file, and there was not
much documentation on the Elm website. So, I started looking at examples
of how this was done, and I based this portion of code on a clean
implementation I found.
Source: https://github.com/tensor-programming/elm-tutorial-6/blob/master/calc.elm
--}
stylesheet: Html Msg
stylesheet =
    let
      tag =
        "link"
      attrs =
        [attribute "Rel" "stylesheet"
        , attribute "property" "stylesheet"
        , attribute "href" "styles.css"
        ]
      children =
        []
    in
      node tag attrs children

-- creates the view for the calculator
view : Model -> Html Msg
view model =
  div [ class "calculator" ]
        [ stylesheet
        , div []
            [ div []
                [ div [ class "display" ]
                    [ div [ class "display-text" ]
                        [ text (model.displayStr) ]
                    ],
     div [class "buttons"] [
       addButton "7" (Number 7),
       addButton "8" (Number 8),
       addButton "9" (Number 9),
       addButton "÷" Divide
     ],
     div [class "buttons"] [
       addButton "4" (Number 4),
       addButton "5" (Number 5),
       addButton "6" (Number 6),
       addButton "x" Multiply
     ],
     div [class "buttons"] [
       addButton "1" (Number 1),
       addButton "2" (Number 2),
       addButton "3" (Number 3),
       addButton "–" Subtract
     ],
     div [class "buttons"] [
       addButton "C" Clear,
       addButton "0" (Number 0),
       addButton "=" Equals,
       addButton "+" Add
     ]
  ]
  ]
  ]
