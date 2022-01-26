port module Main exposing(..)
import Browser
import Html exposing (text, div, input, Html, select, option)
import Dict exposing (update)
import Html.Attributes exposing (style,class)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)

f: Int->Int
f a = a+1

-- MAIN
main : Program () FullModel Msg
main = Browser.sandbox{init = init, update = update, view = view}

-- MODEL

type alias Model = 
    { content1: Int
    , content2: Int
    , op: String
    , output: Float
    }

type alias FullModel =
    { curr : Model
    , hist : List Model
    }
 

init: FullModel
init = 
    { curr={content1 = 0,
            content2 = 0,
            op = "+",
            output = 0.0
        }
     ,hist=[]
    }

-- UPDATE
type Msg
    = Change1 String
    | Change2 String
    | ChangeOp String

port saveHistory: List Model -> Cmd msg

calc: String->Int->Int->Float
calc o = 
    case o of
        "+" -> add
        "-" -> sub
        "*" -> mult
        ":" -> divide
        _ -> zero


add : Int -> Int -> Float
add a b = a+b|>toFloat
sub : Int -> Int -> Float
sub a b = a-b|>toFloat
mult : Int -> Int -> Float
mult a b = a*b|>toFloat
divide : Int -> Int -> Float
divide a b = (toFloat a)/(toFloat b)
zero : Int -> Int -> Float
zero _ _ = 0
    

calcOut: String -> Int -> Int -> Model
calcOut op c1 c2 =
    { content1 = c1
    ,content2 = c2
    ,op = op
    , output = calc op c1 c2
    }

stoi: Int -> String -> (Int, Bool)
stoi d s =
    -- Maybe.withDefault d <| String.toInt s
    let 
        si = String.toInt s
    in
        case si of
            Just value -> (value, True)
            Nothing -> (d, False)
updateCurr: Msg -> Model -> (Model, Bool)
updateCurr msg model =
    case msg of
       Change1 newContent ->
        let
            newContent1 = stoi model.content1 newContent
        in
            (calcOut model.op (Tuple.first newContent1) model.content2, Tuple.second newContent1)
       Change2 newContent ->
        let
            newContent2 = stoi model.content2 newContent
        in
            (calcOut model.op model.content1 (Tuple.first newContent2), Tuple.second newContent2)
       ChangeOp newOp ->
        (calcOut newOp model.content1 model.content2, True)

updateModel: FullModel -> (Model, Bool) -> FullModel
updateModel model newcurr =
    {
        curr = Tuple.first newcurr,
        hist = updateHist model.hist newcurr
    }

updateHist: List Model -> (Model, Bool) -> List Model
updateHist hist newcurr = 
    if Tuple.second newcurr then
        hist ++ [Tuple.first newcurr]
    else
        hist

update: Msg -> FullModel -> FullModel
update msg model = 
    updateCurr msg model.curr |> updateModel model |> save

save: FullModel->FullModel
save model =
    saveHistory(model.hist)|> \_->model
    

-- VIEW

stooption: String -> Html Msg
stooption s =
    option [ value s ] [ text s ]
resview: Model -> Html Msg
resview m =
    text <| (String.fromInt m.content1) ++ " " ++ m.op ++ " " ++ (String.fromInt m.content2) ++ " = " ++ (String.fromFloat m.output)

histItem: Model -> Html Msg
histItem m =
    div[]
    [resview m]

view : FullModel -> Html Msg
view model = 
    div[]
    [div []
    [input[value <| String.fromInt model.curr.content1, onInput Change1][]
    ,select[onInput ChangeOp, value model.curr.op](["+","-","*",":"] |> List.map stooption)
    ,input[value <| String.fromInt  model.curr.content2, onInput Change2][]
    ,div [][text "Result: ",resview model.curr]
    ]
    ,div []
    (div[][text "History"] :: (model.hist |> List.map histItem))
    ]