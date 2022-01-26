port module Main exposing(..)
import Browser
import Html exposing (text, div, input, Html, select, option)
import Dict exposing (update)
import Html.Attributes exposing (class)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Json.Decode as JD exposing (Decoder, field, int,string,float)

f: Int->Int
f a = a+1

-- MAIN
main : Program JD.Value FullModel Msg
main = Browser.element{init = init, update = update, view = view, subscriptions = subscriptions}

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
 
port saveHistory: List Model -> Cmd msg


init: JD.Value -> (FullModel, Cmd Msg)
init flags = 
    let
        history = case JD.decodeValue parseListModel flags of
           Ok status -> status
        --    Err err -> [{content1=1, content2=1,op=JD.errorToString err,output=2}]
            Err _ -> []
    in
    
    ({ curr={content1 = 0,
            content2 = 0,
            op = "+",
            output = 0.0
            }
       ,hist= history
     }
     , Cmd.none
    )

parseModel: Decoder Model
parseModel = 
    JD.map4 Model
        (field "content1" int)
        (field "content2" int)
        (field "op" string)
        (field "output" float)
parseListModel : Decoder (List Model)
parseListModel =
    JD.list parseModel
    

-- UPDATE
type Msg
    = Change1 String
    | Change2 String
    | ChangeOp String

calc: String->Int->Int->Float
calc o = 
    case o of
        "+" -> add
        "-" -> sub
        "*" -> mult
        ":" -> divide
        _ -> zero


add : Int -> Int -> Float
add a b = a + b |> toFloat
sub : Int -> Int -> Float
sub a b = a - b |> toFloat
mult : Int -> Int -> Float
mult a b = a * b |> toFloat
divide : Int -> Int -> Float
divide a b = (toFloat a) / (toFloat b)
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

update: Msg -> FullModel -> (FullModel, Cmd Msg)
update msg model = 
    let
        newmodel = updateCurr msg model.curr |> updateModel model
    in
        (newmodel
         , newmodel.hist |> saveHistory
        )

-- SUBSCRIPTIONS
subscriptions : FullModel -> Sub Msg
subscriptions _ =
    Sub.none

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
    ,div [class "result"][text "Result: ",resview model.curr]
    ]
    ,div []
    (div[class "hist-title"][text "History"] :: (model.hist |> List.map histItem))
    ]