module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url


-- MAIN

main : Program Flags Model Msg
main =
  Browser.application 
  { init = init
  , update = update
  , view = view 
  , subscriptions = subscriptions
  , onUrlRequest = onUrlRequest
  , onUrlChange = onUrlChange
  }


-- MODEL

type Model 
  = Welcome Nav.Key
  | Calc Int Nav.Key
  | ErrorUrl Url.Url Nav.Key
  | ErrorMsg Msg

type alias Flags = ()
init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  ( case url.path of
     "/welcome" -> Welcome key
     "/calc" -> Calc 0 key
     _ -> ErrorUrl url key
  , Cmd.none 
  )


-- UPDATE

type Msg = Increment | Decrement | Reset | Nop
msg2string : Msg -> String
msg2string msg = case msg of
  Increment -> "Increment"
  Decrement -> "Decrement"
  Reset -> "Reset"
  Nop -> "Nop"

msg2Val : Msg -> Int -> Int
msg2Val msg val = 
  case msg of 
    Nop -> val
    Increment -> val + 1
    Decrement -> val - 1
    Reset -> 0

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model of
    Calc val key -> ( Calc (msg2Val msg val) key, Cmd.none )
    _ -> ( ErrorMsg msg, Cmd.none )


-- VIEW

view : Model -> Browser.Document Msg
view model = 
  case model of
    Welcome key -> viewWelcome key
    Calc val key -> viewCalc val key      
    ErrorMsg msg -> viewError ("Bad message " ++ (msg2string msg))
    ErrorUrl url key -> viewError ("Bad URL " ++ (Url.toString url))

viewCalc : Int -> Nav.Key -> Browser.Document Msg
viewCalc val key = 
  { title = "Calc"
  , body = 
      [ div []
          [ button [ onClick Decrement ] [ text "-" ]
          , div [] [ text (String.fromInt val) ]
          , button [ onClick Increment ] [ text "+" ]
          , node "hr" [] []
          , button [ onClick Reset ] [ text "Reset" ]
          ]
      ]
  }

viewWelcome : Nav.Key -> Browser.Document Msg
viewWelcome key =
  { title = "Welcome"
  , body =
    [ div []
      [ a [ href "/calc" ] [ text "Go to calc" ] ]
    ]
  }

viewError : String -> Browser.Document Msg
viewError err =
  { title = "Error"
  , body =
    [ div []
      [ text "Error"
      , a [ href "/welcome" ] [ text "Go to welcome" ] 
      ]
    ]
  }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ = Nop

onUrlChange : Url.Url -> Msg
onUrlChange _ = Nop