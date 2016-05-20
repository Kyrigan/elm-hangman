import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as HtmlA
import Array exposing (Array)
import String
import Random
import Json.Decode as Json
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Attributes as SvgA

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- VARIABLES

answerString = "Alphabet"

answer = Array.fromList (String.split "" answerString)

length' = Array.length answer

-- MODEL

type Status = Win | Loss | Ongoing


type alias Model =
  { input : String,
    chances : Int ,-- Number of attempts remaining
    answer : Array String,
    current : Array String, -- Shows what you have figured out so far.
    oldCurrent : Array String, -- Keeps track of your previous to determine mistakes.
    status : Status,
    hangman : List (Svg Msg)
  }


init : (Model, Cmd Msg)
init = let current = Array.repeat length' "_"
       in ( Model "" 9 answer current current Ongoing
                       [stand, head , torso, leftArm, rightArm, leftLeg, rightLeg, leftEye, rightEye, blood]
          , Cmd.none)


under n = "_"

-- UPDATE

type Msg
  = NewContent String | Check | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewContent content -> ( {model | input = String.toUpper content
                                   }
                            , Cmd.none)
    Check -> (checkFunction model, Cmd.none)
    Reset -> init
    
checkFunction model = -- Searches for the character in the answer.
  let current = Array.fromList (
                  List.map2 (compare model.input) 
                    (Array.toList model.current)
                    (Array.toList model.answer)
                  )
      chances = if current == model.current then model.chances - 1
                                            else model.chances 
  in { model | input = "", -- Empties input box for convenience.
               chances = chances ,
               current = current , 
               oldCurrent = model.current,
               status = if chances == 0 then Loss else
                        if model.answer == current then Win else
                        model.status
                    }
  
compare input currentChar char = 
  if String.toUpper char == input || currentChar == char
    then char 
  else 
         "_"   
  
  
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW


view : Model -> Html Msg
view model = 
  body [] [
  div [headStyle ]
    [ input [inputStyle, placeholder "Make your guess!", onInput NewContent , maxlength 1, value model.input] []
    , br[] []
    , button [ buttonStyle, onClick Check ,
               hidden (model.status /= Ongoing) 
    
    ] [ Html.text "Submit" ]
    , button [ buttonStyle, onClick Reset ,
               hidden (model.status == Ongoing) 
    
    ] [ Html.text "Reset" ]
    ] ,
  div [] [hangmanScreen model] ,
  div [myStyle]
    [ 
      Html.text (viewArray model.current) ] 
    ]
    
myStyle : Html.Attribute msg
myStyle =
  HtmlA.style
    [ ("text-align", "left")
    , ("height", "200px")
    , ("width", "200px")
    , ("font-family", "Courier New")
    , ("padding-left", "20px")
    ]
    
headStyle : Html.Attribute msg
headStyle =
  HtmlA.style
    [ ("width", "140px")
    , ("height", "50px")
    , ("font-family", "Courier New")
    , ("padding-left", "20px")
    , ("padding-top", "20px")
    ]
    
inputStyle : Html.Attribute msg
inputStyle =
  HtmlA.style
    [ ("text-align", "center")
    , ("font-family", "Courier New")
    , ("width", "100%")
    ]
    
buttonStyle : Html.Attribute msg
buttonStyle =
  HtmlA.style
    [ ("font-family", "Courier New")
    , ("margin-top", "2px")
    ]
 
viewArray array = -- Converts array to string.
  String.join " " (Array.toList array)

hangmanScreen model = Svg.svg
      [ SvgA.width "200", SvgA.height "200", viewBox "0 0 100 100" ] (List.take (10-model.chances) model.hangman)
                      

stand = g [] [ 
          polyline [ points "30,90 10,90 20,90 20,10 60,10 60,20", 
                    SvgA.style "fill:none;stroke:rgb(0,0,0);stroke-width:2" ] [] 
        ]

head = g [] [ 
          circle [ cx "60", cy "30", r "10" ,stroke "black" ,strokeWidth "1", fill "#faefca" ] [] 
        ]
        
torso = g [] [ 
          polyline [ points "60,40 60,65", 
                    SvgA.style "fill:none;stroke:rgb(0,0,0);stroke-width:2" ] [] 
        ]
        
leftLeg = g [] [ 
          polyline [ points "60,65 50,85", 
                    SvgA.style "fill:none;stroke:rgb(0,0,0);stroke-width:2" ] [] 
        ]
        
rightLeg = g [] [ 
          polyline [ points "60,65 70,85", 
                    SvgA.style "fill:none;stroke:rgb(0,0,0);stroke-width:2" ] [] 
        ]
        
leftArm = g [] [ 
          polyline [ points "60,45 50,65", 
                    SvgA.style "fill:none;stroke:rgb(0,0,0);stroke-width:2" ] [] 
        ]
        
rightArm = g [] [ 
          polyline [ points "60,45 70,65", 
                    SvgA.style "fill:none;stroke:rgb(0,0,0);stroke-width:2" ] [] 
        ]
        
leftEye = g [] [ 
          circle [ cx "55", cy "30", r "2" ,stroke "black" ,strokeWidth "1", fill "#fff" ] [] 
        ]
        
rightEye = g [] [ 
          circle [ cx "65", cy "30", r "2" ,stroke "black" ,strokeWidth "1", fill "#fff" ] [] 
        ]
       
blood = g [] [ 
          polyline [ points "55,32 55,35", 
                    SvgA.style "fill:none;stroke:rgb(255,0,0);stroke-width:2" ] [], 
          polyline [ points "65,32 65,35", 
                    SvgA.style "fill:none;stroke:rgb(255,0,0);stroke-width:2" ] []
        ]

winScreen = Html.text "You Win!" 

loseScreen = Html.text "You Lost!" 



     