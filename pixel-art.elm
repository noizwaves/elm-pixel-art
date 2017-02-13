import Html exposing (..)
import Html.Attributes exposing (..)
--import Html.Events exposing (onInput)

main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL

type CellColour = NoColour | Colour String

type alias Grid = List (List CellColour)

type alias Model =
  { grid: Grid
  }


-- UPDATE

type Msg = ActionOne | ActionTwo

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ActionOne -> (model, Cmd.none)
    ActionTwo -> (model, Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ stylesheet "reset.css"
    , stylesheet "style.css"
    , gridAsPixels model.grid
    ]

gridAsPixels : Grid -> Html Msg
gridAsPixels grid =
  div [class "grid"] (List.map rowAsPixels grid)

rowAsPixels : (List CellColour) -> Html Msg
rowAsPixels row =
  div [class "row"] (List.map cellAsPixel row)

cellAsPixel : CellColour -> Html Msg
cellAsPixel colour =
  case colour of
    Colour value -> div [ class "pixel", style [ ("backgroundColor", value) ] ] [ ]
    NoColour -> div [ class "pixel", style [("backgroundColor", "white")] ] [ ]

stylesheet : String -> Html Msg
stylesheet url =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      url
            ]
        children = []
    in
        node tag attrs children


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- INIT

initGrid : Int -> Int -> Grid
initGrid rows columns =
  if rows == 0 then
    []
  else
    initRow columns :: (initGrid (rows - 1) columns)

initRow : Int -> List CellColour
initRow size =
  if size == 0 then
    []
  else
    NoColour :: (initRow (size - 1))


init : (Model, Cmd Msg)
init = (Model (initGrid 20 20), Cmd.none)
