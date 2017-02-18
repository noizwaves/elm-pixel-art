import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)

main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL

type CellColour = NoColour | Colour String

type alias Grid = Array (Array CellColour)

type alias Model =
  { grid: Grid
  , brush: String
  , palette: List String
  }


-- UPDATE

type alias RowIndex = Int
type alias ColumnIndex = Int

type Msg = ColourPixel RowIndex ColumnIndex | PickBrush String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ColourPixel row column -> ({model | grid = updatePixelInGrid model.grid row column model.brush}, Cmd.none)
    PickBrush newBrush -> ({model | brush = newBrush}, Cmd.none)

updatePixelInGrid : Grid -> RowIndex -> ColumnIndex -> String -> Grid
updatePixelInGrid grid rowNumber columnNumber brush =
  let
    updatedRow = Array.get rowNumber grid
  in
    case updatedRow of
      Just validRow -> Array.set rowNumber (updatePixelInRow validRow columnNumber brush) grid
      Nothing -> grid

updatePixelInRow : Array CellColour -> ColumnIndex -> String -> Array CellColour
updatePixelInRow row columnNumber brush =
  Array.set columnNumber (Colour brush) row




-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ stylesheet "reset.css"
    , stylesheet "style.css"
    , gridAsPixels model.grid
    , div [class "palette"] (List.map (colourAsBrush model.brush) model.palette)
    ]

gridAsPixels : Grid -> Html Msg
gridAsPixels grid =
  let
    rows = Array.toIndexedList grid -- List (Int, Array CellColour)
  in
    div [ class "grid" ] (List.map rowAsPixels rows)

rowAsPixels : (Int, Array CellColour) -> Html Msg
rowAsPixels (rowNum, row) =
  let
    cells = Array.toIndexedList row -- List (Int, CellColour)
  in
    div [class "row"] (List.map (cellAsPixel rowNum) cells)


cellAsPixel : Int -> (Int, CellColour) -> Html Msg
cellAsPixel rowNum (columnNum, colour) =
  case colour of
    Colour value -> div
      [ class "pixel", style [ ("backgroundColor", value) ], onClick (ColourPixel rowNum columnNum) ]
      [ ]
    NoColour -> div
      [ class "pixel", style [("backgroundColor", "white")], onClick (ColourPixel rowNum columnNum) ]
      [ ]

colourAsBrush : String -> String -> Html Msg
colourAsBrush selected colour =
  div
    [ classList
      [ ("brush", True)
      , ("selected", colour == selected)
      ]
    , style [("backgroundColor", colour)]
    , onClick (PickBrush colour)
    ] []


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
  Array.initialize rows (always (initRow columns))

initRow : Int -> Array CellColour
initRow size =
  Array.initialize size (always NoColour)

initPalette : List String
initPalette = ["blue", "red", "yellow", "green"]


init : (Model, Cmd Msg)
init = (Model (initGrid 20 20) "blue" initPalette, Cmd.none)
