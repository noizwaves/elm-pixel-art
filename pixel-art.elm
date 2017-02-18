import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseOver, onMouseDown, onMouseUp)
import Array exposing (Array)

main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL

type CellColour = NoColour | Colour String

type alias Grid = Array (Array CellColour)

type alias Model =
  { grid: Grid
  , brush: String
  , brushDown: Bool
  , palette: List String
  }


-- UPDATE

type alias RowIndex = Int
type alias ColumnIndex = Int

type Msg = ColourPixel RowIndex ColumnIndex | PickBrush String | BrushDown RowIndex ColumnIndex | BrushUp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ColourPixel row column ->
      let
        updatedGrid = if model.brushDown
          then updatePixelInGrid model.grid row column model.brush
          else model.grid
       in
        ({model | grid = updatedGrid}, Cmd.none)
    PickBrush newBrush -> ({model | brush = newBrush}, Cmd.none)
    BrushDown row column ->
     let
      updatedGrid = updatePixelInGrid model.grid row column model.brush
     in
      ({model | brushDown = True, grid = updatedGrid}, Cmd.none)
    BrushUp -> ({model | brushDown = False}, Cmd.none)

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
  let
    pixelColour = case colour of
      Colour value -> value
      NoColour -> "white"
  in
    div
      [ class "pixel"
      , style [ ("backgroundColor", pixelColour) ]
      , onMouseDown (BrushDown rowNum columnNum)
      , onMouseUp BrushUp
      , onMouseOver (ColourPixel rowNum columnNum)
      ]
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
init = (Model (initGrid 20 20) "blue" False initPalette, Cmd.none)
