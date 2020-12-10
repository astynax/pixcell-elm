module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Button exposing (..)
import Color exposing (Color, rgb255)
import Export
import Grid exposing (Grid)
import History exposing (Direction, History)
import Html
import Palette exposing (Palette)
import String
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (..)
import Svg.Events exposing (onMouseDown)
import Task
import Glyph


type alias Model =
    { grid : History (Grid Int)
    , color : Int
    , palette : Palette
    , guides : Bool
    , resize : Bool
    , width : Int
    , height : Int
    }


type Msg
    = Resize Int Int
    | ToggleResize
    | ToggleGuides
    | NextPalette
    | Cell Int Int
    | Color Int
    | Apply Grid.Transformation
    | Clear
    | Undo
    | Redo
    | ExportPng


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { grid = History.steps 10 <| Grid.from 0
      , color = 0
      , palette = Palette.init
      , guides = True
      , resize = True
      , width = 680
      , height = 680
      }
    , getSize
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize w h ->
            ( { model | width = modSize w, height = modSize h }, Cmd.none )

        ToggleResize ->
            ( { model | resize = not model.resize }
            , if not model.resize then
                getSize

              else
                Cmd.none
            )

        ToggleGuides ->
            ( { model | guides = not model.guides }, Cmd.none )

        Color x ->
            ( { model | color = x }, Cmd.none )

        Clear ->
            ( updateGrid (always <| Grid.from model.color) model
            , Cmd.none
            )

        Cell x y ->
            ( updateGrid (Grid.set model.color x y) model
            , Cmd.none
            )

        Apply t ->
            ( updateGrid (Grid.apply t) model
            , Cmd.none
            )

        Undo ->
            ( timeTravel History.Back model, Cmd.none )

        Redo ->
            ( timeTravel History.Forward model, Cmd.none )

        NextPalette ->
            ( { model | palette = Palette.next model.palette }
            , Cmd.none
            )

        ExportPng ->
            ( model, Export.toPng <| coloredGrid model )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.resize then
        Browser.Events.onResize Resize

    else
        Sub.none


modSize : Int -> Int
modSize x =
    -- ugly, but it scales without artifacts
    17 * (x // 17)


getSize : Cmd Msg
getSize =
    Task.perform
        (\vp ->
            Resize (truncate vp.viewport.width)
                (truncate vp.viewport.height)
        )
        Browser.Dom.getViewport


updateGrid : (Grid Int -> Grid Int) -> Model -> Model
updateGrid f model =
    { model | grid = History.change f model.grid }


timeTravel : Direction -> Model -> Model
timeTravel d model =
    case History.travel d model.grid of
        Just g ->
            { model | grid = g }

        Nothing ->
            model


appTitle : String
appTitle =
    "PixCell/Elm v1.0"


view : Model -> Browser.Document Msg
view model =
    let
        ifResize f x y =
            f <|
                if not model.resize then
                    x

                else
                    String.fromInt y
    in
    { title = appTitle
    , body =
        [ Svg.svg
            [ ifResize width "680" model.width
            , ifResize height "680" model.height
            , viewBox "0 0 170 170"
            ]
            [ editorView model
            ]
        , Html.node "style" [] [ Html.text "body { background: black; } " ]
        ]
    }


coloredGrid : Model -> Grid Color
coloredGrid model =
    Grid.encodeUsing Color.black model.palette.current model.grid.now


editorView : Model -> Svg Msg
editorView model =
    Svg.g []
        [ tools model
        , grid model.guides <| coloredGrid model
        , palette model.palette.current <| model.color
        ]


tools : Model -> Svg Msg
tools model =
    Svg.g [] <|
        List.map viewButton <|
            hbox <|
                [ Button.activate model.resize <|
                    imageButton Glyph.resize ToggleResize
                , Button.activate model.guides <|
                    imageButton Glyph.grid ToggleGuides
                , imageButton Glyph.floppy ExportPng
                , Button.disable
                    (not <| History.canTravel History.Back model.grid)
                  <|
                    imageButton Glyph.undo Undo
                , Button.disable
                    (not <| History.canTravel History.Forward model.grid)
                  <|
                    imageButton Glyph.redo Redo
                , imageButton Glyph.paint Clear
                , imageButton Glyph.scrollR <| Apply Grid.ScrollR
                , imageButton Glyph.scrollD <| Apply Grid.ScrollD
                , imageButton Glyph.flipH <| Apply Grid.FlipH
                , imageButton Glyph.flipV <| Apply Grid.FlipV
                , imageButton Glyph.rotate <| Apply Grid.Rotate
                , imageButton Glyph.reflectQ <| Apply Grid.ReflectQ
                , imageButton Glyph.reflectH <| Apply Grid.ReflectH
                , imageButton Glyph.reflectV <| Apply Grid.ReflectV
                , imageButton Glyph.reflectR <| Apply Grid.ReflectR
                , imageButton Glyph.cycle <| Apply Grid.Cycle
                , imageButton Glyph.palette NextPalette
                ]


palette : List Color -> Int -> Svg Msg
palette colors current =
    Svg.g [] <|
        List.map (viewButton << Button.dy 10 << Button.dx 160) <|
            vbox <|
                List.indexedMap
                    (\idx c ->
                        Button.activate (idx == current) <|
                            colorButton c (Color idx)
                    )
                    colors


grid : Bool -> Grid Color -> Svg Msg
grid guides g =
    Svg.g [] <|
        List.concat <|
            List.indexedMap
                (\row cols ->
                    List.indexedMap
                        (\col color ->
                            let
                                cell =
                                    { x = col * 10
                                    , y = 10 + row * 10
                                    , size = 10
                                    }

                                pixel =
                                    if guides then
                                        { cell | size = cell.size - 1 }

                                    else
                                        cell

                                bg =
                                    if guides then
                                        [ Svg.rect
                                            [ bind x .x cell
                                            , bind y .y cell
                                            , bind width .size cell
                                            , bind height .size cell
                                            , fill "#404040"
                                            ]
                                            []
                                        ]

                                    else
                                        []
                            in
                            Svg.g [ onMouseDown <| Cell col row ] <|
                                List.append bg
                                    [ Svg.rect
                                        [ bind x .x pixel
                                        , bind y .y pixel
                                        , bind width .size pixel
                                        , bind height .size pixel
                                        , fill <| Color.toCssString color
                                        ]
                                        []
                                    ]
                        )
                        cols
                )
                g
