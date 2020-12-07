module Main exposing (main)

import Browser
import Button exposing (..)
import Color exposing (Color, rgb255)
import Grid exposing (Grid)
import History exposing (Direction, History)
import Palette exposing (Palette)
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (..)
import Svg.Events exposing (onClick)


type alias Model =
    { grid : History (Grid Int)
    , color : Int
    , palette : Palette
    , guides : Bool
    }


type Msg
    = Cell Int Int
    | Color Int
    | ToggleGuides
    | Apply Grid.Transformation
    | Clear
    | Undo
    | Redo
    | NextPalette


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { grid = History.steps 10 <| Grid.from 0
      , color = 0
      , palette = Palette.init
      , guides = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleGuides ->
            ( { model
                | guides = not model.guides
              }
            , Cmd.none
            )

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
    { title = appTitle
    , body =
        [ Svg.svg [ width <| Debug.toString (170 * 4), viewBox "0 0 170 170" ]
            [ editorView model
            ]
        ]
    }


editorView : Model -> Svg Msg
editorView model =
    Svg.g []
        [ tools model
        , grid model.guides <|
            Grid.encodeUsing Color.black model.palette.current <|
                model.grid.now
        , palette model.palette.current <| model.color
        ]


tools : Model -> Svg Msg
tools model =
    Svg.g [] <|
        List.map viewButton <|
            hbox <|
                [ Button.disable
                    (not <| History.canTravel History.Back model.grid)
                  <|
                    button "↶" Undo
                , Button.disable
                    (not <| History.canTravel History.Forward model.grid)
                  <|
                    button "↷" Redo
                , Button.disable True <| button "" Clear
                , button "🗋" Clear
                , button "⇸" <| Apply Grid.ScrollR
                , button "⤈" <| Apply Grid.ScrollD
                , button "⇄" <| Apply Grid.FlipH
                , button "⇅" <| Apply Grid.FlipV
                , button "⥁" <| Apply Grid.Rotate
                , button "▟" <| Apply Grid.ReflectQ
                , button "▐" <| Apply Grid.ReflectH
                , button "▄" <| Apply Grid.ReflectV
                , button "◕" <| Apply Grid.ReflectR
                , Button.disable True <| button "" Clear
                , Button.disable True <| button "" Clear
                , Button.activate model.guides <|
                    button "#" ToggleGuides
                , button "🎨" NextPalette
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
                            Svg.g [ onClick <| Cell col row ] <|
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
