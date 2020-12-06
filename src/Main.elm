module Main exposing (main)

import Browser
import Button exposing (..)
import Color exposing (Color, rgb255)
import Grid exposing (Grid)
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (..)
import Svg.Events exposing (onClick)


type alias Model =
    { grid : Grid Int
    , color : Int
    , guides : Bool
    }


type Msg
    = Cell Int Int
    | Color Int
    | ToggleGuides
    | Nop


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
    ( { grid = Grid.from 0
      , color = 0
      , guides = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        ToggleGuides ->
            ( { model
                | guides = not model.guides
              }
            , Cmd.none
            )

        Color x ->
            ( { model
                | color = x
              }
            , Cmd.none
            )

        Cell x y ->
            ( { model
                | grid = Grid.set model.color x y model.grid
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "PixCell/Elm v1.0"
    , body =
        [ Svg.svg [ width "640", height "720", viewBox "0 0 160 180" ]
            [ Svg.rect
                [ x "0"
                , y "0"
                , width "160"
                , height "180"
                , fill "none"
                , stroke "fuchsia"
                ]
                []
            , editorView model
            ]
        ]
    }


editorView : Model -> Svg Msg
editorView model =
    Svg.g []
        [ tools model
        , grid model.guides <|
            Grid.encodeUsing Color.black egaColors <|
                model.grid
        , palette <| model.color
        ]


tools : Model -> Svg Msg
tools model =
    Svg.g [] <|
        List.map viewButton <|
            hbox <|
                [ button "🗋 New" 30 Nop
                , button "🖌 Fill" 25 Nop
                , button "⚄ Rnd" 25 Nop
                , button "🎨" 10 Nop
                , button "⇄" 10 Nop
                , button "⇅" 10 Nop
                , button "▟" 10 Nop
                , button "▐" 10 Nop
                , button "▄" 10 Nop
                , button "⥁" 10 Nop
                , let
                    b =
                        button "#" 10 ToggleGuides
                  in
                  { b | active = model.guides }
                ]


palette : Int -> Svg Msg
palette current =
    Svg.g [] <|
        List.map viewButton <|
            hbox <|
                List.indexedMap
                    (\idx c ->
                        Button.dy 170 <|
                            colorButton (idx == current) c (Color idx)
                    )
                    egaColors


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
                                        Button.dx 1 <|
                                            Button.dy 1 <|
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


egaColors : List Color
egaColors =
    [ rgb255 0 0 0
    , rgb255 0 0 127
    , rgb255 0 127 0
    , rgb255 0 127 127
    , rgb255 127 0 0
    , rgb255 127 0 127
    , rgb255 127 127 0
    , rgb255 127 127 127
    , rgb255 64 64 64
    , rgb255 0 0 255
    , rgb255 0 255 0
    , rgb255 0 255 255
    , rgb255 255 0 0
    , rgb255 255 0 255
    , rgb255 255 255 0
    , rgb255 255 255 255
    ]
