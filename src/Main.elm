module Main exposing (main)

import Browser
import Color exposing (Color, rgb255)
import Grid exposing (Grid)
import Svg exposing (Svg)
import Svg.Events exposing (onClick)
import Svg.Attributes as Attr exposing (..)
import Button exposing (..)


type alias Model =
    { grid : Grid Int
    , color : Int
    }


type Msg
    = Cell Int Int
    | Color Int
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
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Color x ->
            ( { model | color = x }, Cmd.none )

        Cell x y ->
            ( { model | grid = Grid.set model.color x y model.grid }, Cmd.none )


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
        [ tools
        , grid <| Grid.encodeUsing Color.black egaColors <| model.grid
        , palette <| model.color
        ]


tools : Svg Msg
tools =
    Svg.g [] <|
        List.map viewButton <|
            hbox <|
                [ button "🗋 New" 30 Nop
                , button "🖌 Fill" 30 Nop
                , button "⚄ Rnd" 30 Nop
                , button "🎨" 10 Nop
                , button "⇄" 10 Nop
                , button "⇅" 10 Nop
                , button "▟" 10 Nop
                , button "▐" 10 Nop
                , button "▄" 10 Nop
                , button "⥁" 10 Nop
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


grid : Grid Color -> Svg Msg
grid g =
    Svg.g [] <|
        List.concat <|
            List.indexedMap
                (\row cols ->
                    List.indexedMap
                        (\col color ->
                            Svg.rect
                                [ x <| Debug.toString <| col * 10
                                , y <| Debug.toString <| 10 + row * 10
                                , width "10"
                                , height "10"
                                , fill <| Color.toCssString color
                                , onClick <| Cell col row
                                ]
                                []
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
