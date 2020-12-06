module Button exposing (button, colorButton, dx, dy, hbox, viewButton)

import Color exposing (Color)
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (..)
import Svg.Events exposing (onClick)


type alias Button a =
    { x : Int
    , y : Int
    , width : Int
    , msg : a
    , text : Maybe String
    , color : Maybe Color
    , active : Bool
    }


hbox :
    List { a | x : Int, width : Int }
    -> List { a | x : Int, width : Int }
hbox items =
    case items of
        [] ->
            []

        first :: rest ->
            (::) first <|
                Tuple.second <|
                    List.foldl hboxFolder ( first.width, [] ) rest


hboxFolder :
    { a | x : Int, width : Int }
    -> ( Int, List { a | x : Int, width : Int } )
    -> ( Int, List { a | x : Int, width : Int } )
hboxFolder item ( v, acc ) =
    ( v + item.width, dx v item :: acc )


dx : Int -> { a | x : Int } -> { a | x : Int }
dx v i =
    { i | x = i.x + v }


dy : Int -> { a | y : Int } -> { a | y : Int }
dy v i =
    { i | y = i.y + v }


button : String -> Int -> a -> Button a
button t w m =
    { x = 0
    , y = 0
    , width = w
    , text = Just t
    , msg = m
    , color = Nothing
    , active = False
    }


colorButton : Bool -> Color -> a -> Button a
colorButton a c m =
    { x = 0
    , y = 0
    , width = 10
    , text = Nothing
    , msg = m
    , color = Just c
    , active = a
    }


viewButton : Button a -> Svg a
viewButton b =
    Svg.map (always b.msg) <|
        Svg.g
            [ style "cursor: pointer;"
            , onClick b.msg
            ]
        <|
            List.append
                [ Svg.rect
                    [ bind x .x b
                    , bind y .y b
                    , width <| Debug.toString b.width
                    , height "10"
                    , fill <|
                        if b.active then
                            "white"

                        else
                            "black"
                    ]
                    []
                , Svg.rect
                    [ bind x .x b
                    , bind y .y b
                    , width <| Debug.toString <| b.width - 1
                    , height "9"
                    , fill <|
                        if b.active then
                            "none"

                        else
                            "gray"
                    ]
                    []
                , Svg.rect
                    [ bind x .x <| dx 1 b
                    , bind y .y <| dy 1 b
                    , width <| Debug.toString <| b.width - 2
                    , height "8"
                    , fill <|
                        Maybe.withDefault "#404040" <|
                            Maybe.map Color.toCssString b.color
                    ]
                    []
                ]
            <|
                Maybe.withDefault [] <|
                    Maybe.map
                        (\t ->
                            [ Svg.text_
                                [ bind x .x <| dx (b.width // 2) b
                                , bind y .y <| dy 7 b
                                , fill "white"
                                , fontSize "6px"
                                , textAnchor "middle"
                                ]
                                [ Svg.text t ]
                            ]
                        )
                        b.text


bind : (String -> Svg.Attribute m) -> (b -> a) -> b -> Svg.Attribute m
bind f g =
    f << Debug.toString << g
