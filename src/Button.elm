module Button exposing
    ( Button
    , activate
    , bind
    , button
    , colorButton
    , disable
    , dx
    , dy
    , hbox
    , viewButton
    )

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
    , enabled : Bool
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
    , msg = m
    , active = False
    , enabled = True
    , width = w
    , text = Just t
    , color = Nothing
    }


colorButton : Color -> a -> Button a
colorButton c m =
    { x = 0
    , y = 0
    , width = 10
    , msg = m
    , active = False
    , enabled = True
    , text = Nothing
    , color = Just c
    }


activate : Bool -> Button a -> Button a
activate a b =
    { b | active = a }


disable : Bool -> Button a -> Button a
disable d b =
    { b | enabled = not d }


viewButton : Button a -> Svg a
viewButton b =
    Svg.g
        (if b.enabled then
            [ style "cursor: pointer;"
            , onClick b.msg
            ]

         else
            []
        )
    <|
        List.append
            [ Svg.rect
                [ bind x .x b
                , bind y .y b
                , width <| Debug.toString b.width
                , height "10"
                , fill <|
                    if b.active && b.enabled then
                        "gray"

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
                    if b.active || not b.enabled then
                        "black"

                    else
                        "gray"
                ]
                []
            , Svg.rect
                [ bind x .x <| dx 1 b
                , bind y .y <| dy 1 b
                , width <| Debug.toString <| b.width - 2
                , height "8"
                , fill <| if b.enabled then "#606060" else "black"
                ]
                []
            , Svg.rect
                [ bind x .x <| dx 2 b
                , bind y .y <| dy 2 b
                , width "6"
                , height "6"
                , fill <|
                    Maybe.withDefault "none" <|
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
                            , fill <| if b.enabled then "white" else "gray"
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
