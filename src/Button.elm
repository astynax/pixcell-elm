module Button exposing
    ( Button
    , activate
    , bind
    , textButton
    , colorButton
    , imageButton
    , disable
    , dx
    , dy
    , hbox
    , vbox
    , viewButton
    )

import Color exposing (Color)
import String
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (..)
import Svg.Events exposing (onClick)


type Type
    = Text String
    | Color Color
    | Image String


type alias Button a =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , msg : a
    , state : State
    , type_ : Type
    }


type State
    = Normal
    | Active
    | Disabled


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


vbox :
    List { a | y : Int, height : Int }
    -> List { a | y : Int, height : Int }
vbox items =
    case items of
        [] ->
            []

        first :: rest ->
            (::) first <|
                Tuple.second <|
                    List.foldl vboxFolder ( first.height, [] ) rest


vboxFolder :
    { a | y : Int, height : Int }
    -> ( Int, List { a | y : Int, height : Int } )
    -> ( Int, List { a | y : Int, height : Int } )
vboxFolder item ( v, acc ) =
    ( v + item.height, dy v item :: acc )


dx : Int -> { a | x : Int } -> { a | x : Int }
dx v i =
    { i | x = i.x + v }


dy : Int -> { a | y : Int } -> { a | y : Int }
dy v i =
    { i | y = i.y + v }


textButton : String -> a -> Button a
textButton t m =
    { x = 0
    , y = 0
    , width = 10
    , height = 10
    , msg = m
    , state = Normal
    , type_ = Text t
    }


colorButton : Color -> a -> Button a
colorButton c m =
    let
        b =
            textButton "" m
    in
    { b | type_ = Color c }

imageButton : String -> a -> Button a
imageButton href m =
    let
        b =
            textButton "" m
    in
    { b | type_ = Image href }


activate : Bool -> Button a -> Button a
activate a b =
    if a then
        { b | state = Active }

    else
        b


disable : Bool -> Button a -> Button a
disable d b =
    if d then
        { b | state = Disabled }

    else
        b


viewButton : Button a -> Svg a
viewButton b =
    Svg.g
        (if b.state /= Disabled then
            [ style "cursor: pointer;"
            , onClick b.msg
            ]

         else
            []
        )
    <|
        List.concat
            [ [ Svg.rect
                    [ set x b.x
                    , set y b.y
                    , set width b.width
                    , set height b.height
                    , fill <|
                        case b.state of
                            Normal ->
                                "#404040"

                            Active ->
                                "gray"

                            Disabled ->
                                "#606060"
                    ]
                    []
              , Svg.rect
                    [ set x b.x
                    , set y b.y
                    , set width <| b.width - 1
                    , set height <| b.height - 1
                    , fill <|
                        case b.state of
                            Normal ->
                                "gray"

                            Active ->
                                "black"

                            Disabled ->
                                "#606060"
                    ]
                    []
              , Svg.rect
                    [ set x <| b.x + 1
                    , set y <| b.y + 1
                    , set width <| b.width - 2
                    , set height <| b.height - 2
                    , fill "#606060"
                    ]
                    []
              ]
            , case b.type_ of
                Color c ->
                    [ Svg.rect
                        [ set x <| b.x + 2
                        , set y <| b.y + 2
                        , set width <| b.width - 4
                        , set height <| b.height - 4
                        , fill <| Color.toCssString c
                        ]
                        []
                    ]

                Text t ->
                    [ Svg.text_
                        [ bind x .x <| dx (b.width // 2) b
                        , bind y .y <| dy 7 b
                        , fill <|
                            case b.state of
                                Normal ->
                                    "white"

                                Active ->
                                    "white"

                                Disabled ->
                                    "gray"
                        , fontSize <|
                            String.append (String.fromInt <| b.height - 4)
                                "px"
                        , textAnchor "middle"
                        ]
                        [ Svg.text t ]
                    ]

                Image p ->
                    [ Svg.image
                        [ set x <| b.x + 2
                        , set y <| b.y + 2
                        , set width <| b.width - 4
                        , set height <| b.height - 4
                        , xlinkHref p
                        ]
                        []
                    ]
            ]


bind : (String -> Svg.Attribute m) -> (b -> Int) -> b -> Svg.Attribute m
bind f g =
    f << String.fromInt << g


set : (String -> Svg.Attribute m) -> Int -> Svg.Attribute m
set f =
    bind f identity
