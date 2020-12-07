module Grid exposing
    ( Grid
    , Transformation(..)
    , apply
    , encodeUsing
    , from
    , map
    , set
    )

import Color exposing (Color)


type alias Grid a =
    List (List a)


type Transformation
    = ScrollR
    | ScrollL
    | ScrollU
    | ScrollD
    | FlipH
    | FlipV
    | Rotate
    | ReflectH
    | ReflectV
    | ReflectQ
    | ReflectR
    | Cycle


from : Int -> Grid Int
from =
    List.repeat 16 << List.repeat 16


map : (a -> b) -> Grid a -> Grid b
map =
    List.map << List.map


set : a -> Int -> Int -> Grid a -> Grid a
set v x y =
    at y <| at x <| always v


at : Int -> (a -> a) -> List a -> List a
at n f l =
    List.append (List.take n l) <|
        case List.drop n l of
            [] ->
                []

            x :: xs ->
                f x :: xs


apply : Transformation -> Grid Int -> Grid Int
apply t =
    case t of
        ScrollL ->
            List.map scroll

        ScrollR ->
            List.map scrollBack

        ScrollU ->
            scroll

        ScrollD ->
            scrollBack

        FlipH ->
            List.map List.reverse

        FlipV ->
            List.reverse

        Rotate ->
            rotate

        ReflectH ->
            List.map reflect

        ReflectV ->
            reflect

        ReflectQ ->
            reflect << List.map reflect

        ReflectR ->
            reflectRotate

        Cycle ->
            map (modBy 16 << (+) 1)


scroll : List a -> List a
scroll l =
    case uncons l of
        Just ( x, xs ) ->
            List.append xs [ x ]

        Nothing ->
            l


scrollBack : List a -> List a
scrollBack =
    List.reverse << scroll << List.reverse


reflect : List a -> List a
reflect l =
    let
        r =
            List.take 8 l
    in
    List.append r <| List.reverse r


reflectRotate : Grid a -> Grid a
reflectRotate g =
    let
        q =
            List.take 8 <| List.map (List.take 8) g

        h =
            List.map2 List.append q <| rotate q
    in
    List.append h <| List.reverse <| List.map List.reverse h


rotate : List (List a) -> List (List a)
rotate =
    List.map List.reverse << transpose


transpose : Grid a -> Grid a
transpose l =
    case conses l of
        Just ( x, xs ) ->
            x :: transpose xs

        Nothing ->
            []


conses : List (List a) -> Maybe ( List a, List (List a) )
conses =
    List.foldr
        (\i acc ->
            Maybe.map2
                (\( x, xs ) ( y, ys ) -> ( x :: y, xs :: ys ))
                (uncons i)
                acc
        )
        (Just ( [], [] ))


uncons : List a -> Maybe ( a, List a )
uncons l =
    Maybe.map2 Tuple.pair (List.head l) (List.tail l)


encodeUsing : b -> List b -> Grid Int -> Grid b
encodeUsing x =
    List.map << encodeRow x


encodeRow : b -> List b -> List Int -> List b
encodeRow x =
    List.map << encodeOne x


encodeOne : b -> List b -> Int -> b
encodeOne x xs i =
    case List.drop i xs of
        [] ->
            x

        v :: _ ->
            v
