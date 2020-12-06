module Grid exposing (Grid, from, map, set, encodeUsing)

import Color exposing (Color)


type alias Grid a =
    List (List a)


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
