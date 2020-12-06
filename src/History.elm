module History exposing
    ( Direction(..)
    , History
    , canTravel
    , change
    , steps
    , travel
    )


type Direction
    = Back
    | Forward


type alias History a =
    { now : a
    , future : List a
    , past : List a
    , limit : Int
    }


steps : Int -> a -> History a
steps l x =
    { now = x
    , future = []
    , past = []
    , limit = l
    }


change : (a -> a) -> History a -> History a
change f h =
    let
        new =
            f h.now
    in
    if new == h.now then
        h

    else
        { h
            | now = new
            , past = h.now :: List.take h.limit h.past
            , future = []
        }


travel : Direction -> History a -> Maybe (History a)
travel d h =
    case d of
        Forward ->
            case h.future of
                [] ->
                    Nothing

                x :: xs ->
                    Just
                        { h
                            | future = xs
                            , now = x
                            , past = h.now :: h.past
                        }

        Back ->
            case h.past of
                [] ->
                    Nothing

                x :: xs ->
                    Just
                        { h
                            | past = xs
                            , now = x
                            , future = h.now :: h.future
                        }


canTravel : Direction -> History a -> Bool
canTravel d h =
    case d of
        Forward ->
            not <| List.isEmpty h.future

        Back ->
            not <| List.isEmpty h.past
