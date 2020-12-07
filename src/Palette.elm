module Palette exposing (Palette, init, next)

import Color exposing (Color, rgb255)


type alias Palette =
    { current : List Color
    , other : List (List Color)
    }


init : Palette
init =
    { current = ega
    , other =
        [ commodore
        , appleII
        ]
    }


next : Palette -> Palette
next p =
    case p.other of
        [] ->
            p

        x :: xs ->
            { current = x, other = List.append xs [ p.current ] }


ega : List Color
ega =
    [ rgb255 0x00 0x00 0x00 -- black
    , rgb255 0x00 0x00 0xAA -- log  blue
    , rgb255 0x00 0xAA 0x00 -- low  green
    , rgb255 0x00 0xAA 0xAA -- low  cyan
    , rgb255 0xAA 0x00 0x00 -- low  red
    , rgb255 0xAA 0x00 0xAA -- low  magenta
    , rgb255 0xAA 0xAA 0x00 -- brown
    , rgb255 0x7F 0x7F 0x7F -- light gray
    , rgb255 0x55 0x55 0x55 -- dark gray
    , rgb255 0x55 0x55 0xFF -- high blue
    , rgb255 0x55 0xFF 0x55 -- high green
    , rgb255 0x55 0xFF 0xFF -- high cyan
    , rgb255 0xFF 0x55 0x55 -- high red
    , rgb255 0xFF 0x55 0xFF -- high magenta
    , rgb255 0xFF 0xFF 0x55 -- yellow
    , rgb255 0xFF 0xFF 0xFF -- white
    ]


commodore : List Color
commodore =
    [ rgb255 0x00 0x00 0x00 -- black
    , rgb255 0x8B 0x54 0x29 -- orange
    , rgb255 0xFF 0xFF 0xFF -- white
    , rgb255 0x57 0x42 0x00 -- brown*
    , rgb255 0x88 0x39 0x32 -- red
    , rgb255 0xB8 0x69 0x62 -- light red
    , rgb255 0x67 0xB6 0xBD -- cyan
    , rgb255 0x50 0x50 0x50 -- dark grey
    , rgb255 0x8B 0x3F 0x96 -- purple
    , rgb255 0x78 0x78 0x78 -- grey
    , rgb255 0x55 0xA0 0x49 -- green
    , rgb255 0x94 0xE0 0x89 -- light green
    , rgb255 0x40 0x31 0x8D -- blue
    , rgb255 0x78 0x69 0xC4 -- light blue
    , rgb255 0xBF 0xCE 0x72 -- yellow
    , rgb255 0x9F 0x9F 0x9F -- light grey
    ]


appleII : List Color
appleII =
    [ rgb255 0x00 0x00 0x00 -- black
    , rgb255 0x72 0x26 0x40 -- red
    , rgb255 0x40 0x33 0x7F -- dark blue
    , rgb255 0xE4 0x34 0xFE -- purple
    , rgb255 0x0E 0x59 0x40 -- dark green
    , rgb255 0x80 0x80 0x80 -- gray
    , rgb255 0x1B 0x9A 0xFE -- blue-cyan
    , rgb255 0xBF 0xB3 0xFF -- light blue
    , rgb255 0x40 0x4C 0x00 -- brown
    , rgb255 0xE4 0x65 0x01 -- orange
    , rgb255 0x80 0x80 0x80 -- gray
    , rgb255 0xF1 0xA6 0xBF -- pink
    , rgb255 0x1B 0xCB 0x01 -- bright green
    , rgb255 0xBF 0xCC 0x80 -- yellow
    , rgb255 0x8D 0xD9 0xBF -- cyan
    , rgb255 0xFF 0xFF 0xFF -- white
    ]
