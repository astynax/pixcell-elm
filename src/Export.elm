module Export exposing (toPng)

import Color exposing (Color)
import File.Download
import Image
import Image.Color


toPng : List (List Color) -> Cmd msg
toPng =
    File.Download.bytes "art16x16.png" "image/png"
        << Image.toPng
        << Image.Color.fromList2d
