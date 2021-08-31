module Area exposing (..)


type Area
    = SquareMeters Float


map : (Float -> Float) -> Area -> Area
map f (SquareMeters a) =
    SquareMeters (f a)


map2 : (Float -> Float -> Float) -> Area -> Area -> Area
map2 f (SquareMeters a) (SquareMeters b) =
    SquareMeters (f a b)
