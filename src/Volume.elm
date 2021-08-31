module Volume exposing (..)


type Volume
    = Liters Float


map : (Float -> Float) -> Volume -> Volume
map f (Liters a) =
    Liters (f a)


map2 : (Float -> Float -> Float) -> Volume -> Volume -> Volume
map2 f (Liters a) (Liters b) =
    Liters (f a b)
