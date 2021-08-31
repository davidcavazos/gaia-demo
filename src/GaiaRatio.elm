module GaiaRatio exposing (..)

import Currency
import Resources exposing (Resources)


{-| Gaia ratio for a footprint.

    import Area exposing (Area(..))
    import Product exposing (totalResources)
    import Volume exposing (Volume(..))

    Just (gaiaRatio { air = Liters 0, water = Liters 1496.0, land = SquareMeters 1.76 }) --> Just 0.5891925720026621
    Maybe.map gaiaRatio (totalResources "Endangered species chocolate (bar)") --> Just 0.5891925720026621
    Maybe.map gaiaRatio (totalResources "KitKat (bar)") --> Just 0.49844456650070046

-}
gaiaRatio : Resources -> Float
gaiaRatio resources =
    let
        cost : Float
        cost =
            Resources.cost resources |> Currency.toUSD
    in
    1 / (1 + cost)
