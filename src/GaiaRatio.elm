module GaiaRatio exposing (..)

import Resources exposing (Resources)


{-| Gaia ratio for a footprint.

    import Product exposing (totalResources)

    Just (gaiaRatio { air = 0, water = 1496.0, land = 1.76 }) --> Just 0.5891925720026621
    Maybe.map gaiaRatio (totalResources "Endangered species chocolate (bar)") --> Just 0.5891925720026621
    Maybe.map gaiaRatio (totalResources "KitKat (bar)") --> Just 0.49844456650070046

-}
gaiaRatio : Resources -> Float
gaiaRatio resources =
    1 / (1 + Resources.cost resources)
