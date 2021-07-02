module Microservice exposing (..)

{-| Functions to become their own microservice.
-}

import Footprint exposing (Footprint)


{-| Gaia ratio for a footprint.

    import Product exposing (totalFootprint)

    gaiaRatio { air = 0, water = 1496.0, land = 1.76 } --> Just 0.5891925720026621

    Maybe.andThen gaiaRatio (totalFootprint "Endangered species chocolate (bar)") --> Just 0.5891925720026621

    Maybe.andThen gaiaRatio (totalFootprint "KitKat (bar)") --> Just 0.49844456650070046

-}
gaiaRatio : Footprint -> Maybe Float
gaiaRatio footprint =
    Maybe.map (\cost -> 1 / (1 + cost))
        (footprintCost footprint)


{-| Calculate the footprint cost in dollars.

    footprintCost { air = 0, water = 1496.0, land = 1.76 } --> Just 0.6972379617770906

-}
footprintCost : Footprint -> Maybe Float
footprintCost { air, water, land } =
    Maybe.map3 (\x y z -> x + y + z)
        (airCost air)
        (waterCost water)
        (landCost land)


{-| Air resource cost.
-}
airCost : Float -> Maybe Float
airCost cubicMeters =
    Just 0


{-| Water resource cost.

    waterCost 1 --> Just 0.00040211373832598743 -- dollars per liter

-}
waterCost : Float -> Maybe Float
waterCost liters =
    let
        dollarsPerLiter =
            -- dollarsPerAcreFoot / litersPerAcreFoot
            496 / 1233481.85532
    in
    Just (liters * dollarsPerLiter)


{-| Land resource cost.

    landCost 1 --> Just 0.05436125525080306 -- dollars per square meter

-}
landCost : Float -> Maybe Float
landCost squareMeters =
    let
        dollarsPerSquareMeter =
            -- dollarsPerAcre / squareMetersPerAcre
            220 / 4047
    in
    Just (squareMeters * dollarsPerSquareMeter)
