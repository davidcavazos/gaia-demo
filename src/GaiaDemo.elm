module GaiaDemo exposing (..)

import Dict
import Footprint exposing (Footprint)


{-| Products table.

    get "Endangered species chocolate (bar)"
    --> Just { footprint = {air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }

-}
get : String -> Maybe { footprint : Footprint, requires : List ( String, Float ) }
get name =
    Dict.fromList
        [ ( "Endangered species chocolate (bar)"
          , { footprint = { air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }
          )
        , ( "KitKat (bar)"
          , { footprint = { air = 0, water = 0, land = 0 }, requires = [ ( "Cocoa (g)", 83.82 ), ( "Sugar (g)", 43.18 ) ] }
          )
        , ( "Cocoa (g)"
          , { footprint = { air = 0, water = 17, land = 0.02 }, requires = [] }
          )
        , ( "Sugar (g)"
          , { footprint = { air = 0, water = 17, land = 0.02 }, requires = [] }
          )
        ]
        |> Dict.get name


{-| Collect the footprint for a single unit of a product (recursive).

    collect "Cocoa (g)" --> Just { air = 0, water = 17, land = 0.02 }

    collect "Endangered species chocolate (bar)" --> Just { air = 0, water = 1496.0, land = 1.76 }

-}
collect : String -> Maybe Footprint
collect productName =
    Maybe.andThen
        (\{ footprint, requires } ->
            List.foldl
                (Maybe.map2 (Footprint.map2 (+)))
                (Just footprint)
                (List.map collectMany requires)
        )
        (get productName)


{-| Collect the footprint for multiple units of a product (recursive).

    collectMany ( "Cocoa (g)", 2 ) --> Just { air = 0, water = 34, land = 0.04 }

-}
collectMany : ( String, Float ) -> Maybe Footprint
collectMany ( productName, units ) =
    Maybe.map (Footprint.map ((*) units))
        (collect productName)



-- Score microservices


{-| Gaia ratio for a footprint.

    collect "Endangered species chocolate (bar)" |> Maybe.andThen gaiaRatio --> Just 0.5891925720026621

    collect "KitKat (bar)" |> Maybe.andThen gaiaRatio --> Just 0.49844456650070046

-}
gaiaRatio : Footprint -> Maybe Float
gaiaRatio footprint =
    Maybe.map (\cost -> 1 / (1 + cost))
        (footprintCost footprint)


{-| Calculate the footprint cost in dollars.

    collect "Endangered species chocolate (bar)" |> Maybe.andThen footprintCost --> Just 0.6972379617770906

    collect "KitKat (bar)" |> Maybe.andThen footprintCost --> Just 1.0062411493828467

-}
footprintCost : Footprint -> Maybe Float
footprintCost { air, water, land } =
    Maybe.map3 (\x y z -> x + y + z)
        (airCost air)
        (waterCost water)
        (landCost land)



-- Footprint cost microservices


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
