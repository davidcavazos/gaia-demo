module GaiaDemo exposing (..)

import Dict


type alias Footprint =
    { air : Float --   cubic meters per unit
    , water : Float -- liters per unit
    , land : Float --  square meters per unit
    }


{-| Products table.

    get "Endangered species chocolate (bar)"
    --> Just { footprint = {air = 0, water = 0, land = 0 }, price = 5.0, requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ] }

-}
get : String -> Maybe { footprint : Footprint, price : Float, requires : List ( String, Float ) }
get name =
    Dict.fromList
        [ ( "Endangered species chocolate (bar)"
          , { footprint = { air = 0, water = 0, land = 0 }
            , price = 5.0
            , requires = [ ( "Cocoa (g)", 74.8 ), ( "Sugar (g)", 13.2 ) ]
            }
          )
        , ( "KitKat (bar)"
          , { footprint = { air = 0, water = 0, land = 0 }
            , price = 1.5
            , requires = [ ( "Cocoa (g)", 83.82 ), ( "Sugar (g)", 43.18 ) ]
            }
          )
        , ( "Cocoa (g)", { footprint = { air = 0, water = 17, land = 0.02 }, requires = [], price = 0 } )
        , ( "Sugar (g)", { footprint = { air = 0, water = 17, land = 0.02 }, requires = [], price = 0 } )
        ]
        |> Dict.get name


{-| Collect the footprint for a single unit of a product (recursive).

    collect "Cocoa (g)" --> Just { footprint = { air = 0, water = 17, land = 0.02 }, price = 0 }

    collect "Endangered species chocolate (bar)" --> Just { footprint = { air = 0, water = 1496.0, land = 1.76 }, price = 5.0 }

-}
collect : String -> Maybe { footprint : Footprint, price : Float }
collect productName =
    Maybe.andThen
        (\{ footprint, price, requires } ->
            List.foldl
                (Maybe.map2 (\x y -> { air = x.air + y.air, water = x.water + y.water, land = x.land + y.land }))
                (Just footprint)
                (List.map collectMany requires |> List.map (Maybe.map .footprint))
                |> Maybe.map (\fp -> { footprint = fp, price = price })
        )
        (get productName)


{-| Collect the footprint for multiple units of a product (recursive).

    collectMany ( "Cocoa (g)", 2 ) --> Just { footprint = { air = 0, water = 34, land = 0.04 }, price = 0 }

-}
collectMany : ( String, Float ) -> Maybe { footprint : Footprint, price : Float }
collectMany ( productName, units ) =
    Maybe.map
        (\{ footprint, price } ->
            { footprint = { air = footprint.air * units, water = footprint.water * units, land = footprint.land * units }
            , price = price
            }
        )
        (collect productName)



-- Score microservices


{-| Gaia ratio for a product.

    collect "Endangered species chocolate (bar)"
        |> Maybe.andThen gaiaRatio
    --> Just 0.860552407644582

    collect "KitKat (bar)"
        |> Maybe.andThen gaiaRatio
    --> Just 0.3291725670781022

-}
gaiaRatio : { footprint : Footprint, price : Float } -> Maybe Float
gaiaRatio { footprint, price } =
    Maybe.map (\cost -> (price - cost) / price)
        (footprintCost footprint)



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


footprintCost : Footprint -> Maybe Float
footprintCost { air, water, land } =
    Maybe.map3 (\x y z -> x + y + z)
        (airCost air)
        (waterCost water)
        (landCost land)
