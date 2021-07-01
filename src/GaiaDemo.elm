module GaiaDemo exposing (..)

import Dict


type alias Product =
    { materials : Materials
    , price : Float
    }


type alias Materials =
    List ( String, Float )


type alias Metrics =
    { cost : Float, price : Float }


type alias Resources =
    { airCubicMeters : Float -- TODO: is this something like carbon emissions?
    , waterLitersPerUnit : Float
    , unitsPerHectare : Float
    }


{-| Products table.

    getProduct "Endangered species chocolate"
    --> Just { materials = [ ( "Cocoa", 74.8 ), ( "Sugar", 13.2 ) ], price = 5.0 }

-}
getProduct : String -> Maybe Product
getProduct name =
    Dict.fromList
        [ ( "Endangered species chocolate"
          , { materials = [ ( "Cocoa", 74.8 ), ( "Sugar", 13.2 ) ], price = 5.0 }
          )
        , ( "KitKat"
          , { materials = [ ( "Cocoa", 83.82 ), ( "Sugar", 43.18 ) ], price = 1.5 }
          )
        ]
        |> Dict.get name


{-| Resources table.

    getMaterialResources "Cocoa"
    --> Just { airCubicMeters = 0, waterLitersPerUnit = 17, unitsPerHectare = 500000 }

-}
getMaterialResources : String -> Maybe Resources
getMaterialResources name =
    Dict.fromList
        [ ( "Cocoa", { airCubicMeters = 0, waterLitersPerUnit = 17, unitsPerHectare = 500 * 1000 } )
        , ( "Sugar", { airCubicMeters = 0, waterLitersPerUnit = 17, unitsPerHectare = 500 * 1000 } )
        ]
        |> Dict.get name



-- Resource microservices


{-| Air resource cost.
-}
airCost : Float -> Maybe Float
airCost air =
    Just 0


{-| Water resource cost.

    waterCost 17 --> Just 0.006835933551541786 -- per gram

-}
waterCost : Float -> Maybe Float
waterCost waterLiters =
    let
        costPerAcreFoot =
            496

        costPerLiter =
            costPerAcreFoot / 1233481.85532
    in
    Just (waterLiters * costPerLiter)


{-| Land resource cost.

    landCost 500000 --> Just 0.001089 -- per gram

-}
landCost : Float -> Maybe Float
landCost yieldPerHectare =
    let
        costPerAcre =
            220

        costPerHectare =
            costPerAcre * 2.475
    in
    Just (1 / yieldPerHectare * costPerHectare)



-- Score microservices


{-| Gaia ratio for a product.

    gaiaRatio "Endangered species chocolate" --> Just 0.8605211694928645

    gaiaRatio "KitKat" --> Just 0.3290222926361288

-}
gaiaRatio : String -> Maybe Float
gaiaRatio productName =
    Maybe.map (\{ cost, price } -> (price - cost) / price)
        (getProduct productName |> Maybe.andThen productMetrics)



-- Internal functions


{-|

    productMetrics { materials = [ ( "Cocoa", 74.8 ), ( "Sugar", 13.2 ) ], price = 5.0 }
    --> Just { cost = 0.6973941525356773, price = 5.0 }

-}
productMetrics : Product -> Maybe Metrics
productMetrics { materials, price } =
    Maybe.map (\cost -> { cost = cost, price = price })
        (materialsCost materials)


{-| Calculate the total materials cost.

    materialsCost [ ( "Cocoa", 74.8 ), ( "Sugar", 13.2 ) ]
    --> Just 0.6973941525356773

-}
materialsCost : Materials -> Maybe Float
materialsCost materials =
    materials
        |> List.map
            (\( material, amount ) ->
                getMaterialResources material
                    |> Maybe.andThen resourcesCost
                    |> Maybe.map ((*) amount)
            )
        |> List.foldl (Maybe.map2 (+)) (Just 0)


{-| Calculates the total resources costs.

    resourcesCost { airCubicMeters = 0, waterLitersPerUnit = 17, unitsPerHectare = 500000 }
    --> Just 0.007924933551541787 -- per gram

-}
resourcesCost : Resources -> Maybe Float
resourcesCost { airCubicMeters, waterLitersPerUnit, unitsPerHectare } =
    Maybe.map3 (\air water land -> air + water + land)
        (airCost airCubicMeters)
        (waterCost waterLitersPerUnit)
        (landCost unitsPerHectare)
