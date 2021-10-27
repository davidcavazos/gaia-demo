module Resources exposing (..)


type alias Resources =
    { air : Float --    carbon emissions per unit (liters)
    , water : Float --  water used per unit (liters)
    , land : Float --   land used per unit (square meters)
    }


{-| Calculate the resources footprint cost.

    Just (cost { air = 0, water = 1496.0, land = 1.76 }) --> Just 0.6972379617770906

-}
cost : Resources -> Float
cost { air, water, land } =
    airCost air + waterCost water + landCost land


{-| Air resource cost.
-}
airCost : Float -> Float
airCost _ =
    0


{-| Water resource cost.

    Just (waterCost 1) --> Just 0.00040211373832598743

-}
waterCost : Float -> Float
waterCost liters =
    let
        dollarsPerLiter =
            -- dollarsPerAcreFoot / litersPerAcreFoot
            496 / 1233481.85532
    in
    liters * dollarsPerLiter


{-| Land resource cost.

    Just (landCost 1) --> Just 0.05436125525080306

-}
landCost : Float -> Float
landCost squareMeters =
    let
        dollarsPerSquareMeter =
            -- dollarsPerAcre / squareMetersPerAcre
            220 / 4047
    in
    squareMeters * dollarsPerSquareMeter


map : (Float -> Float) -> Resources -> Resources
map f a =
    { air = f a.air
    , water = f a.water
    , land = f a.land
    }


map2 : (Float -> Float -> Float) -> Resources -> Resources -> Resources
map2 f a b =
    { air = f a.air b.air
    , water = f a.water b.water
    , land = f a.land b.land
    }
