module Currency exposing (..)


type Currency
    = USD Float


toUSD : Currency -> Float
toUSD (USD usd) =
    usd


map : (Float -> Float) -> Currency -> Currency
map f (USD a) =
    USD (f a)


map2 : (Float -> Float -> Float) -> Currency -> Currency -> Currency
map2 f (USD a) (USD b) =
    USD (f a b)


map3 : (Float -> Float -> Float -> Float) -> Currency -> Currency -> Currency -> Currency
map3 f (USD a) (USD b) (USD c) =
    USD (f a b c)
