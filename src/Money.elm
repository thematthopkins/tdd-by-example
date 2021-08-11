module Money exposing
    ( Expression(..)
    , Money
    , Rate(..)
    , dollar
    , franc
    , rate
    , reduce
    , times
    )


type alias Currency =
    String


type Money
    = Money Currency Int


franc : Int -> Money
franc =
    Money "CHF"


dollar : Int -> Money
dollar =
    Money "USD"


times : Int -> Money -> Money
times scalar (Money currency amount) =
    Money currency (amount * scalar)


type Expression
    = Plus Expression Expression
    | MoneyExpr Money
    | Times Int Expression


rate : Currency -> Currency -> List Rate -> Maybe Float
rate from to rates =
    if from == to then
        Just 1

    else
        rates
            |> List.filterMap
                (\(Rate rateFrom rateTo rateRate) ->
                    if rateFrom == from && rateTo == to then
                        Just rateRate

                    else
                        Nothing
                )
            |> List.head


reduce : Currency -> List Rate -> Expression -> Maybe Money
reduce currency rates e =
    case e of
        Plus a b ->
            Maybe.map2
                (\(Money _ aMoney) (Money _ bMoney) ->
                    Money currency
                        (aMoney + bMoney)
                )
                (reduce currency rates a)
                (reduce currency rates b)

        Times f expr ->
            Maybe.map
                (\(Money _ exprMoney) ->
                    Money currency
                        (exprMoney * f)
                )
                (reduce currency rates expr)

        MoneyExpr (Money fromCurrency a) ->
            rate fromCurrency currency rates
                |> Maybe.map
                    (\foundRate ->
                        Money currency (round (toFloat a / foundRate))
                    )


type Rate
    = Rate Currency Currency Float
