module MoneyTest exposing (all)

import Expect
import Money
import Test exposing (..)


all : Test
all =
    describe "Dollar"
        [ test "multiplication 1" <|
            \_ ->
                Money.dollar 5
                    |> Money.times 2
                    |> Expect.equal (Money.dollar 10)
        , test "multiplication 2" <|
            \_ ->
                Money.dollar 5
                    |> Money.times 3
                    |> Expect.equal (Money.dollar 15)
        , test "Franc multiplication 1" <|
            \_ ->
                Money.franc 5
                    |> Money.times 2
                    |> Expect.equal (Money.franc 10)
        , test "Franc multiplication 2" <|
            \_ ->
                Money.franc 5
                    |> Money.times 3
                    |> Expect.equal (Money.franc 15)
        , test "Reduce sum" <|
            \_ ->
                Money.MoneyExpr (Money.dollar 3)
                    |> Money.Plus (Money.MoneyExpr (Money.dollar 4))
                    |> Money.reduce "USD" []
                    |> Expect.equal (Just (Money.dollar 7))
        , test "Reduce Francs to USD" <|
            \_ ->
                Money.MoneyExpr (Money.franc 2)
                    |> Money.reduce "USD" [ Money.Rate "CHF" "USD" 2.0 ]
                    |> Expect.equal (Just (Money.dollar 1))
        , test "Rates" <|
            \_ ->
                Money.rate "USD" "USD" []
                    |> Expect.equal (Just 1.0)
        , test "Mixed Addition" <|
            \_ ->
                Money.MoneyExpr (Money.dollar 5)
                    |> Money.Plus (Money.MoneyExpr (Money.franc 10))
                    |> Money.reduce "USD" [ Money.Rate "CHF" "USD" 2 ]
                    |> Expect.equal (Just (Money.dollar 10))
        , test "Sum Times" <|
            \_ ->
                Money.MoneyExpr (Money.dollar 5)
                    |> Money.Plus (Money.MoneyExpr (Money.franc 10))
                    |> Money.Times 2
                    |> Money.reduce "USD" [ Money.Rate "CHF" "USD" 2 ]
                    |> Expect.equal (Just (Money.dollar 20))
        , test "Plus USD" <|
            \_ ->
                Money.MoneyExpr (Money.dollar 1)
                    |> Money.Plus (Money.MoneyExpr (Money.dollar 1))
                    |> Money.Times 2
                    |> Money.reduce "USD" [ Money.Rate "CHF" "USD" 2 ]
                    |> Expect.equal (Just (Money.dollar 20))
        ]
