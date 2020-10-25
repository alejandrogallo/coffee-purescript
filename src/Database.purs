module Database where

import Data.Monoid (class Monoid, class Semigroup)
import Prelude ((*), (+), (/))

type Price = Number
data Coffees = Coffees Price Number

instance coffeeSemigroup ∷ Semigroup Coffees where
  append (Coffees p1 a1) (Coffees p2 a2) = Coffees newPrice newAmount
    where newPrice = (p1 * a1 + p2 * a2) / newAmount
          newAmount = a1 + a2

instance coffeeMonoid ∷ Monoid Coffees where
  mempty = Coffees 0.0 0.0

type User
  = { name ∷ String
    , email ∷ String
    , coffees ∷ Array Coffees
    , saldo ∷ Array Number
    }

getUsers ∷ Array User
getUsers = [ { name: "Fulano"
             , email: "Fulano.mengano@tuwien.ac.at"
             , coffees: [ Coffees 0.3 10.0
                        , Coffees 0.3 10.0
                        ]
             , saldo: [ 10.0
                      ]
             }
           , { name: "Alejandro"
             , email: "alejandro.gallo@tuwien.ac.at"
             , coffees: [ Coffees 0.3 10.0
                        , Coffees 0.3 10.0
                        ]
             , saldo: [ 10.0
                      ]
             }
           ]
