module Components.List where

import Database

import Data.Functor ((<$>))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (show, ($))

type State = Array User

totalCoffees ∷ User -> Number
totalCoffees u = 12.0

effectiveSaldo ∷ User -> Number
effectiveSaldo u = 12.0


renderEntry ∷ ∀ m a. User -> H.ComponentHTML a () m
renderEntry u = HH.li
                 [ HP.classes [ H.ClassName "list-group-item d-flex justify-content-between align-items-center" ] ]
                 [ HH.text u.name
                 , HH.span
                    [ HP.classes [ H.ClassName "fa fa-coffee badge badge-success badge-pill" ] ]
                    [ HH.text $ show $ totalCoffees u ]
                 , HH.span
                    [ HP.classes [ H.ClassName "fa fa-euro-sign badge badge-primary badge-pill" ] ]
                    [ HH.text $ show $ effectiveSaldo u ]
                 ]

render ∷ ∀ m a. State → H.ComponentHTML a () m
render s = HH.ul
            [ HP.classes [ H.ClassName "list-group" ] ]
            (renderEntry <$> s)
