module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
--import Data.Monoid
--import Effect.Console (log, logShow)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Components.List as CL
import Database as DB

type State = { enabled :: Boolean
             , counter :: Int
             , users :: CL.State
             }

data Action = Toggle

component :: ∀ q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: ∀ i. i → State
initialState _ = { enabled: false
                 , counter: 0
                 , users: DB.getUsers
                 }

renderButton :: ∀ m. State → H.ComponentHTML Action () m
renderButton state =
  let
    label = if state.enabled then "On" else "Off"
    className = if state.enabled then "btn-success" else "btn-warning"
  in
    HH.div
      []
      [ HH.p
        [HP.classes [H.ClassName "alert alert-success"]]
        [ HH.text $ "Times clicked: " <> show state.counter ]
      , HH.button
        [ HP.title label
        , HP.classes [ H.ClassName "btn",  H.ClassName className ]
        , HE.onClick \_ -> Just Toggle
        ]
        [ HH.text $ " It is " <> label ]
      ]

render :: ∀ m. State → H.ComponentHTML Action () m
render state =
  HH.div
    []
    [ (CL.render state.users)
    , renderButton state
    ]

handleAction :: ∀ o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle →
    H.modify_ \st → st { enabled = not st.enabled, counter = st.counter + 1 }

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI (component) unit body
