{-|
Module      : Component.HTML.Form
Description : Module to render input forms.
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : ...
Maintainer  : sample@email.com
Stability   : experimental

Module to render the input forms.
-}

module Component.HTML.Form where

import Prelude
import Component.HTML.Utils (css)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Data.Array ((:))

textInput :: forall a b. Maybe String -> (String -> b) -> HH.HTML a b
textInput value onValueInput =
  let
    baseProperties =
      [ css "input"
      , HP.required true
      , HE.onValueInput onValueInput
      , HP.type_ HP.InputText
      ]

    allProperties = case value of
      Just v -> HP.value v : baseProperties
      Nothing -> baseProperties
  in
    HH.input allProperties

integerInput :: forall a b. Maybe Int -> (Int -> b) -> HH.HTML a b
integerInput value onValueInput =
  let
    baseProperties =
      [ css "input"
      , HP.required true
      , HE.onValueInput \s -> onValueInput (unsafePartial $ fromJust $ fromString s)
      , HP.type_ HP.InputNumber
      ]

    allProperties = case value of
      Just v -> HP.value (show v) : baseProperties
      Nothing -> baseProperties
  in
    HH.input allProperties

booleanInput :: forall a b. Maybe Boolean -> (Boolean -> b) -> HH.HTML a b
booleanInput value onChange =
  let
    getCheckedAttribute booleanType = case value of
      Just v -> [ HP.checked (booleanType == v) ]
      Nothing -> []
  in
    HH.div_
      [ HH.div []
          [ HH.label
              [ css "radio" ]
              [ HH.input
                  ( [ HP.required true
                    , HP.type_ HP.InputRadio
                    , HP.name "radiobuttonTrueFalse"
                    , HE.onChange \_ -> onChange false
                    ]
                      <> getCheckedAttribute false
                  )
              , HH.text " False"
              ]
          ]
      , HH.div []
          [ HH.label
              [ css "radio" ]
              [ HH.input
                  ( [ HP.required true
                    , HP.type_ HP.InputRadio
                    , HP.name "radiobuttonTrueFalse"
                    , HE.onChange \_ -> onChange true
                    ]
                      <> getCheckedAttribute true
                  )
              , HH.text " True"
              ]
          ]
      ]
