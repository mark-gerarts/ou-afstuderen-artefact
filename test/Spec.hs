import Communication (JsonTask (..))
import Data.Aeson
import Data.Aeson.Types (Pair)
import Task (Editor (..), Name (..), Task (..))
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main =
  hspec <| do
    describe "Communication" <| do
      it "encodes a Done task to JSON" <| do
        encode (JsonTask (Done ()) [])
          `shouldBe` encode
            (taskObject ["type" .= String "done"])

      prop "encodes an Update editor to JSON" <| do
        \name value ->
          encode (JsonTask (Edit (Named name) (Update (value :: Int))) [])
            `shouldBe` encode
              ( taskObject
                  [ "type" .= String "edit",
                    "editor"
                      .= object
                        [ "type" .= String "update",
                          "value" .= value
                        ],
                    "name" .= name
                  ]
              )

taskObject :: [Pair] -> Value
taskObject task =
  object
    [ "inputs" .= ([] :: [Value]),
      "task" .= object task
    ]
