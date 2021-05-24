import Communication (JsonTask (..))
import Data.Aeson
import Data.Aeson.Types (Pair)
import Task (Editor (..), Name (..), Task (..))
import Test.Hspec

main :: IO ()
main =
  hspec <| do
    describe "Communication" <| do
      it "encodes a Done task to JSON" <| do
        encode (JsonTask (Done ()) [])
          `shouldBe` encode
            (taskObject ["type" .= String "done"])

      it "encodes an Update editor to JSON" <| do
        encode (JsonTask (Edit (Named 1) (Update (5 :: Int))) [])
          `shouldBe` encode
            ( taskObject
                [ "type" .= String "edit",
                  "editor"
                    .= object
                      [ "type" .= String "update",
                        "value" .= Number 5
                      ],
                  "name" .= Number 1
                ]
            )

taskObject :: [Pair] -> Value
taskObject task =
  object
    [ "inputs" .= ([] :: [Value]),
      "task" .= object task
    ]
