module Test.Main where


import Prelude

import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import TopLevel (formatDefault)



-- Orphan
-- comment



{-
 Orphan multi-line comment
-}


-- docs comment
typesTogether :: Array String
typesTogether =
  [ "type X = String"

  , "type Y = String"
  ]



-- close to the end comment


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Top level formatting" do
    it "should leave one line types together" do
      let text = joinWith "\n" typesTogether
      let expected = text <> "\n"
      formatDefault text `shouldEqual` expected

    it "should put two lines between split one line types" do
      let text = joinWith "\n\n" typesTogether
      let expected = joinWith "\n\n\n" typesTogether <> "\n"
      formatDefault text `shouldEqual` expected
