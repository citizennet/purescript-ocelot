module Test.Ocelot.Typeahead (suite) where

import Prelude
import Data.Array as Data.Array
import Data.Fuzzy as Data.Fuzzy
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Foreign.Object as Foreign.Object
import Ocelot.Typeahead as Ocelot.Typeahead
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
  
suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Ocelot.Typeahead" do
    Test.Unit.suite "getNewItems'" do
      Test.Unit.test "filters out selected Array items and poor matches, and sorts results" do
        let
          actual =
            Ocelot.Typeahead.getNewItems'
              { insertable: Ocelot.Typeahead.NotInsertable 
              , itemToObject: Foreign.Object.fromHomogeneous <<< { name: _ }
              , runFilterFuzzy: Ocelot.Typeahead.defFilterFuzzy
              , runFilterItems: Data.Array.difference 
              , search: "foo"
              , selected: ["foo", "bar"]
              }
              ["foo", "food", "boof", "fooboo", "baz"]

          expected = ["food", "fooboo"]
        unfuzzyTestResults { actual, expected }
      Test.Unit.test "filters out selected Maybe item and poor matches, and sorts results" do
        let
          actual =
            Ocelot.Typeahead.getNewItems'
              { insertable: Ocelot.Typeahead.NotInsertable 
              , itemToObject: Foreign.Object.fromHomogeneous <<< { name: _ }
              , runFilterFuzzy: Ocelot.Typeahead.defFilterFuzzy
              , runFilterItems: \items -> Data.Maybe.maybe items (\item -> Data.Array.filter (_ /= item) items)
              , search: "foo"
              , selected: Just "foo"
              }
              ["foo", "food", "boof", "fooboo", "baz"]

          expected = ["food", "fooboo"]
        unfuzzyTestResults { actual, expected }
      Test.Unit.test "doesn't filter poor matches or sort results" do
        let
          actual =
            Ocelot.Typeahead.getNewItems'
              { insertable: Ocelot.Typeahead.NotInsertable 
              , itemToObject: Foreign.Object.fromHomogeneous <<< { name: _ }
              , runFilterFuzzy: identity
              , runFilterItems: Data.Array.difference 
              , search: "foo"
              , selected: ["foo", "bar"]
              }
              ["foo", "food", "boof", "fooboo", "baz"]

          expected = ["food", "boof", "fooboo", "baz"]
        unfuzzyTestResults { actual, expected }
      Test.Unit.test "inserts search to results without a perfect match" do
        let
          actual =
            Ocelot.Typeahead.getNewItems'
              { insertable: Ocelot.Typeahead.Insertable identity
              , itemToObject: Foreign.Object.fromHomogeneous <<< { name: _ }
              , runFilterFuzzy: Ocelot.Typeahead.defFilterFuzzy
              , runFilterItems: \items _ -> items
              , search: "foo"
              , selected: Nothing 
              }
              ["food"]

          expected = ["foo", "food"]
        unfuzzyTestResults { actual, expected }
      Test.Unit.test "doesn't insert search into non-empty results" do
        let
          actual  =
            Ocelot.Typeahead.getNewItems'
              { insertable: Ocelot.Typeahead.Insertable identity
              , itemToObject: Foreign.Object.fromHomogeneous <<< { name: _ }
              , runFilterFuzzy: Ocelot.Typeahead.defFilterFuzzy
              , runFilterItems: \items _ -> items
              , search: "foo"
              , selected: Nothing 
              }
              ["foo", "food"]

          expected = ["foo", "food"]
        unfuzzyTestResults { actual, expected }

unfuzzyTestResults ::
  { actual :: Array (Data.Fuzzy.Fuzzy String)
  , expected :: Array String
  } ->
  Test.Unit.Test
unfuzzyTestResults config = Test.Unit.Assert.equal config.expected actual
  where
  actual :: Array String
  actual = map (_.original <<< Data.Newtype.unwrap) config.actual
