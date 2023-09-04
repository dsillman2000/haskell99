module TestHaskell20 (tests20) where
import Test.Tasty (TestTree, testGroup)

import Haskell11 (Run(..), encodeModified)
import Haskell12 (decodeModified)
import Haskell13 (encodeDirect)
import Haskell14 (dupli)
import Haskell15 (repli)
import Haskell16 (dropEvery)
import Haskell17 (split)
import Haskell18 (slice)
import Haskell19 (rotate)
import Haskell20 (removeAt)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests20 :: TestTree
tests20 = testGroup "Unit Tests for Haskell 11-20" 
    [ haskell11Tests, haskell12Tests, haskell13Tests,
      haskell14Tests, haskell15Tests, haskell16Tests,
      haskell17Tests, haskell18Tests, haskell19Tests,
      haskell20Tests ]

-- Tests for Haskell 11

haskell11Tests :: TestTree
haskell11Tests = testGroup "Haskell 11 Tests" [
    testCase "Empty list" haskell11_emptyList,
    testCase "Simple unique list" haskell11_simpleUniqueList,
    testCase "Simple repeat list" haskell11_simpleRepeatList,
    testProperty "encodeModified shortens lists" encodeModifiedShortensList ]

haskell11_emptyList :: IO ()
haskell11_emptyList = do
    assertEqual "Empty list case" (encodeModified []) ([] :: [Run Integer])

haskell11_simpleUniqueList :: IO ()
haskell11_simpleUniqueList = do
    assertEqual "Simple list with unique values"
        (encodeModified ([0, 1, 2] :: [Integer]))
        ([Single 0, Single 1, Single 2] :: [Run Integer])

haskell11_simpleRepeatList :: IO ()
haskell11_simpleRepeatList = do
    assertEqual "Simple list with repeat values"
        (encodeModified (['x', 'x', 'x', 'y', 'y', 'z'] :: [Char]))
        ([Multiple 3 'x', Multiple 2 'y', Single 'z'] :: [Run Char])

encodeModifiedShortensList :: Property
encodeModifiedShortensList = forAll arbitrary
    (\l -> length (encodeModified l) <= length (l :: [Integer]))

-- Tests for Haskell 12

haskell12Tests :: TestTree
haskell12Tests = testGroup "Haskell 12 Tests" [
    testCase "Empty run list case" haskell12_emptyRunList,
    testCase "List of single Runs" haskell12_singlesList,
    testCase "List of Multiples Runs" haskell12_multiplesList,
    testProperty "decodeModified lengthens lists" decodeModifiedLengthensList ]

haskell12_emptyRunList :: IO ()
haskell12_emptyRunList = do
    assertEqual "Empty run list" (decodeModified [] :: [Run Integer]) []

haskell12_singlesList :: IO ()
haskell12_singlesList = do
    assertEqual "List of Single Runs" 
        (decodeModified ([Single 0, Single 1, Single 2] :: [Run Integer]))
        ([0, 1, 2] :: [Integer])

haskell12_multiplesList :: IO ()
haskell12_multiplesList = do
    assertEqual "List of Multiples Runs"
        (decodeModified ([Multiple 3 'x', Multiple 2 'y', Single 'z'] :: [Run Char]))
        "xxxyyz"

decodeModifiedLengthensList :: Property
decodeModifiedLengthensList = forAll arbitrary
    (\l -> length (l :: [Run Integer]) <= length (decodeModified l))

-- Tests for Haskell 13

haskell13Tests :: TestTree
haskell13Tests = testGroup "Haskell 13 Tests" [
    testCase "Empty list" haskell13_emptyList,
    testCase "Simple unique list" haskell13_simpleUniqueList,
    testCase "Simple repeat list" haskell13_simpleRepeatList,
    testProperty "encodeDirect shortens lists" encodeDirectShortensList ]

haskell13_emptyList :: IO ()
haskell13_emptyList = do
    assertEqual "Empty list case" (encodeDirect []) ([] :: [Run Integer])

haskell13_simpleUniqueList :: IO ()
haskell13_simpleUniqueList = do
    assertEqual "Simple list with unique values"
        (encodeDirect ([0, 1, 2] :: [Integer]))
        ([Single 0, Single 1, Single 2] :: [Run Integer])

haskell13_simpleRepeatList :: IO ()
haskell13_simpleRepeatList = do
    assertEqual "Simple list with repeat values"
        (encodeDirect (['x', 'x', 'x', 'y', 'y', 'z'] :: [Char]))
        ([Multiple 3 'x', Multiple 2 'y', Single 'z'] :: [Run Char])

encodeDirectShortensList :: Property
encodeDirectShortensList = forAll arbitrary
    (\l -> length (encodeDirect l) <= length (l :: [Integer]))

-- Tests for Haskell 14

haskell14Tests :: TestTree
haskell14Tests = testGroup "Haskell 14 Tests" [
    testCase "Empty string" haskell14_emptyString,
    testCase "Simple string" haskell14_simpleString,
    testProperty "dupli doubles lists" dupliDoublesLists ]

haskell14_emptyString :: IO ()
haskell14_emptyString = do
    assertEqual "Empty string case" (dupli "") ""

haskell14_simpleString :: IO ()
haskell14_simpleString = do
    assertEqual "Simple string case" (dupli "azxy") "aazzxxyy"

dupliDoublesLists :: Property
dupliDoublesLists = forAll arbitrary
    (\l -> 2 * length (l :: [Integer]) == length (dupli l))

-- Tests for Haskell 15

haskell15Tests :: TestTree
haskell15Tests = testGroup "Haskell 15 Tests" [
    testCase "Simple tripling" haskell15_simpleTests,
    testProperty "repli of empty strings are empty" repliEmptyStringIsEmpty,
    testProperty "repli multiplies lists" repliMultipliesLists ]

haskell15_simpleTests :: IO ()
haskell15_simpleTests = do
    assertEqual "Simple tripling case" (repli "has" 3) "hhhaaasss"

repliEmptyStringIsEmpty :: Property
repliEmptyStringIsEmpty = forAll arbitrary
    (\n -> repli "" n == "")

repliMultipliesLists :: Property
repliMultipliesLists = forAll arbitrary
    (\l n -> 
        (n < 0) ||
        (n :: Int) * length (l :: [Integer]) == length (repli l n) 
    )

-- Tests for Haskell 16

haskell16Tests :: TestTree
haskell16Tests = testGroup "Haskell 16 Tests" [
    testCase "clearing a list" haskell16_clearingList,
    testCase "Simple dropping every 3" haskell16_simpleDropList,
    testProperty "dropEvery shortens lists" dropEveryShortensLists ]

haskell16_clearingList :: IO ()
haskell16_clearingList = do
    assertEqual "Clearing a list case" (dropEvery ([1, 2, 3, 4, 5, 6] :: [Integer]) 1) []

haskell16_simpleDropList :: IO ()
haskell16_simpleDropList = do
    assertEqual "Clearing a list case" (dropEvery ([1, 2, 3, 4, 5, 6] :: [Integer]) 3) [1, 2, 4, 5]

dropEveryShortensLists :: Property
dropEveryShortensLists = forAll arbitrary
    (\l n -> 
        ((n :: Int) < 1) ||
        (length (l :: [Integer]) >= length (dropEvery l n))
    )

-- Tests for Haskell 17

haskell17Tests :: TestTree
haskell17Tests = testGroup "Haskell 17 Tests" [
    testCase "Simple list & string split" haskell17_simpleSplits,
    testProperty "splitting empty list returns two empties" splittingEmptyList ]

haskell17_simpleSplits :: IO ()
haskell17_simpleSplits = do
    assertEqual "simple split list case" (split ([1, 2, 3, 4, 5, 6] :: [Integer]) 2) ([1, 2], [3, 4, 5, 6])
    assertEqual "simple split string case" (split "axbycz" 4) ("axby", "cz")

splittingEmptyList :: Property
splittingEmptyList = forAll arbitrary 
    (\n ->  split ([] :: [Integer]) n == ([], []))

-- Tests for Haskell 18

haskell18Tests :: TestTree
haskell18Tests = testGroup "Haskell 18 Tests" [
    testCase "Simple valid string slices" haskell18_simpleValidString,
    testProperty "slice length upper bound" sliceLengthUpperBound ]

haskell18_simpleValidString :: IO ()
haskell18_simpleValidString = do
    assertEqual "empty string slice" (slice "" 1 4) ""
    assertEqual "simple valid string slice" (slice "axyzbc" 2 5) "xyz"
    assertEqual "simple valid string slice oob" (slice "short" 4 8) "rt"
    assertEqual "another valid oob string slice" (slice "tiny" (-2) 6) "tiny"

sliceLengthUpperBound :: Property
sliceLengthUpperBound = forAll arbitrary 
    (\li lo hi -> 
        (lo > hi) ||
        length (slice (li :: [Char]) (lo :: Int) (hi :: Int)) <= (hi - lo)
    )

-- Tests for Haskell 19

haskell19Tests :: TestTree
haskell19Tests = testGroup "Haskell 19 Tests" [
    testCase "Simple string rotation" haskell19_simpleStringRotation,
    testProperty "rotate does not vary length" rotateLengthInvariant ]

haskell19_simpleStringRotation :: IO ()
haskell19_simpleStringRotation = do
    assertEqual "empty string rotation" (rotate "" 3) ""
    assertEqual "unary string rotation" (rotate "n" 4) "n"
    assertEqual "simple string rotation" (rotate "davidsillman" 5) "sillmandavid"
    assertEqual "simple negative string rotation" (rotate "davidsillman" (-7)) "sillmandavid"

rotateLengthInvariant :: Property
rotateLengthInvariant = forAll arbitrary
    (\l n -> length (l :: [Char]) == length (rotate l (n :: Int)))

-- Tests for Haskell 20

haskell20Tests :: TestTree
haskell20Tests = testGroup "Haskell 20 Tests" [
    testCase "Simple string Char extraction" haskell20_stringCharExtraction,
    testProperty "valid removeAt calls decrement length" validRemovalDecrementsLength ]

haskell20_stringCharExtraction :: IO ()
haskell20_stringCharExtraction = do
    assertEqual "extract only character" (removeAt "a" 1) ('a', "")
    assertEqual "extract middle character" (removeAt "abc" 2) ('b', "ac")

validRemovalDecrementsLength :: Property
validRemovalDecrementsLength = forAll arbitrary
    (\l n ->
        ((n :: Int) < 1 || length (l :: [Integer]) < n) ||
        ((length . snd) (removeAt l n) == pred (length l))
    )