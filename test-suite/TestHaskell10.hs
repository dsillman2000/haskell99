module TestHaskell10 (tests10) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.QuickCheck
import Utils (assertErrors)

import Data.List (group)

import Haskell1 (myLast, myLastMaybe)
import Haskell2 (myButLast, myButLastMaybe)
import Haskell3 (elementAt, elementAtMaybe)
import Haskell4 (myLength)
import Haskell5 (myReverse)
import Haskell6 (isPalindrome)
import Haskell7 (NestedList(..), flatten)
import Haskell8 (compress)
import Haskell9 (pack)
import Haskell10 (encode)

tests10 :: TestTree
tests10 = testGroup "Unit Tests for Haskell 1-10" 
    [ haskell1Tests, haskell2Tests, haskell3Tests,
      haskell4Tests, haskell5Tests, haskell6Tests,
      haskell7Tests, haskell8Tests, haskell9Tests,
      haskell10Tests ]

-- Tests for Haskell 1

haskell1Tests :: TestTree
haskell1Tests = testGroup "Haskell 1 Tests" 
    [ testCase "Empty list test case" haskell1_emptyList,
      testCase "Singleton list test case" haskell1_singletonList,
      testProperty "Last of arbitrary list" lastOfArbitraryList ]

haskell1_emptyList :: IO ()
haskell1_emptyList = do
    assertErrors "myLast shall throw on an empty list" (myLast [])
    assertEqual "myLastMaybe shall return Nothing on an empty list" (myLastMaybe []) (Nothing :: Maybe[Integer])

haskell1_singletonList :: IO ()
haskell1_singletonList = do
    assertEqual "myLast shall return the sole element of a singleton list" (myLast ['x']) 'x'
    assertEqual "myLastMaybe shall return Just the sole element of a singleton list" 
        (myLastMaybe [0 :: Integer]) (Just 0)

lastOfArbitraryList :: Property
lastOfArbitraryList = forAll arbitrary
    (\l -> 
        null l || (
            myLast (l :: [Integer]) == last l &&
            myLastMaybe (l :: [Integer]) == Just (last l)
        )
    )

-- Tests for Haskell 2

haskell2Tests :: TestTree
haskell2Tests = testGroup "Haskell 2 Tests" [
    testCase "Empty or singleton list" haskell2_emptyOrSingletonList,
    testProperty "But-last of arbitrary list" butLastOfArbitraryList ]

haskell2_emptyOrSingletonList :: IO ()
haskell2_emptyOrSingletonList = do
    assertErrors "myButLast shall error on empty list" (myButLast [])
    assertErrors "myButLast shall error on a singleton list" (myButLast [pure (negate 2 :: Integer)])
    assertEqual "myButLastMaybe shall return Nothing on empty list" (myButLastMaybe ([] :: [Integer])) Nothing
    assertEqual "myButLastMaybe shall return Nothing on a singleton list" (myButLastMaybe ["abc" :: String]) Nothing

butLastOfArbitraryList :: Property
butLastOfArbitraryList = forAll arbitrary
    (\l ->
        (length l < 2) || (
            myButLast (l :: [Integer]) == (head . tail . reverse) l &&
            myButLastMaybe (l :: [Integer]) == Just ((head . tail . reverse) l)
        )
    )

-- Tests for Haskell 3

haskell3Tests :: TestTree
haskell3Tests = testGroup "Haskell 3 Tests" [
    testCase "Empty list case" haskell3_emptyList,
    testProperty "elementAt in arbitrary list" validIndexInArbitraryList,
    testProperty "elementAt in arbitrary string" validIndexInArbitraryString ]

haskell3_emptyList :: IO ()
haskell3_emptyList = do
    assertErrors "elementAt in empty list shall throw" (Haskell3.elementAt [] 2)
    assertEqual "elementAtMaybe in empty list shall return Nothing" 
        (elementAtMaybe ([] :: [Integer]) 2) Nothing

validIndexInArbitraryList :: Property
validIndexInArbitraryList = forAll arbitrary 
    (\l i -> 
        (i <= 0 || null l || i > length l) ||
        (((l !! (i - 1:: Int)) :: Integer) == elementAt l i)
    )

validIndexInArbitraryString :: Property
validIndexInArbitraryString = forAll arbitrary 
    (\s i -> 
        (i <= 0  || null s || i > length s) ||
        (((s !! (i - 1:: Int)) :: Integer) == elementAt s i)
    )

-- Tests for Haskell 4

haskell4Tests :: TestTree
haskell4Tests = testGroup "Haskell 4 Tests" [ 
    testProperty "Arbitrary list test" arbitraryListLength ]

arbitraryListLength :: Property
arbitraryListLength = forAll arbitrary
    (\l -> myLength (l:: [Integer]) == length l)

-- Tests for Haskell 5

haskell5Tests :: TestTree
haskell5Tests = testGroup "Haskell 5 Tests" [
    testProperty "Arbitrary list test" arbitraryReverseList ]

arbitraryReverseList :: Property
arbitraryReverseList = forAll arbitrary
    (\l -> myReverse (l :: [Integer]) == reverse l)

-- Tests for Haskell 6

haskell6Tests :: TestTree
haskell6Tests = testGroup "Haskell 6 Tests" [
    testProperty "Arbitrary non-palindrome list test" arbitraryNonPalindromeList,
    testProperty "Arbitrary even palindrome list test" arbitraryEvenPalindromeList,
    testProperty "Arbitrary odd palindrome list test" arbitraryOddPalindromeList ]

arbitraryNonPalindromeList :: Property
arbitraryNonPalindromeList = forAll arbitrary 
    (\l -> not (isPalindrome ([0] ++ (l :: [Integer]) ++ [1])))

arbitraryEvenPalindromeList :: Property
arbitraryEvenPalindromeList = forAll arbitrary
    (\l -> isPalindrome ((l :: [Integer]) ++ reverse l))

arbitraryOddPalindromeList :: Property
arbitraryOddPalindromeList = forAll arbitrary
    (\l -> isPalindrome ((l :: [Integer]) ++ [4] ++ reverse l))

-- Tests for Haskell 7

haskell7Tests :: TestTree
haskell7Tests = testGroup "Haskell 7 Tests" [
    testCase "Flattening empty list shall return empty list" haskell7_emptyList,
    testCase "Flattening flat list shall return same list" haskell7_simpleList,
    testCase "Flattening deep list shall return flattened list" haskell7_deepList,
    testCase "Flattening deeper list shall return flattened list" haskell7_deeperList ]

haskell7_emptyList :: IO ()
haskell7_emptyList = do
    assertEqual "flatten on empty List shall return empty list" 
        (flatten (List [])) ([] :: [Integer])

haskell7_simpleList :: IO ()
haskell7_simpleList = do
    assertEqual "flatten on simple List shall return a flat list"
        (flatten (List [Elem 1, Elem 2, Elem 3])) ([1, 2, 3] :: [Integer])

haskell7_deepList :: IO ()
haskell7_deepList = do
    assertEqual "flatten on deep List shall return a flat list"
        (flatten (List [List [Elem 2, Elem 4], Elem 0, List [], Elem 1, List [Elem 3, Elem 7, Elem 8]]))
        ([2, 4, 0, 1, 3, 7, 8] :: [Integer])

haskell7_deeperList :: IO ()
haskell7_deeperList = do
    assertEqual "flatten on deeper List shall return a flat list"
        (flatten (List [
            List [ Elem 2, List [] ],
            List [ List [ Elem 0 ], List [ Elem 4 ], Elem 0, Elem 0 ],
            List [ List [ List [], Elem 1, List [Elem 7, Elem 8 ] ],
            Elem 2, Elem 3,
            List [ Elem 7, List [ Elem 3, Elem 0 ] ]
        ] ]))
        ([ 2, 0, 4, 0, 0, 1, 7, 8, 2, 3, 7, 3, 0 ] :: [Integer])

-- Tests for Haskell 8

haskell8Tests :: TestTree
haskell8Tests = testGroup "Haskell 8 Tests" [
    testCase "Simple repetitive string" haskell8_repetitiveString,
    testCase "Fibonacci start" haskell8_fibStart,
    testProperty "compress shortens lists" compressShortensLists ]

haskell8_repetitiveString :: IO ()
haskell8_repetitiveString = do
    assertEqual "compress shall remove the duplicate groups of characters"
        (compress "aaabcccdefeeefg") "abcdefefg"

haskell8_fibStart :: IO ()
haskell8_fibStart = do
    assertEqual "compress shall remove the initial duplicates from the fib sequence"
        (compress [1, 1, 2, 3, 5]) ([1, 2, 3, 5] :: [Integer])

compressShortensLists :: Property
compressShortensLists = forAll arbitrary
    (\l -> length (compress l) <= length (l :: [Integer]))

-- Tests for Haskell 9

haskell9Tests :: TestTree
haskell9Tests = testGroup "Haskell 9 Tests" [
    testCase "Simple un-groupable list" haskell9_simpleUngroupableList,
    testCase "Simple groupable list" haskell9_simpleGroupableList,
    testProperty "pack groups lists" packGroupsLists ]

haskell9_simpleUngroupableList :: IO ()
haskell9_simpleUngroupableList = do
    assertEqual "pack shall singleton-group an ungroupable list"
        (pack ['a', 'b', 'e', 'l', 'o']) ["a", "b", "e", "l", "o"]

haskell9_simpleGroupableList :: IO ()
haskell9_simpleGroupableList = do
    assertEqual "pack shall group a simple groupable list"
        (pack ['x', 'x', 'x', 'y', 'y', 'z']) ["xxx", "yy", "z"]

packGroupsLists :: Property
packGroupsLists = forAll arbitrary
    (\l -> pack (l :: [Char]) == group l)

-- Tests for Haskell 10

haskell10Tests :: TestTree
haskell10Tests = testGroup "Haskell 10 Tests" [
    testCase "Empty list" haskell10_emptyList,
    testCase "Simple unique list" haskell10_simpleUniqueList,
    testCase "Simple list with repeats" haskell10_simpleRepeatList,
    testProperty "encode shortens lists" encodeShortensLists ]

haskell10_emptyList :: IO ()
haskell10_emptyList = do
    assertEqual "Empty list test case" (encode []) ([] :: [(Integer, Int)])

haskell10_simpleUniqueList :: IO ()
haskell10_simpleUniqueList = do
    assertEqual "Simple list test case" (encode ([1, 2, 3] :: [Integer]))
        ([(1, 1), (2, 1), (3, 1)] :: [(Integer, Int)])

haskell10_simpleRepeatList :: IO ()
haskell10_simpleRepeatList = do
    assertEqual "Simple repeated list test case" (encode ([0, 0, 1, 1, 1] :: [Integer]))
        ([(0, 2), (1, 3)] :: [(Integer, Int)])

encodeShortensLists :: Property
encodeShortensLists = forAll arbitrary
    (\l -> length (encode l) <= length (l :: [Integer]))