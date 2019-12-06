module Main where

import Prelude

import Data.Array ((!!), modifyAtIndices)
import Data.Int (decimal, toStringAs)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)

state :: Array Int
state = [
1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,6,23,27,1,27,5,31,2,9,31,35,1,5,35,39,2,6,39,43,2,6,43,47,1,5,47,51,2,9,51,55,1,5,55,59,1,10,59,63,1,63,6,67,1,9,67,71,1,71,6,75,1,75,13,79,2,79,13,83,2,9,83,87,1,87,5,91,1,9,91,95,2,10,95,99,1,5,99,103,1,103,9,107,1,13,107,111,2,111,10,115,1,115,5,119,2,13,119,123,1,9,123,127,1,5,127,131,2,131,6,135,1,135,5,139,1,139,6,143,1,143,6,147,1,2,147,151,1,151,5,0,99,2,14,0,0
]

extractProgram :: Array Int -> Int -> { operation:: Int, indexValue1::Int, indexValue2::Int, indexDestination::Int}
extractProgram arr indexProgram = do
  let debutIndexProgram = indexProgram * 4
  {
    operation: fromMaybe 99 (arr !! debutIndexProgram), 
    indexValue1: fromMaybe 0 (arr !! (debutIndexProgram + 1)), 
    indexValue2: fromMaybe 0 (arr !! (debutIndexProgram + 2)), 
    indexDestination: fromMaybe 0 (arr !! (debutIndexProgram + 3))
  }

extractDataForProgram :: Array Int -> Int -> { operation:: Int, indexValue1::Int, indexValue2::Int, indexDestination::Int} ->  { firstVal:: Int, secondVal::Int, newIndex::Int}
extractDataForProgram arr indexProgram program = { 
   firstVal: fromMaybe 0 ( arr !! program.indexValue1),
   secondVal: fromMaybe 0 ( arr !! program.indexValue2),
   newIndex: indexProgram + 1
  }

executeOperation :: Array Int -> { operation:: Int, indexValue1::Int, indexValue2::Int, indexDestination::Int} -> Int -> ({ firstVal:: Int, secondVal::Int, newIndex::Int} -> Int) -> Int
executeOperation arr program indexProgram operation = do
    let pgData = extractDataForProgram arr indexProgram program
    let newArr = modifyAtIndices [program.indexDestination] (\x -> (operation pgData)) arr
    executeProgram newArr (extractProgram newArr pgData.newIndex) pgData.newIndex 

executeProgram :: Array Int -> { operation:: Int, indexValue1::Int, indexValue2::Int, indexDestination::Int} -> Int -> Int
executeProgram arr program indexProgram
  | program.operation == 1 = executeOperation arr program indexProgram (\pgData -> pgData.firstVal + pgData.secondVal)
  | program.operation == 2 = executeOperation arr program indexProgram (\pgData -> pgData.firstVal * pgData.secondVal)
  | otherwise = fromMaybe 0 (arr !! 0)

firstProgram :: Array Int -> Int
firstProgram arr = do 
  let newArr = modifyAtIndices [2] (\x -> 2) (modifyAtIndices [1] (\x -> 12) arr)
  executeProgram newArr (extractProgram newArr 0) 0

main :: Effect Unit
main = do
  log (toStringAs decimal (firstProgram state))
