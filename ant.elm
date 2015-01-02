module Ant where

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Input
import Signal
import Dict
import Dict (..)
import List

main : Signal Element
main = lift view state
  
state : Signal State
state = foldp step startState input

input = sampleOn (fps 100) antTypeInput.signal

view : State -> Element
view st = flow down 
            [collage 400 400 (Dict.foldr (\key val list -> (cell2form key val) :: list) [] st.visitedCells),
             flow right [ plainText " Type of ant:   ",
                Graphics.Input.dropDown antTypeInput.handle antTypeOptions]]
  
antTypeInput : Graphics.Input.Input AntType
antTypeInput = Graphics.Input.input (snd (head antTypeOptions))  

antTypeOptions : [(String, AntType)]
antTypeOptions = [("LR", [(0, L), (1, R)]), 
                  ("LLRR", [(0, L), (1, L), (2, R), (3, R)]),
                  ("LRLR", [(0, L), (1, R), (2, L), (3, R)]),
                  ("LRRL", [(0, L), (1, R), (2, R), (3, L)])]  

cell2form : Cell -> Int -> Form
cell2form c st = rect squareWidth squareWidth |> filled (getColor st) 
                                              |> move (toFloat (fst c) * squareWidth, toFloat (snd c) * squareWidth)

getColor : Int -> Color
getColor i = case i of
                  0 -> white
                  1 -> black
                  2 -> purple
                  3 -> blue

squareWidth = 4

type Cell = (Int, Int)

data Rotation = L | R

type AntType = [(Int, Rotation)]

type State = { cell: Cell, visitedCells : Dict Cell Int, direction :  (Int, Int), currentType : AntType}

   
startState : State
startState = { cell = (0,1), 
               visitedCells = Dict.empty, 
               direction = (-1, 0), 
               currentType = snd (head antTypeOptions)}

getCellState : Dict Cell Int -> Cell -> Int
getCellState dict c = case (get c dict) of
                        Just i -> i
                        Nothing -> 0
                       
updateCellState : AntType -> Int -> Int
updateCellState t i = (i + 1) % (List.length t)

newDir : AntType -> (Int, Int) -> Int -> (Int, Int)
newDir t (x, y) b = let rot = snd (head (List.filter (\(s, r) -> s == b) t)) 
                  in case (x, y) of
                    (-1, 0) -> if rot == L then (0, -1) else (0, 1)
                    (1, 0)  -> if rot == L then (0, 1) else (0, -1)
                    (0, -1) -> if rot == L then (1, 0) else (-1, 0)
                    (0, 1)  -> if rot == L then (-1, 0) else (1, 0)

step : AntType -> State -> State
step t st = if t == st.currentType then
                let cellState = getCellState st.visitedCells st.cell
                    newDirection = newDir t st.direction cellState
                in {cell = ((fst st.cell) + (fst newDirection), (snd st.cell) + (snd newDirection)), 
                    visitedCells = insert st.cell (updateCellState t cellState) st.visitedCells, 
                    direction = newDirection,
                    currentType = t}
            else {cell = startState.cell, visitedCells = startState.visitedCells, direction = startState.direction, currentType = t}