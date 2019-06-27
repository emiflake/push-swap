-- ************************************************************************** --
--                                                                            --
--                                                        ::::::::            --
--   Machine.hs                                         :+:    :+:            --
--                                                     +:+                    --
--   By: nmartins <nmartins@student.codam.nl>         +#+                     --
--                                                   +#+                      --
--   Created: 2019/06/27 23:02:44 by nmartins       #+#    #+#                --
--   Updated: 2019/06/27 23:02:45 by nmartins      ########   odam.nl         --
--                                                                            --
-- ************************************************************************** --

{-# LANGUAGE RecordWildCards #-}
module Machine where

import           Data.List
import           Instruction

data Machine = Machine { stackA :: [Integer]
                       , stackB :: [Integer]
                       }
                       deriving (Show, Eq)

startMachine :: [Integer] -> Machine
startMachine xs = Machine { stackA = xs
                          , stackB = [] }

runInstruction :: Machine -> Instruction -> Machine
runInstruction (Machine as (b:bs)) (Push A)                = Machine (b:as) bs
runInstruction (Machine (a:as) bs) (Push B)                = Machine as (a:bs)
runInstruction (Machine as (b1:b2:bs)) (Swap B)            = Machine as (b2:b1:bs)
runInstruction (Machine (a1:a2:as) bs) (Swap A)            = Machine (a2:a1:as) bs
runInstruction (Machine (a1:a2:as) (b1:b2:bs)) (Swap Both) = Machine (a2:a1:as) (b2:b1:bs)
runInstruction (Machine as bs) (Rotate A)                  = Machine (rotate as) bs
runInstruction (Machine as bs) (Rotate B)                  = Machine as (rotate bs)
runInstruction (Machine as bs) (Rotate Both)               = Machine (rotate as) (rotate bs)
runInstruction (Machine as bs) (ReverseRotate A)           = Machine (reverseRotate as) bs
runInstruction (Machine as bs) (ReverseRotate B)           = Machine as (reverseRotate bs)
runInstruction (Machine as bs) (ReverseRotate Both)        = Machine (reverseRotate as) (reverseRotate bs)
runInstruction machine _                                   = machine

runInstructions  :: Machine -> [Instruction] -> Machine
runInstructions = foldl runInstruction

runParsed :: [Integer] -> String -> Bool
runParsed ints instStrs = let insts = parseInstructions instStrs
                           in isSorted $ runInstructions (startMachine ints) insts


isSorted :: Machine -> Bool
isSorted (Machine as []) = sort as == as
isSorted _               = False

rotate :: [a] -> [a]
rotate []     = []
rotate [x]    = [x]
rotate (x:xs) = xs ++ [x]

reverseRotate :: [a] -> [a]
reverseRotate []  = []
reverseRotate [x] = [x]
reverseRotate xs  = last xs : init xs

stepByStep :: Machine -> [Instruction] -> [Machine]
stepByStep = scanl runInstruction

