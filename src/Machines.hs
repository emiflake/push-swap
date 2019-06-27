-- ************************************************************************** --
--                                                                            --
--                                                        ::::::::            --
--   Machines.hs                                        :+:    :+:            --
--                                                     +:+                    --
--   By: nmartins <nmartins@student.codam.nl>         +#+                     --
--                                                   +#+                      --
--   Created: 2019/06/27 23:02:38 by nmartins       #+#    #+#                --
--   Updated: 2019/06/27 23:05:57 by nmartins      ########   odam.nl         --
--                                                                            --
-- ************************************************************************** --

module Machines where

import           Control.Monad
import           Data.List
import           DSL
import           Instruction
import           Machine

stepByStepInstW :: (Machine -> InstW Machine) -> Machine -> ([Machine], [Instruction])
stepByStepInstW f mach = let (res, insts) = runWriter $ f mach
                          in (stepByStep mach insts, insts)

debugStepper :: (Machine -> InstW Machine) -> Machine -> IO ()
debugStepper f mach = do
    let (machines, insts) = stepByStepInstW f mach
    forM_ (zip machines insts) $ \(mach, inst) -> do
        putStrLn $ "Machine: " ++ show mach
        putStrLn $ "Inst:    " ++ show inst
    print $ f mach

bubbleSort :: Machine -> InstW Machine
bubbleSort mach@(Machine [] [])      = pure (Machine [] [])
bubbleSort mach@(Machine [x] [])     = pure mach
bubbleSort mach@(Machine as [])
    | sort as == as = pure mach
    | otherwise = do
        afterPass <- pass (length as) mach
        if afterPass == mach then
            pure afterPass
        else bubbleSort afterPass
    where pass 1 mach = rotateA mach
          pass n mach@(Machine all@(a:b:as) [])
            | a > b = swapA mach >>= rotateA >>= pass (pred n)
            | otherwise = rotateA mach >>= pass (pred n)

insertionSort :: Machine -> InstW Machine
insertionSort mach@(Machine [] []) = pure mach
insertionSort mach@(Machine [] bs) = pushAll mach
            where pushAll mach@(Machine _ [])     = pure mach
                  pushAll mach@(Machine _ (b:bs)) = pushA mach >>= pushAll
insertionSort mach@(Machine xs bs) =
    rotateANTimes (minIndex xs) mach >>= pushB >>= insertionSort

minIndex []     = 0
minIndex (x:xs) = go xs 1 (x, 0)
    where go [] _ (mv, mi) = mi
          go (x:xs) i (mv, mi) | x < mv = go xs (succ i) (x, i)
                               | otherwise = go xs (succ i) (mv, mi)


runMachine' :: (Machine -> InstW Machine) -> Machine -> String
runMachine' f mach = let (_, insts) = runWriter $ f mach
                     in genInsts insts

runMachine :: (Machine -> InstW Machine) -> [Integer] -> String
runMachine f ints = let (_, insts) = runWriter $ f (startMachine ints)
                     in genInsts insts
