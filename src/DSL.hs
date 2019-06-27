-- ************************************************************************** --
--                                                                            --
--                                                        ::::::::            --
--   DSL.hs                                             :+:    :+:            --
--                                                     +:+                    --
--   By: nmartins <nmartins@student.codam.nl>         +#+                     --
--                                                   +#+                      --
--   Created: 2019/06/27 23:02:41 by nmartins       #+#    #+#                --
--   Updated: 2019/06/27 23:03:06 by nmartins      ########   odam.nl         --
--                                                                            --
-- ************************************************************************** --

module DSL where

import           Control.Monad
import           Data.List
import           Debug.Trace
import           Instruction
import           Machine

newtype InstW a = InstW { runWriter :: (a, [Instruction]) }
                  deriving Show

instance Functor InstW where
    fmap f (InstW (a, xs)) = InstW (f a, xs)

instance Applicative InstW where
    pure a = InstW (a, [])
    (<*>) (InstW (f, xs)) (InstW (x, ys)) = InstW (f x, xs ++ ys)

instance Monad InstW where
    return = pure
    (>>=) (InstW (a, xs)) f = let (InstW (b, ys)) = f a in InstW (b, xs ++ ys)

rawInst :: Instruction -> InstW ()
rawInst inst = InstW ((), [inst])

runRawInst :: Machine -> Instruction -> InstW Machine
runRawInst mach inst =
    runInstruction mach inst <$ rawInst inst

swapA mach = runRawInst mach $ Swap A
swapB mach = runRawInst mach $ Swap B
pushA mach = runRawInst mach $ Push A
pushB mach = runRawInst mach $ Push B
rotateA mach = runRawInst mach $ Rotate A
rotateB mach = runRawInst mach $ Rotate B
reverseRotateA mach = runRawInst mach $ ReverseRotate A
reverseRotateB mach = runRawInst mach $ ReverseRotate A

rotateANTimes 0 mach = pure mach
rotateANTimes n mach = rotateA mach >>= rotateANTimes (pred n)
