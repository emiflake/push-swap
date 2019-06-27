-- ************************************************************************** --
--                                                                            --
--                                                        ::::::::            --
--   Instruction.hs                                     :+:    :+:            --
--                                                     +:+                    --
--   By: nmartins <nmartins@student.codam.nl>         +#+                     --
--                                                   +#+                      --
--   Created: 2019/06/27 23:02:46 by nmartins       #+#    #+#                --
--   Updated: 2019/06/27 23:02:47 by nmartins      ########   odam.nl         --
--                                                                            --
-- ************************************************************************** --

module Instruction where


data Target = A
            | B
            | Both
            deriving (Show, Eq)

data Instruction = Swap Target
                 | Push Target
                 | Rotate Target
                 | ReverseRotate Target
                 deriving (Show, Eq)

parseInst :: String -> Instruction
parseInst "sa"  = Swap A
parseInst "sb"  = Swap B
parseInst "ss"  = Swap Both
parseInst "pa"  = Push A
parseInst "pb"  = Push B
parseInst "ra"  = Rotate A
parseInst "rb"  = Rotate B
parseInst "rr"  = Rotate Both
parseInst "rra" = ReverseRotate A
parseInst "rrb" = ReverseRotate B
parseInst "rrr" = ReverseRotate Both
parseInst _     = error "Instruction unsupported"

parseInstructions :: String -> [Instruction]
parseInstructions str = parseInst <$> words str

genInsts :: [Instruction] -> String
genInsts = unwords . fmap genInst

genInst :: Instruction -> String
genInst (Swap A)             = "sa"
genInst (Swap B)             = "sb"
genInst (Swap Both)          = "sb"
genInst (Push A)             = "pa"
genInst (Push B)             = "pb"
genInst (Rotate A)           = "ra"
genInst (Rotate B)           = "rb"
genInst (Rotate Both)        = "rr"
genInst (ReverseRotate A)    = "rra"
genInst (ReverseRotate B)    = "rrb"
genInst (ReverseRotate Both) = "rrr"
