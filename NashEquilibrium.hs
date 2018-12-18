{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module NashEquilibrium where

class PlayersList a
class StrategyList b

data TwoPlayers = Alice | Bob deriving (Eq,Enum,Bounded)
instance PlayersList TwoPlayers

data PrisonersOptions = Cooperate | Defect deriving (Eq,Enum,Bounded,Show)
instance StrategyList PrisonersOptions

class (PlayersList a,StrategyList b) => PayoffMatrix a b where
    payoff :: a -> (a -> b) -> Int

competingStrategy :: TwoPlayers -> (TwoPlayers -> PrisonersOptions)
competingStrategy Alice = \x -> (case x of Alice -> Defect
                                           Bob -> Cooperate)
competingStrategy Bob = \x -> (case x of Bob -> Defect
                                         Alice -> Cooperate)
everybody_same_strategy :: b -> (a -> b)
everybody_same_strategy z = (\x -> z)
both_cooperate :: (TwoPlayers -> PrisonersOptions)
both_cooperate = everybody_same_strategy Cooperate
both_defect :: (TwoPlayers -> PrisonersOptions)
both_defect = everybody_same_strategy Defect

data HawkDoveOptions = Hawk | Dove deriving (Eq,Enum,Bounded,Show)
instance StrategyList HawkDoveOptions

both_hawk :: (TwoPlayers -> HawkDoveOptions)
both_hawk = everybody_same_strategy Hawk
both_dove :: (TwoPlayers -> HawkDoveOptions)
both_dove = everybody_same_strategy Dove
competingStrategy2 :: TwoPlayers -> (TwoPlayers -> HawkDoveOptions)
competingStrategy2 Alice = \x -> (case x of Alice -> Dove
                                            Bob -> Hawk)
competingStrategy2 Bob = \x -> (case x of Bob -> Dove
                                          Alice -> Hawk)

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x:xs)
             | xs == [] = True
             | otherwise = (x==(head xs)) && (allEqual xs)

instance PayoffMatrix TwoPlayers PrisonersOptions where
    payoff z f = case (allEqual $ map f [minBound..maxBound]) of
                       True -> helper1 (f minBound)
                       False -> (case (f z) of Cooperate -> -3
                                               Defect -> 0)
                       where helper1 Cooperate = -1
                             helper1 Defect = -2

instance PayoffMatrix TwoPlayers HawkDoveOptions where
    payoff z f = case (allEqual $ map f [minBound..maxBound]) of
                       True -> helper1 (f minBound)
                       False -> (case (f z) of Hawk -> 7
                                               Dove -> 2)
                       where helper1 Hawk = 0
                             helper1 Dove = 6

replace_func_value :: (Eq a) => a -> (a -> b) -> b -> (a->b)
replace_func_value me current_f to_replace x 
                                             | x==me = to_replace
                                             | otherwise = current_f x

onePlayerModifications :: (Eq a,Eq b,Enum b,Bounded b) => a -> (a -> b) -> [(a->b)]
onePlayerModifications me currentStrategies = (map (replace_func_value me currentStrategies) alternatives) where alternatives = filter (\e -> e/=(currentStrategies me)) [minBound..maxBound]

can_I_do_better_helper :: (Ord c,Bounded c) => c -> [c] -> Bool
can_I_do_better_helper x y = (x < (foldl max minBound y))

can_I_do_better :: (PayoffMatrix a b,Eq a,Eq b,Enum b,Bounded b) => (a->b) -> a -> Bool
can_I_do_better currentStrategies me = can_I_do_better_helper (payoff me currentStrategies) (map (payoff me) alternatives) where alternatives=(onePlayerModifications me currentStrategies)

isPureNashEq :: (PayoffMatrix a b,Eq a,Eq b,Enum b,Bounded b,Enum a,Bounded a) => (a -> b) -> Bool
isPureNashEq currentStrategies = not $ or (map (can_I_do_better currentStrategies) [minBound..maxBound])

type MixedStrategies a b = a -> (b->Float)
promote_to_mixed :: (Eq b) => (a -> b) -> MixedStrategies a b
promote_to_mixed f me b_elem
                             | f me == b_elem = 1
                             | otherwise = 0

-- Examples
--isPureNashEq both_dove False
--isPureNashEq both_hawk False
--isPureNashEq (competingStrategy2 Alice) True
--isPureNashEq (competingStrategy2 Bob) True
--isPureNashEq both_defect True
--isPureNashEq both_cooperate False
--isPureNashEq (competingStrategy Bob) False
--isPureNashEq (competingStrategy Alice) False