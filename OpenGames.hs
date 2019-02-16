{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

--http://www.cs.ox.ac.uk/people/julian.hedges/papers/Thesis.pdf

module OpenGames(
OpenGame(..)
)where

import qualified Data.Set as Set
import Control.Applicative

data Singleton = Trivial deriving (Eq,Ord,Show)

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = powerList xs ++ map (x:) (powerList xs)
myPowerset :: (Ord a) => Set.Set a -> Set.Set (Set.Set a)
myPowerset s = Set.fromList $ map (Set.fromList) (powerList $ Set.toList s)

trivial_function :: Singleton -> Singleton
trivial_function Trivial = Trivial

trivial_ps_coalg :: Singleton -> Set.Set Singleton
trivial_ps_coalg Trivial = Set.fromList [Trivial]

data OpenGame x s y r sigma = OpenGame{
    play_function :: sigma -> x -> y,
    coplay_function :: sigma -> x  -> r -> s,
    best_response_function :: x -> (y -> r) -> (sigma -> Set.Set (sigma))
}

type ClosedGame sigma = OpenGame Singleton Singleton Singleton Singleton sigma
type Decision x y r = OpenGame x Singleton y r (x->y)
type DecisionNoObs y r = Decision Singleton y r
type CovariantComputation x y sigma = OpenGame x Singleton y Singleton sigma
type ContravariantComputation y x sigma = OpenGame Singleton y Singleton x sigma

from_decision :: ((x->y) -> Set.Set (x->y)) -> Decision x y r
from_decision best_response = OpenGame{play_function=(\sigma_elem -> sigma_elem),
                                       coplay_function = (\sigma_elem x_elem r_elem -> Trivial),
                                       best_response_function=(\x_elem z -> best_response)}

rationality_function :: Decision x y r -> x -> (y -> r) -> (x->y) -> Set.Set (x->y)
rationality_function game h k sigma_unimportant = (best_response_function game) h k sigma_unimportant

from_function :: (x -> y) -> (x-> sigma -> Set.Set sigma) -> CovariantComputation x y sigma
from_function f best_response = OpenGame{play_function = (\sigma_elem -> f),
                           coplay_function = (\sigma_elem x_elem r_elem -> Trivial),
                           best_response_function =(\x_elem z -> best_response x_elem)}

from_function_reverse :: (x -> y) -> (x-> sigma -> Set.Set sigma) -> ContravariantComputation y x sigma
from_function_reverse f best_response = OpenGame{play_function=(\sigma_elem z -> Trivial),
                                coplay_function=(\sigma_elem z x_elem -> f x_elem),
                                best_response_function = (\z triv_to_x_elem -> best_response (triv_to_x_elem Trivial))}

from_pair_functions :: (x->y) -> (r -> s) -> OpenGame x s y r Singleton
from_pair_functions f g = OpenGame{play_function=(\sigma_elem -> f),
                                   coplay_function=(\sigma_elem x_elem r_elem -> g r_elem),
                                   best_response_function=(\x_elem z -> trivial_ps_coalg)}

covariant_from_function :: (x->y) -> CovariantComputation x y Singleton
covariant_from_function f = from_pair_functions f trivial_function

contravariant_from_function :: (r->s) -> ContravariantComputation s r Singleton
contravariant_from_function g = from_pair_functions trivial_function g

counit_helper :: (x -> sigma -> Set.Set sigma) -> OpenGame x x Singleton Singleton sigma
counit_helper best_response = OpenGame {play_function = (\sigma_elem x_elem -> Trivial),
                   coplay_function = (\sigma_elem x_elem z -> x_elem),
                   best_response_function = (\x_elem z -> best_response x_elem)}

counit :: OpenGame x x Singleton Singleton Singleton
counit = counit_helper (\x_elem -> trivial_ps_coalg)

from_ps_coalg :: (sigma -> Set.Set sigma) -> ClosedGame sigma
from_ps_coalg f = OpenGame{play_function =(\sigma_elem x_elem -> Trivial),
                           coplay_function=(\sigma_elem x_elem r_elem -> Trivial),
                           best_response_function=(\x_elem z -> f)}

coplay_composition_helper2 :: (sigma2 -> y  -> r -> s) -> sigma2 -> (sigma1 -> x -> y) -> sigma1 -> x -> r -> s
coplay_composition_helper2 coplay_2 tau_elem play_1 sigma_elem x_elem r_elem = coplay_2 tau_elem (play_1 sigma_elem x_elem) r_elem
coplay_composition_helper :: (sigma1 -> x  -> s -> t) -> (sigma2 -> y  -> r -> s) -> sigma2 -> (sigma1 -> x -> y) -> sigma1 -> x -> r -> t
coplay_composition_helper coplay_1 coplay_2 tau_elem play_1 sigma_elem x_elem r_elem = coplay_1 sigma_elem x_elem (coplay_composition_helper2 coplay_2 tau_elem play_1 sigma_elem x_elem r_elem)

play_function_helper :: OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> ((sigma1,sigma2) -> x -> z)
play_function_helper game1 game2= (\(sigma_elem,tau_elem) x_elem -> ((play_function game2) tau_elem) $ ((play_function game1) sigma_elem) x_elem)

ktbullet :: OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> sigma2 -> (z -> r) -> y -> s
ktbullet game1 game2 sigma2_elem z_to_r_elem = (\y_elem -> (coplay_function game2) sigma2_elem y_elem (z_to_r_elem $ (play_function game2) sigma2_elem y_elem))

first_factor_composition :: OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> x -> (z -> r) -> sigma1 -> sigma2 -> (Set.Set sigma1)
first_factor_composition game1 game2 x_elem z_to_r_elem sigma1_elem sigma2_elem = (best_response_function game1) x_elem (ktbullet game1 game2 sigma2_elem z_to_r_elem) sigma1_elem

second_factor_composition :: OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> x -> (z -> r) -> sigma1 -> sigma2 -> (Set.Set sigma2)
second_factor_composition game1 game2 x_elem z_to_r_elem sigma1_elem sigma2_elem = (best_response_function game2) ((play_function game1) sigma1_elem x_elem) z_to_r_elem sigma2_elem

intersectAll :: (Ord a) => [Set.Set a] -> Set.Set a
intersectAll [] = Set.fromList []
intersectAll (x:xs) = foldl Set.intersection x xs

second_factor_composition2 :: (Bounded sigma1,Enum sigma1,Ord sigma2) => OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> x -> (z -> r) -> sigma2 -> Set.Set sigma2
second_factor_composition2 game1 game2 x_elem z_to_r_elem sigma2_elem = intersectAll $ map (\sigma1_elem -> (second_factor_composition game1 game2 x_elem z_to_r_elem sigma1_elem sigma2_elem)) [minBound..maxBound]

cart_Prod :: (Ord a,Ord b) => Set.Set a -> Set.Set b -> Set.Set (a,b)
cart_Prod xs ys = Set.fromList $ liftA2 (,) (Set.toList xs) (Set.toList ys)

nash_best_response_composition :: (Ord sigma1,Ord sigma2) => OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> x -> (z -> r) -> (sigma1,sigma2) -> (Set.Set (sigma1,sigma2))
nash_best_response_composition game1 game2 x_elem z_to_r_elem (sigma1_elem,sigma2_elem) = cart_Prod (first_factor_composition game1 game2 x_elem z_to_r_elem sigma1_elem sigma2_elem) (second_factor_composition game1 game2 x_elem z_to_r_elem sigma1_elem sigma2_elem)

sgp_best_response_composition :: (Ord sigma1,Ord sigma2,Bounded sigma1,Enum sigma1) => OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> x -> (z -> r) -> (sigma1,sigma2) -> (Set.Set (sigma1,sigma2))
sgp_best_response_composition game1 game2 x_elem z_to_r_elem (sigma1_elem,sigma2_elem) = cart_Prod (first_factor_composition game1 game2 x_elem z_to_r_elem sigma1_elem sigma2_elem) (second_factor_composition2 game1 game2 x_elem z_to_r_elem sigma2_elem)

nash_compose_games :: (Ord sigma1,Ord sigma2) => OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> OpenGame x t z r (sigma1,sigma2)
nash_compose_games game1 game2 = OpenGame{play_function=play_function_helper game1 game2,
                                 coplay_function=(\(sigma_elem,tau_elem) x_elem r_elem -> coplay_composition_helper (coplay_function game1) (coplay_function game2) tau_elem (play_function game1) sigma_elem x_elem r_elem),
                                 best_response_function=(nash_best_response_composition game1 game2)
                                }

sgp_compose_games :: (Ord sigma1,Ord sigma2,Bounded sigma1,Enum sigma1) => OpenGame x t y s sigma1 -> OpenGame y s z r sigma2 -> OpenGame x t z r (sigma1,sigma2)
sgp_compose_games game1 game2 = OpenGame{play_function=play_function_helper game1 game2,
                                 coplay_function=(\(sigma_elem,tau_elem) x_elem r_elem -> coplay_composition_helper (coplay_function game1) (coplay_function game2) tau_elem (play_function game1) sigma_elem x_elem r_elem),
                                 best_response_function=(sgp_best_response_composition game1 game2)
                                }