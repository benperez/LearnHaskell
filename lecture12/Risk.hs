{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army
                               , defenders :: Army 
                               }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield nAttack nDefend) = do
  attackDie <- sequence (replicate nAttack die)
  defendDie <- sequence (replicate nDefend die)
  let nToBattle = min nAttack nDefend
  let sortedAttackDie = reverse . take nToBattle . sort $ attackDie
  let sortedDefendDie = reverse . take nToBattle . sort $ defendDie
  let attackerWins = zipWith didAttackerWin sortedAttackDie sortedDefendDie
  let nAttackerWins = length $ filter id attackerWins
  let nAttackerLosses = nToBattle - nAttackerWins
  let nRemainingAttackers = nAttack - nAttackerLosses
  let nRemainingDefenders = nDefend - nAttackerWins
  return Battlefield {attackers=nRemainingAttackers, defenders=nRemainingDefenders}
  where
    didAttackerWin attacker defender = case attacker `compare` defender of
      GT -> True
      EQ -> False
      LT -> False
