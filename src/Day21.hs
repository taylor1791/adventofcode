module Day21 where

import           Day17 (combinations)

data Player = Player {
    pHp     :: Int
  , pWeapon :: Equipment
  , pArmor  :: Maybe Equipment
  , pRings  :: [Equipment]
  } deriving Show

data Boss = Boss {
    bHp         :: Int
  , bDamage     :: Int
  , bArmorClass :: Int
  } deriving Show

data Equipment = Equipment {
    name       :: String
  , cost       :: Int
  , damage     :: Int
  , armorClass :: Int
  } deriving Show

weapons :: [Equipment]
weapons = [
    Equipment "Dagger"      8 4 0
  , Equipment "Shortsword" 10 5 0
  , Equipment "Warhammer"  25 6 0
  , Equipment "Longsword"  40 7 0
  , Equipment "GreatAxe"   74 8 0
  ]

armor :: [Equipment]
armor = [
    Equipment "Leather"    13 0 1
  , Equipment "Chainmail"  31 0 2
  , Equipment "Splintmail" 53 0 3
  , Equipment "Bandedmail" 75 0 4
  , Equipment "Platemail" 102 0 5
  ]

rings :: [Equipment]
rings = [
    Equipment "Damage +1"  25 1 0
  , Equipment "Damage +2"  50 2 0
  , Equipment "Damage +3" 100 3 0
  , Equipment "Defense +1" 20 0 1
  , Equipment "Defense +2" 40 0 2
  , Equipment "Defense +3" 80 0 3
  ]

player :: Player
player = Player 100 (Equipment "" 0 0 0) Nothing []

playerCost :: Player -> Int
playerCost p = cost (pWeapon p) + armorCost + sum (map cost (pRings p))
  where
    armorCost = maybe 0 cost (pArmor p)

winningEquipmentSets:: Player -> Boss -> [Player]
winningEquipmentSets p b = filter (`playerWins` b) (equipmentChoices p)

losingEquipmentSets :: Player -> Boss -> [Player]
losingEquipmentSets p b = filter (\x -> not $ x `playerWins` b) (equipmentChoices p)

equipmentChoices :: Player -> [Player]
equipmentChoices p = [Player (pHp p) s a r | s <- weapons, a <- armor', r <- rings']
    where
      armor' = Nothing:[Just x | x <- armor]
      rings' = [] : map (:[]) rings ++ combinations 2 rings

battle :: Player -> Boss -> (Player, Boss)
battle p b = head . dropWhile (not . uncurry gameOver) . iterate (uncurry turn) $ (p, b)

turn :: Player -> Boss -> (Player, Boss)
turn p b = if bossDead b' then (p, b') else (p', b')
  where
    (_, b') = playerMove p b
    (p', _) = bossMove p b'

gameOver :: Player -> Boss -> Bool
gameOver p b = playerDead p || bossDead b

playerDead :: Player -> Bool
playerDead = (0 ==) . pHp

bossDead :: Boss -> Bool
bossDead = (0 ==) . bHp

playerWins :: Player -> Boss -> Bool
playerWins p = (> 0) . pHp . fst . battle p

playerMove :: Player -> Boss -> (Player, Boss)
playerMove p b = (p, b')
  where
    b' = b {
      bHp = max 0 (bHp b - max 0 (playerDamage p - bArmorClass b))
    }

bossMove :: Player -> Boss -> (Player, Boss)
bossMove p b = (p', b)
  where
    p' = p {
      pHp = max 0 (pHp p - max 0 (bDamage b - playerArmorClass p))
    }

-- This assume armor cannot do damage
playerDamage :: Player -> Int
playerDamage p = damage (pWeapon p) + sum (map damage $ pRings p)

playerArmorClass :: Player -> Int
playerArmorClass p = armorValue + sum (map armorClass $ pRings p)
  where
    armorValue = maybe 0 armorClass (pArmor p)

