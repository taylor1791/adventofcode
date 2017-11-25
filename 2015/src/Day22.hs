module Day22 where

import           Data.List (find)

type Turns = Int

data Effect =
              EShield Turns
            | EPoison Turns
            | ERecharge Turns
            deriving (Eq, Show)

data Player = Player {
              pHp      :: Int
            , pMana    :: Int
            , pEffects :: [Effect]
            } deriving (Eq, Show)

data Boss = Boss {
            bHp      :: Int
          , bDamage  :: Int
          , bEffects :: [Effect]
          } deriving (Eq, Show)

data Spell =
              Missile
            | Drain
            | Shield
            | Poison
            | Recharge
            deriving (Eq, Show)

class Character a where
  hp :: a -> Int
  takeDamage :: Int -> a -> a
  addEffect :: Effect -> a -> a
  isDead :: a -> Bool
  isDead = (0 == ) . hp

instance Character Boss where
  hp = bHp
  takeDamage n a = a { bHp = max 0 (bHp a - n) }
  addEffect e a = a { bEffects = e : bEffects a }

instance Character Player where
  hp = pHp
  takeDamage n a = a { pHp = max 0 (pHp a - n) }
  addEffect e a = a { pEffects = e : pEffects a }

loseMana :: Int -> Player -> Player
loseMana n p = p { pMana = pMana p - n }

mana :: Spell -> Int
mana Missile = 53
mana Drain = 73
mana Shield = 113
mana Poison = 173
mana Recharge = 229

isEShield :: Effect -> Bool
isEShield (EShield _) = True
isEShield _           = False

isEPoisoned :: Effect -> Bool
isEPoisoned (EPoison _) = True
isEPoisoned _ = False

isERecharge :: Effect -> Bool
isERecharge (ERecharge _) = True
isERecharge _ = False

hasShield :: Player -> Bool
hasShield = any isEShield . pEffects

isPoisoned :: Boss -> Bool
isPoisoned = any isEPoisoned . bEffects

isRecharge :: Player -> Bool
isRecharge = any isERecharge . pEffects

cast :: Spell -> (Player, Boss) -> (Player, Boss)
cast Missile (p, b) = (loseMana 53 p, takeDamage 4 b)
cast Drain (p, b) = (takeDamage (-2) $ loseMana 73 p, takeDamage 2 b)
cast Shield (p, b)
  | hasShield p = (p {pHp = 0}, b)
  | otherwise   = (addEffect (EShield 6) $ loseMana 113 p, b)
cast Poison (p, b)
  | isPoisoned b = (p {pHp = 0}, b)
  | otherwise    = (loseMana 173 p, addEffect (EPoison 6) b)
cast Recharge (p, b)
  | isRecharge p = (p {pHp = 0}, b)
  | otherwise    = (addEffect (ERecharge 5) $ loseMana 229 p, b)

applyPlayerEffect :: Effect -> Player -> Player
applyPlayerEffect (ERecharge 1) = loseMana (-101)
applyPlayerEffect (ERecharge n) = addEffect (ERecharge (n-1)) . applyPlayerEffect (ERecharge 1)
applyPlayerEffect (EShield 1) = id
applyPlayerEffect (EShield n) = addEffect $ EShield (n-1)

applyBossEffect :: Effect -> Boss -> Boss
applyBossEffect (EPoison 1) = takeDamage 3
applyBossEffect (EPoison n) = addEffect (EPoison (n-1)) . applyBossEffect (EPoison 1)

applyEffects :: (Player, Boss) -> (Player, Boss)
applyEffects (p, b) = (p', b')
  where
    p' = foldr applyPlayerEffect (p {pEffects = []}) $ pEffects p
    b' = foldr applyBossEffect (b {bEffects = []}) $ bEffects b

bossAttack :: (Player, Boss) -> (Player, Boss)
bossAttack (p, b)
  | isDead b  = (p, b)
  | otherwise = (takeDamage dam p, b)
    where
      armor = maybe 0 (const 7) $ find isEShield $ pEffects p
      dam = max 0 $ bDamage b - armor

isGameOver :: (Player, Boss) -> Bool
isGameOver (p, b) = isDead p || isDead b

turn :: Spell -> (Player, Boss) -> (Player, Boss)
turn s (p, b)
  | pMana p < mana s = (takeDamage (hp p) p, b)
  | otherwise        = turnBind bossAttack . turnBind applyEffects . turnBind (cast s) . turnBind applyEffects $ (p, b)

turnBind :: ((Player, Boss) -> (Player, Boss)) -> (Player, Boss) -> (Player, Boss)
turnBind f x = if isGameOver x then x else f x

play :: (Player, Boss) -> [Spell] -> (Player, Boss)
play = foldl (flip turn)

allSpells :: [Spell]
allSpells = [Missile, Drain, Shield, Poison, Recharge]

spells :: Int -> [[Spell]]
spells 0 = [[]]
spells n = allSpells >>= flip map (spells (n - 1)) . (:)

playerWins :: (Player, a) -> Bool
playerWins (p, _) = hp p > 0

cost :: [Spell] -> Int
cost = sum . map mana

turn' :: Spell -> (Player, Boss) -> (Player, Boss)
turn' s = turn s . lose1Hp

lose1Hp :: (Player, Boss) -> (Player, Boss)
lose1Hp (p, b) = (takeDamage 1 p, b)

play' :: (Player, Boss) -> [Spell] -> (Player, Boss)
play' = foldl (flip turn')


