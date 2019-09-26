import Vector

type Vec a = Vec2D a
type R = Double

data Pos = Vec R
data Move = Vec R
type Time = R

data Particle = Particle { mass :: R, charge :: R, position :: Vec R, velocity :: Vec R }

type Force = Vec R

newtype ForceFunc = ForceFunc Particle -> Particle -> Force

instance Semigroup ForceFunc where
    (ForceFunc f) <> (ForceFunc g) = ForceFunc $ \p1 p2 -> f p1 p2 + g p1 p2

instance Monoid ForceFunc where
    mempty = ForceFunc $ \_ _ -> 0

-- сила, действующая на 1 со стороны 2
coulombForce :: Particle -> Particle -> Force
coulombForce p1 p2 = k * charge p1 * charge p2 / (abs distance ** 3) * distance where
    distance = position p1 - position p2

data Setup = Setup { particles :: [Particle], potentialForce :: ForceFunc, links :: [(Int, Int)] }

strain maxLen potentialForce radius = if abs radius >= maxLen && projection < 0 then -projection * unit radius else pure 0 where
    projection = proj potentialForce radius



main = do
    s = Setup particles (ForceFunc coulombForce) linked where
        particles = [
            Particle 0.001 -1 (pure 3) (pure 0),
            Particle 0.001 -1 (pure -3) (pure 0)
            Particle 0.001 -1 (pure -1) (pure 0)
        ],
        linked = [
            (0, 1),
            (0, 2)
        ]

