module ForSyDe.Atom.Skel.FastVector.Cube where

import ForSyDe.Atom.Skel.FastVector (Vector(..), vector, fromVector, (<++>))
import ForSyDe.Atom.Skel.FastVector.Matrix (Matrix, matrix, fromMatrix)

import qualified ForSyDe.Atom.Skel.FastVector as V
import qualified ForSyDe.Atom.Skel.FastVector.Matrix as M

-- | 'Cube' is simply a type synonym for vector of vectors. This
-- means that /any/ function on 'Vector' works also on 'Cube'.
type Cube a = Vector (Vector (Vector a))

-- | Prints out to the terminal a cube in a readable format, where
-- all elements are right-aligned and separated by a custom separator.
--
-- >>> let m = cube 2 2 2 [1,2,3,3,100,4,12,32]
-- >>> pretty "|" m
-- --------
-- 1|2
-- 3|3
-- --------
-- 100| 4
--  12|32
-- --------
pretty :: Show a
       => String   -- ^ separator string
       -> Cube a -- ^ input cube
       -> IO ()
pretty sep mat = mapM_ (\m -> putStrLn "--------" >> M.pretty sep m) mat >> putStrLn "--------"

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.isNull'.
isNull :: Matrix a -> Bool
isNull (Vector []) = True
isNull (Vector [Vector []]) = True
isNull _ = False

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.size'.
size :: Cube a -> (Int,Int,Int)
size m = (x,y,z)
  where
    z = V.length (wellFormed m)
    y = (V.length . V.first) m
    x = (V.length . V.first . V.first) m

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.wellFormed'.
wellFormed :: Cube a -> Cube a
wellFormed (Vector []) = Vector []
wellFormed (Vector (x:xs)) = Vector $ M.wellFormed x :
                             (fromVector $ wellFormed $ Vector xs)

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.cube'.
cube :: Int      -- ^ number of columns (X dimension) @= x@
     -> Int      -- ^ number of rows (Y dimension) @= y@
     -> Int      -- ^ depth (Z dimension) @= z@
     -> [a]      -- ^ list of values; /length/ = @x * y * z@
     -> Cube a -- ^ 'Cube' of values; /size/ = @(x,y,z)@
cube x y z = vector . Prelude.map (matrix x y) . M.groupEvery (x * y) . check
  where
    check l | length l == x * y * z = l
            | otherwise
      = error $ "cannot form cube (" ++ show x ++ ","
              ++ show y ++ "," ++ show z ++ ") from a list with "
              ++ show (length l) ++ " elements"

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.fromCube'.
fromCube :: Cube a -- ^ /size/ = @(x,y)@
         -> [a]    -- ^ /length/ = @x * y@
fromCube = concatMap fromVector . fromMatrix

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.unit'.
unit :: a -> Cube a -- ^ /size/ = @(1,1)@
unit a = Vector [Vector [Vector [a]]]

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.fanout'.
fanout :: a -> Cube a
fanout n = V.fanout $ V.fanout $ V.fanout n

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.indexes'.
indexes :: Cube (Int, Int, Int)
indexes = farm31 (,,) colix rowix depthix
  where
    colix = vector $ repeat $ vector $ repeat $ vector [0..]
    rowix = V.farm11 M.transpose colix
    depthix =  M.transpose $ V.farm11 M.transpose colix
    
-- | See 'ForSyDe.Atom.Skel.Vector.Cube.transpose'.
transpose :: Cube a -- ^ dimensions @(Z,Y,X)@
           -> Cube a -- ^ dimensions @(Y,X,Z)@
transpose = V.farm11 M.transpose . M.transpose

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.transpose''.
transpose' :: Cube a -- ^ dimensions @(Y,X,Z)@
            -> Cube a -- ^ dimensions @(Z,Y,X)@
transpose' = M.transpose . V.farm11 M.transpose

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.farm11'.
farm11 :: (a -> b)
       -> Cube a -- ^ /size/ = @(xa,ya)@
       -> Cube b -- ^ /size/ = @(xa,ya)@
farm11 = V.farm11 . V.farm11 . V.farm11

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.farm21'.
farm21 :: (a -> b -> c)
           -> Cube a -- ^ /size/ = @(xa,ya)@
           -> Cube b -- ^ /size/ = @(xb,yb)@
           -> Cube c -- ^ /size/ = @(minimum [xa,xb], minimum [ya,yb])@
farm21 f = V.farm21 (V.farm21 (V.farm21 f))

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.farm31'.
farm31 :: (a -> b -> c -> d)
            -> Cube a -- ^ /size/ = @(xa,ya)@
            -> Cube b -- ^ /size/ = @(xb,yb)@
            -> Cube c -- ^ /size/ = @(xc,yc)@
            -> Cube d -- ^ /size/ = @(minimum [xa,xb,xc], minimum [ya,yb,yc])@
farm31 f = V.farm31 (V.farm31 (V.farm31 f))

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.reduce'.
reduce :: (a -> a -> a) -> Cube a -> a
reduce f = V.reduce f . V.farm11 (M.reduce f)

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.get'.
get :: Int       -- ^ X index starting from zero
    -> Int       -- ^ Y index starting from zero
    -> Int       -- ^ Z index starting from zero
    -> Cube a
    -> Maybe a
get x y z = getMaybe . V.get z
  where getMaybe Nothing = Nothing
        getMaybe (Just a) = M.get x y a

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.take'.
take :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Int       -- ^ > index starting from zero
     -> Cube a
     -> Cube a
take x y z = V.farm11 (M.take x y) . V.take z

-- | See 'ForSyDe.Atom.Skel.Vector.Cube.drop'.
drop :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Int       -- ^ Z index starting from zero
     -> Cube a
     -> Cube a
drop x y z = V.farm11 (M.drop x y) . V.drop z


