import Data.Maybe 

_gcdTrail :: Int -> Int -> [[Int]] -> [[Int]]
_gcdTrail 0 _ res = res 
_gcdTrail _ 0 res = res 
_gcdTrail x y res 
  | x < y     = let q = y `quot` x  
                    r = y - q * x 
                 in _gcdTrail x r ([x,q,r]: res)
  | otherwise = let q = x `quot` y
                    r = x - q*y
                 in _gcdTrail y r ([y,q,r]:res)

--returns an array of arrays of the form [r_{n+1}, q_{n+1}, r_{n+2}], where r_n = r_{n+1} q_{n+1} + r_{n+2}
gcdTrail :: Int -> Int -> [[Int]]
gcdTrail x y 
  | x < y     = _gcdTrail y x [[y,0,x]] 
  | otherwise = _gcdTrail x y [[x,0,y]]

_backTrail :: [[Int]] -> [[Int]] 
_backTrail [] = [] 
_backTrail (x:xs) 
  | null xs   = xs
  | otherwise =
    [1, head $ head xs, -(x !! 1), head x] : _backTrail xs 

--r = u*x + v*y, then backTrail returns an array of arrays like [u,x,v,y], one entry for each residue.
backTrail :: Int -> Int -> [[Int]]
backTrail x y = _backTrail $ tail $ gcdTrail x y 


_gcdLC :: [[Int]] -> [Int]
_gcdLC [] = []
_gcdLC (x:xs) 
  | null xs   = x
  | otherwise =  let u1 = x  !! 0 
                     x1 = x  !! 1 
                     v1 = x  !! 2 
                     y1 = x  !! 3 
                     _x = head xs 
                     u2 = _x !! 0 
                     x2 = _x !! 1 
                     v2 = _x !! 2 
                     y2 = _x !! 3 
                  in 
                     if u2*x2 + v2*y2 == y1
                        then _gcdLC ([(v1*v2)+u1,  x1,      v1*u2, x2]: tail xs)
                        else _gcdLC ([u1*u2     ,  x2, (u1*v2)+v1, y1]: tail xs)

--Returns a pair (g, [u,x,v,y]) such that g = gcd x y and u*x + v*y = g
gcdLC :: Int -> Int -> (Int, [Int])
gcdLC x y = (gcd x y, _gcdLC $ backTrail x y)

--Given a,n find k such that ak = 1 mod n 
inverseModuloN :: Int -> Int -> Maybe Int  
inverseModuloN _ 0 = Nothing 
inverseModuloN a n 
  | a > n     = inverseModuloN (a `mod` n) n  
  | a == 1    = Just 1 
  | otherwise = let _d = gcdLC a n
                    g  = fst _d 
                    d  = snd _d
                 in if g /= 1  --waste to do all of that work and then finally check this condition... I should rewrite this...
                       then Nothing 
                       else if d !! 1 == n 
                               then Just $ (d !! 2) `mod` n 
                               else Just $ (d !! 0) `mod` n

--Counts number of coprime elements 
_totient :: Int -> Int -> Int -> Int 
_totient total a n 
  | a == n                         = total 
  | isNothing $ inverseModuloN a n = _totient total (a+1) n 
  | otherwise                      = _totient (total+1) (a+1) n

totient :: Int -> Int 
totient 0 = 1 
totient 1 = 1 
totient n 
  | n < 0     = error $ "totient takes one non negative argument of type Integer. You gave: " ++ (show n)
  | otherwise =  _totient 0 0 n

