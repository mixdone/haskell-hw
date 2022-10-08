inc x | x >= 0 = x + 1
      | otherwise = error "Arg must be positive!"
dec x | x > 0 = x - 1
      | x == 0 = 0
      | otherwise = error "Arg must be positive!"

--------------------------------------------------------------------------------

-- 1
--------------------------------------------------------------------------------
pls :: Integer -> Integer -> Integer
pls 0 y = y
pls x 0 = x
pls x y | x < 0 = error "Arg must be positive!"
        | y < 0 = error "Arg must be positive!"
        | otherwise = pls (inc x) (dec y)

mns :: Integer -> Integer -> Integer
mns x 0 = x
mns x y | x < 0 = error "Arg must be positive!"
        | y < 0 = error "Arg must be positive!"
        | x < y = 0
        | otherwise = mns (dec x) (dec y)

ml :: Integer -> Integer -> Integer -> Integer
ml 0 _ _ = 0
ml _ 0 _ = 0
ml 1 y _ = y
ml x 1 _ = x
ml x y n = ml (pls x n) (dec y) n

mlt x y | x < 0 = error "Arg must be positive!"
        | y < 0 = error "Arg must be positive!"
        | otherwise = ml x y x
--------------------------------------------------------------------------------

-- 2
--------------------------------------------------------------------------------
mymin :: Integer -> Integer -> Integer
mymin x y | x < 0 = error "Arg must be positive!"
          | y < 0 = error "Arg must be positive!"
          | x > y = y
          | otherwise = x

mymax :: Integer -> Integer -> Integer
mymax x y | x < 0 = error "Arg must be positive!"
          | y < 0 = error "Arg must be positive!"
          | x > y = x
          | otherwise = y
--------------------------------------------------------------------------------

-- 3
--------------------------------------------------------------------------------
myd1 :: Integer -> Integer -> Integer -> Integer -> Integer
myd1 _ 0 _ _ = error "Can't divide by 0"
myd1 x y s n | (y > x) && (n == 0) = 0
             | x == y =  dec n
             | y > x = dec $ dec n
             | otherwise = myd1 x (mlt s n) s (n+1)


mydiv1 x y | x < 0 = error "Arg must be positive!"
           | y < 0 = error "Arg must be positive!"
           | otherwise = myd1 x y y 1

--------------------------------------------------------------------------------

-- 4
--------------------------------------------------------------------------------
myd2 :: Integer -> Integer -> Integer -> Integer
myd2 0 _ _ = 0
myd2 x y n | mlt y n > x = dec n
           | mlt y n == x = n
           | mlt y n < x = myd2 x y (inc n)

mydiv2 x y | x < 0 = error "Arg must be positive!"
           | y <= 0 = error "Arg must be positive!"
           | otherwise = myd2 x y 0
--------------------------------------------------------------------------------

-- 5
--------------------------------------------------------------------------------
mymod1 :: Integer -> Integer -> Integer
mymod1 x y | y == 0 = error "Can't divide by 0"
           | x < 0 = error "Arg must be positive!"
           | y < 0 = error "Arg must be positive!"
mymod1 x y | x < y = x
           | otherwise = mymod1 (mns x y) y

mym2 :: Integer -> Integer -> Integer -> Integer
mym2 _ _ 0 = error "Can't divide by 0"
mym2 x y n  | x > y = mym2 x (pls y n) n
            | otherwise = mns n (mns y x)

mymod2 x y | x < 0 = error "Arg must be positive!"
           | y <= 0 = error "Arg must be positive!"
           | otherwise = mym2 x y y

--------------------------------------------------------------------------------

-- 6
--------------------------------------------------------------------------------
d_v:: Integer -> Integer -> Integer -> Bool
d_v 0 _ _ = True
d_v x y t | mlt y t == x = True
          | mlt y t > x = False
          | mlt y t < x = d_v x y (inc t)

divide x y | y <= 0 = error "Arg must be positive!"
           | otherwise = d_v x y 1

--------------------------------------------------------------------------------

-- 7
--------------------------------------------------------------------------------
nd::Integer -> Integer -> Integer -> Integer
nd 1 _ _ = 1
nd x n m | x == n = m + 1
         | divide x n == True = nd x (n + 1) (m + 1)
         | divide x n == False = nd x (n + 1) m

countdividers x | x <= 0 = error "Arg must be positive!"
                | otherwise = nd x 1 0

--------------------------------------------------------------------------------

-- 8
--------------------------------------------------------------------------------
sumd1:: Integer -> Integer -> Integer -> Integer
sumd1 1 _ _ = 1
sumd1 x n m | x == n = m + n
           | divide x n == True = sumd1 x (n + 1) (m + n)
           | divide x n == False = sumd1 x (n + 1) m

sumd x | x <= 0 = error "Arg must be positive!"
       | otherwise = sumd1 x 1 0

--------------------------------------------------------------------------------

-- 9
--------------------------------------------------------------------------------
prime1:: Integer -> Bool
prime1 1 = False
prime1 x | countdividers x == 2 = True
         | otherwise = False

prime x | x <= 0 = error "Arg must be positive!"
        | otherwise = prime1 x

--------------------------------------------------------------------------------

-- 10
--------------------------------------------------------------------------------
pnd1 :: Integer -> Integer -> Integer -> Integer
pnd1 x n count | x < n = count
               | divide x n == False = pnd1 x (n + 1) count
               | prime n == False = pnd1 x (n + 1) count
               | otherwise = pnd1 x (n + 1) (count + 1)

pnd x | x <= 0 = error "Arg must be positive!"
      | otherwise = pnd1 x 2 0

--------------------------------------------------------------------------------

-- 11
--------------------------------------------------------------------------------
nod1::Integer -> Integer -> Integer -> Integer
nod1 x y 1 = 1
nod1 x y n | (divide x n) && (divide y n) = n
           | otherwise = nod1 x y (n - 1)

nod x y | x <= 0 = error "Arg must be positive!"
        | y <= 0 = error "Arg must be positive!"
        | otherwise = nod1 x y y

--------------------------------------------------------------------------------

-- 12
--------------------------------------------------------------------------------
nok1:: Integer -> Integer -> Integer -> Integer
nok1 x y n | n == x * y = x * y
nok1 x y n | (divide n x) && (divide n y) = n
           | otherwise = nok1 x y (n + 1)

nok x y | x <= 0 = error "Arg must be positive!"
        | y <= 0 = error "Arg must be positive!"
        | otherwise = nok1 x y y

--------------------------------------------------------------------------------

-- 13
--------------------------------------------------------------------------------
m_g1:: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
m_g1 f i x c | (f i x) == 0 = i
             | c > 100 = error "mostly likely the function is not defined"
             | (f i x) >= (f (i - 1) x) = m_g1 f (i + 1) x (c + 1)
             | otherwise = m_g1 f (i + 1) x 0
func1 f x = m_g1 f 0 x 0

m_g2:: (Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer -> Integer
m_g2 f i x1 x2 c | (f i x1 x2) == 0 = i
                 | c > 100 = error "mostly likely the function is not defined"
                 | (f i x1 x2) >= (f (i - 1) x1 x2) = m_g2 f (i + 1) x1 x2 (c + 1)
                 | otherwise = m_g2 f (i + 1) x1 x2 0
func2 f x1 x2 = m_g2 f 0 x1 x2 0


--------------------------------------------------------------------------------

-- 14
--------------------------------------------------------------------------------
m_g3:: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer -> Integer
m_g3 p i x c | (p i x) == True = i
             | c > 1000 = error "mostly likely the function is not defined"
             | otherwise = m_g3 p (i + 1) x (c+1)
predicat p x = m_g1 p 0 x 0

--------------------------------------------------------------------------------

-- 15
--------------------------------------------------------------------------------
mns1::Integer -> Integer -> Integer  -- crutch :)
mns1 x y | (x+1)*(x+1) > y = 0
             | x*x < y = 1
             | x*x >= y = 0

root::Integer -> Integer
root n = func1 f n where
  f  = (\i x -> (mns1 i x))

--------------------------------------------------------------------------------

-- 16
--------------------------------------------------------------------------------
mns2::Integer -> Integer -> Integer -> Integer
mns2 x1 x2 i | x2 * i >= x1 = 0
             | otherwise = 1

minus :: Integer -> Integer -> Integer
minus x1 x2 = func2 f x1 x2 where
  f = (\i x1 x2 -> (mns2 x1 x2 (i+1)))
