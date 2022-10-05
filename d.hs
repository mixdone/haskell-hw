solver :: Float -> Float -> Float -> [Float]
solver a b c
  | d < 0     = error ""
  | otherwise = [(-b + sqrt d) / 2 / a, (-b - sqrt d) / 2 / a]
    where d = b*b - 4*a*c
