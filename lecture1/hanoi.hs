
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disk source dest spare
  | disk == 1 = [(source, dest)]
  | otherwise = hanoi (disk-1) source spare dest ++
                [(source, dest)] ++
                hanoi (disk-1) spare dest source