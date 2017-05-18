module Random where


import System.Random (RandomGen, Random(randomR))


chooseRandom :: RandomGen g => g -> [a] -> (a, g)
chooseRandom rnd ls =
  let n = length ls
      (i, newRnd) = randomR (0, n - 1) rnd
      result = ls !! i
  in (result, newRnd)
