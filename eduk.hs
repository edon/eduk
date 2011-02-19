addTest        :: (Integral a) => (a,a) -> [(a,a)] -> a -> [(a,a)]
addTest _ [] _ = []
addTest (w,p) ((j,f):s) c
    |j+w>c     = []
    |otherwise = (j+w,f+p):(addTest (w,p) s c)

merge                :: (Integral a) => [(a,a)] -> [(a,a)] -> [(a,a)]
merge [] s           = s
merge s []           = s
merge ((j1,f1):s1) ((j2,f2):s2)
          |j1<j2     = (j1,f1):(merge s1 ((j2,f2):s2))
          |j2<j1     = (j2,f2):(merge ((j1,f1):s1) s2)
          |otherwise = (j1,max f1 f2):(merge s1 s2)

sparse         :: (Integral a) => [a] -> [a] -> a -> Int -> [(a,a)]
sparse _ _ _ 0 = []
sparse ws ps c k = 
    merge (sparse ws ps c (k-1)) (addTest (ws!!k,ps!!k) ((0,0):(sparse ws ps c k)) c)

ssolve         :: (Integral a) => [a] -> [a] -> a -> a
ssolve ws ps c = snd . last $ sparse (0:ws) (0:ps) c (length ws)

collectivlyDominated :: (Integral a) => [a] -> [a] -> a -> a -> Bool
collectivlyDominated ws ps w p = ssolve ws ps w > p

filterCollectiveDominance :: (Integral a) => [(a,a)] -> [(a,a)]
filterCollectiveDominance wps = f wps wps
    where
      f _ [] = []
      f fwps ((w,p):wps)
          | collectivlyDominated ws ps w p = f fwps wps
          | otherwise = (w,p):(f fwps wps)
          where ws = (fst . unzip) fwps
                ps = (snd . unzip) fwps

solve :: (Integral a) => [(a,a)] -> a -> a
solve wps c = ssolve ws ps c
    where ws = (fst . unzip) filtered
          ps = (snd . unzip) filtered
          filtered = filterCollectiveDominance wps
