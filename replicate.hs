maps f [] = []
maps f (x:xs) = f [x] : maps f xs

rep a b
  |b==0 = []
  |b<0 = error "negative value"
  |otherwise = a ++ rep a (b-1)

replicas []=[]
replicas (x:xs) =rep (x:xs) x


replic (x:xs) = maps replicas (x:xs)
