module MiniMIDI
  ( MusObj
  , stretch
  , noteCount
  , copy
  , getDur
  , voice1
  , voice2
  , exemple
  , transpose
  , mirror
  , retrograde
  , mkList
  , repeatN
  , canon
  , nextChord
  )
where

data MusObj =  Note Integer Integer Integer | Rest Integer | Sequential [MusObj] | Parallel [MusObj] deriving (Show)


voice1 = Sequential
  [ Note 60 1000 100
  , Note 64 500  100
  , Note 62 500  100
  , Rest 1000
  , Note 67 1000 100
  ]

voice2 = Sequential [Note 52 2000 100, Note 55 1000 100, Note 55 1000 100]

exemple = Parallel [voice1, voice2]

-- Calcule le prochain accord
nextChord :: MusObj -> Maybe ([MusObj],MusObj)
nextChord ( Rest   0         ) = Nothing
nextChord ( Rest   d         ) = Just ([], Rest (d-1) )
nextChord ( Note p 0 v       ) = Nothing
nextChord ( Note p d v       ) = Just ([Note p 1 v], Note p (d-1) v)
nextChord ( Sequential []    ) = Nothing  
nextChord ( Sequential (h:t) ) = case nextChord h of
                                     Nothing -> nextChord (Sequential t)
                                     Just (c,ct) -> Just (c,Sequential (ct:t))
nextChord ( Parallel   []    ) = Nothing  
nextChord ( Parallel   (h:t) ) = 
  case (nextChord h, nextChord (Parallel t)) of
    (Nothing , Nothing ) -> Nothing 
    (c, Nothing ) -> c
    (Nothing , t )-> t
    (Just (ch,ct), Just (th,tt)) -> Just (ch ++ th,Parallel [ct,tt])


--Calcule la duree d'un objet musical
getDur :: MusObj -> Integer
getDur (Rest d          ) = d
getDur (Note p d v      ) = d
getDur (Sequential elems) = sum (map getDur elems)
getDur (Parallel   elems) = foldl max 0 (map getDur elems)

--Copy un objet musical
copy :: MusObj -> MusObj
copy (Note p d v  ) = Note p d v
copy (Rest       d) = Rest d
copy (Sequential l) = Sequential (map copy l)
copy (Parallel   l) = Parallel (map copy l)

--Compte le nombre de notes d'un objet musical
noteCount :: MusObj -> Integer
noteCount (Note p d v  ) = 1
noteCount (Parallel   l) = sum (map noteCount l)
noteCount (Sequential l) = sum (map noteCount l)
noteCount _              = 0

--Strech un objet musical par un factor fact
stretch :: MusObj -> Float -> MusObj
stretch (Note p d v  ) fact = Note p (round (fromIntegral d * fact)) v
stretch (Rest       d) fact = Rest (round (fromIntegral d * fact))
stretch (Parallel   l) fact = Parallel (map (`stretch` fact) l)
stretch (Sequential l) fact = Sequential (map (`stretch` fact) l)

--Transpose obj de n demitons
transpose :: MusObj -> Integer -> MusObj
transpose (Note p d v  ) n = Note (p + n) d v
transpose (Rest       d) n = Rest (n + d)
transpose (Parallel   l) n = Parallel (map (`transpose` n) l)
transpose (Sequential l) n = Sequential (map (`transpose` n) l)

--mirror de obj au tour du center c 
mirror :: MusObj -> Integer -> MusObj
mirror (Note p d v  ) c = Note (c - (p - c)) d v
mirror (Rest       d) c = Rest d
mirror (Parallel   l) c = Parallel (map (`mirror` c) l)
mirror (Sequential l) c = Sequential (map (`mirror` c) l)

--retrograde un obj  
retrograde :: MusObj -> MusObj
retrograde (Sequential l) = Sequential (reverse (map retrograde l))
retrograde obj            = obj

--make a list of n obj 
mkList :: MusObj -> Integer -> [MusObj]
mkList obj 0 = []
mkList obj n = copy obj : mkList obj (n - 1)

--make a sequential avec n fois obj 
repeatN :: MusObj -> Integer -> MusObj
repeatN obj n = Sequential (mkList obj n)

--make obj en parallele avec lui meme avec un decalage de n ms.
canon :: MusObj -> Integer -> MusObj
canon obj n = Parallel [copy obj, Sequential [Rest n, copy obj]]
