Theorem
-- For all finite lists `xs::[[a]]` and well behaving `f :: a -> b`
-- nael.n.soukouti@student.jyu.fi
concat (map (map f xs)) == map f (concat xs)

definitions:

map f []     = []             -- Map.1
map f (x:xs) = f x : map f xs -- Map.2

concat []     = []             -- Concat.1 
concat (x:xs) = x ++ concat xs -- Concat.2

Assumption: concat (map (map f) xs) == map f (concat xs)
Claim: concat (map (map f) (x:xs)) == map f (concat (x:xs))

a)
concat (map (map f) (x:xs))
== {- Map.2,  map g (y:ys) = g y : map g ys -}
concat (map f x:map (map f) xs)
== {- re-gruoping -}
concat ((map f x):(map (map f) xs))
=={- concat (x:xs) = x ++ concat xs  -}
map f x ++ concat (map (map f) xs)
== {- Assumption -}
map f x ++ map f (concat xs)


b) 
definitions:
[]     ++ ys = ys               -- (++).1
(x:xs) ++ ys = x:(xs++ys)       -- (++).2
map f []     = []             -- Map.1
map f (x:xs) = f x : map f xs -- Map.2

map f x ++ map f y == map f (x++y)

Muuttuva xs: 
Assumption: map f xs ++ map f ys == map f (xs++ys)
Claim: map f (x:xs) ++ map f ys == map f ((x:xs)++ys)

 map f (x:xs) ++ map f ys 
 == {- Map.2 -}
 f x : map f xs ++ map f ys
 == {-(++).2-}
 f x : (map f xs ++ map f ys)
 == {-assumption -}
 f x : (map f (xs++ys))
 == {-Map.2-}
 map f (x:(xs++ys))
 =={- (++).2 -}
 map f ((x:xs)++ys)

 Mikä todistaa väitteen, kun xs ei ole tyhjä lista.

 Näytetään vielä, että pätee kun xs on tyhjä.

 map f [] ++ map f ys 
 == {- Map.1-}
 [] ++ map f ys
 =={- (++).1 -}
 map f ys
 =={- (++).1 -}
 map f ([]++ys)

Muuttuva ys:

Assumption: map f xs ++ map f ys == map f (xs++ys)
Claim: map f xs ++ map f (y:ys) == map f (xs++(y:ys))

 map f xs ++ map f (y:ys) 
 == {- Map.2 -}
map f (xs++y:[]) : map f ys







 Todistaa väitteen tyhjälle listalle ja näin väite on todistettu.

