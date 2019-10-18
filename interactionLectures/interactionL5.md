# Answers for interaction lecture 5

## L5_Q1_2018_2

### What does => mean in the following code?

class Semigroup a where (<>) :: a -> a -> a

class Semigroup a => Monoid a where   
mempty :: a

**Mark these either as True or False:**

You can use <> in the implementation of f :: Monoid a => [a]->a T 

You can use mempty in the implementation off :: Semigroup a => [a]->a` F

Everything that is a Monoid is also a Semigroup T

Everything that is a Semigroup is also a Monoid F

Selitys: Jos joku on monoidi se on myös semigroup. Itse toteutetuille monoideille ja semigroupeille täytyy määritellä instanssit näistä tyyppiluokista.

## L5_Q3_2018_2
class HasCapacity f  where   
capacity :: f a -> Int  
example :: f a -> Maybe a  
-- Laws: ∀x. capacity > 0 ==> isJust (example)  

#### which of the following instance declarations are fine?

Mark these either as True or False:

instance HasCapacity Int where ...F

instance HasCapacity Either where ... F

instance HasCapacity (Either String) where ... T

instance HasCapacity (Either String Int) where ... F

Selitys: käsittääkseni tässä tarvitaan tyyppikonstruktori, jonka
kind on * -> *  


## L5_Q2_2018_2

class Blub a where  
blub :: a -> String  


instance Blub a => [a]  where ...  
instance Blub ()        where ...  
instance Blub (Maybe a) where ...  
which of the following applications would work?

Mark these either as True or False:  
blub [[[[(),()],[]]]] F

blub [(),()] F

blub [1,2,3] F

blub (Just [1,2,3]) T 

selitys: Tuon lista-instanssin luominen ei onnistu. Tuossa kerrotaan, että a:n pitää olla luokasta Blub, mutta listoille a:sta ei luoda Blub instanssia.

## L5_Q4_2018_2
### Which of these are monoids?

#### Mark these either as True or False:
Int, a<>b=a+b, mempty=0 T

Int, a<>b=a*b, mempty=0 F

forall a. a -> a, f<>g=\x -> f (g x), mempty=\x->x T

forall a. Monoid a=> (a,a,a), (x,y,z)<>(h,j,k)=(x<>h,y<>j,z<>k), mempty=(mempty,mempty,mempty)  T

forall a. Monoid a=> (a,a,a), (x,y,z)<>(h,j,k)=(x<>k,y<>j,z<>h), mempty=(mempty,mempty,mempty) F

Selitys: True:t täyttävät monoidien lait.

## L5_Q5_2018_2
### Which of the following can be given a Functor instance?

Mark these either as True or False:
Int F

data Vector = Vec Int Int Int F 

data Vector a = Vec a a a T

data Vector a = Vec [a] T 

Selitys: näissä True tapauksissa rakenteen sisältöä voidaan muuttaa funktorilakien mukaisesti.


## L5_Q6_2018_2
### Functors have laws. Which of the following would be law-abiding fmaps for data Counter a = Counter a Int?

#### Mark these either as True or False:
fmap f (Counter a n) = Counter (f a) 0 - F ( rikkoo id - sääntöä)

fmap f (Counter a n) = Counter (f a) (n+1) - F ( rikkoo id - sääntöä)

fmap f (Counter a n) = Counter a (f n) - F ei voida käyttää f:ää Int:iin (se ei saa muuttua)

fmap f (Counter a n) = Counter (f a) n - T ( tämä on ainut Ok mielestäni näistä)

## L5_Q7_2018_2
### Which of the following can be given a Functor instance? (This is a hard question)

#### Mark these either as True or False:
data AllBoolFuns = A (Bool -> Bool) - F (Koska tämän kind on *)

data ConstBool a = C Bool - F Mielestäni tässä rakenteessa tyyppi ei voi muuttua sisällä

data Pred a = P (a -> Bool) F - Nämä menee nyt veikkaukseksi

data Select a = S (Bool -> a) T - Nämä menee nyt veikkaukseksi

Kahteen viimeiseen kaipaisin selvennystä.
