module Tests where
import Test.QuickCheck


-- CASE 1
-- Onko `unlines (lines x) == x`?
-- Oletin, että on.

prop_unlines_lines = \x -> unlines (lines x) == x

-- TEST:
-- > quickCheck prop_unlines_lines
--  *** Failed! Falsifiable (after 2 tests and 1 shrink):
--"a"
-- unlines lisää uuden rivin aloituksen minkä takia merkkijono muuttuu
-- eivätkä nämä ole aina samat.

-- CASE 2
-- Onko `unlines (lines x)== x`?
--samasta syystä kuin edellinen ei ole.

prop_unlines_lines = \x -> lines (unlines x) == x

-- TEST
-- > quickCheck prop_unlines_lines
-- *** Failed! Falsified (after 14 tests and 8 shrinks):
-- ["\n"]
-- Sama rivinvaihto-ongelma.


-- CASE 3
-- Onko 'reverse (reverse x)' = x
-- Alunperin oletin, että on.

prop_reverse_reverse = \x -> reverse (reverse x) == x

-- Test.QuickCheck> quickCheck (\x -> reverse (reverse x) == x)
-- +++ OK, passed 100 tests.
-- Testit menivät läpi.

--CASE 4
-- Onko length xs > length (destutter xs)
-- Odotin, että ei ole, koska eihän listassa välttämättä ole duplikaatteja.

prop_length_nub = \xs -> length xs > length (nub xs)

-- Test
-- quickCheck (\xs -> length xs > length (destutter xs))
-- *** Failed! Falsified (after 1 test):
-- []
-- Tyhjän listan tapaus on jo poikkeus.

--CASE 5
-- Onko length xs > length (filter f xs)
-- En usko, että pätee, koska kaikki alkiot listassa voivat täyttää ehdon

prop_length_filter = \xs -> length xs > length (filter f xs)

--TEST
-- quickCheck (\xs -> length xs > length (filter even xs))
-- *** Failed! Falsified (after 1 test):
-- []
-- Testit eivät menneet läpi heti tyhjän listan tapauksessa.


--CASE 6
-- Onko destutter xs = destutter (destutter xs) / nub xs = nub (nub xs)
-- Pitäisi olla, koska toisella kierroksella ei pitäisi olla enää poistettavaa.

 prop_destutter_destutter = \xs -> destutter (destutter xs) == destutter xs

 -- TEST
 --quickCheck (\xs -> destutter (destutter xs) == destutter xs)
-- +++ OK, passed 100 tests.
-- Testit menivät läpi kuten oletin.

--CASE 7
-- Onko unwords (words xs) == xs
-- Tästä muistaakseni varoitettiin, että se ei täysin toimi.

prop_words_unwords = \ xs -> unwords (words xs) == xs

-- TEST
-- quickCheck (\xs -> unwords (words xs) ==  xs)
-- *** Failed! Falsified (after 15 tests and 4 shrinks):
-- " "
-- välilyönnit eivät siis säily oikein.

--CASE 8
-- Onko myConcat (group xs) == xs
-- Olettaisin, että on.

prop_concat_group = \xs -> myConcat (group xs) == xs

-- Test
-- quickCheck (\xs -> myConcat (group xs) ==  xs)
-- +++ OK, passed 100 tests.
-- Testit menivät läpi.


--CASE 9
-- Onko listan pituus aina lyhyempi drop-funktion käytön jälkeen?
-- Oletin että on, koska miten siitä voisi olla pudottamatta mitään?

prop_length_drop = \xs -> length (drop y xs) < length xs

-- TEST
-- quickCheck (\xs y -> length (drop y xs)  < length xs)
-- *** Failed! Falsified (after 1 test):
-- []
-- 0
-- En ajatellut tyhjän listan tapausta, joka tietenkään ei lyhene.

--CASE 10
-- Onko delete-funktion antaman listan pituus lyhyempi tai saman pituinen kuin
-- alkuperäinen lista?

prop_delete_length = \xs y -> length (delete y xs) <= length xs

--TEST
-- quickCheck (\xs y -> length (delete y xs) <= length xs)
-- +++ OK, passed 100 tests.
-- Testit menivät läpi, mutta kysmys taisikin olla vähän triviaali.

--CASE 11
-- Onko 'takeEvens (takeEvens xs) == takeEvens xs'
-- pitäisi olla, koska ei ensimmäisen kierroksen jälkeen pitäisi jäädä poistettavaa.

prop_takeEvens = \xs -> takeEvens (takeEvens xs) == takeEvens xs

-- TEST
-- quickCheck (\xs  -> takeEvens (takeEvens xs) == takeEvens xs )
-- +++ OK, passed 100 tests.
-- kuten arvelinkin


--CASE 12
-- Onko omatekemä mergeSort toimiva?
-- mergeSort xs == sort xs

prop_mergeSort = \xs -> mergeSort xs == sort xs

-- TEST
-- quickCheck (\xs -> mergeSort xs == sort xs)
-- +++ OK, passed 100 tests.
-- toimii.

-- CASE 13
-- onko kahteen kertaan lajiteltu lista sama?
-- pitäisi olla

prop_mergeSort = \xs -> mergeSort (mergeSort xs) = mergeSort xs

-- TEST
-- quickCheck (\xs -> mergeSort (mergeSort xs) == mergeSort xs)
-- +++ OK, passed 100 tests.
