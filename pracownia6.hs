import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


type Generator a = [a]

type Pole = (Int,Int)
type Sciezka = [Pole]
type Wynik = [Sciezka]
type Wolne = Map Int (Map Int ())


data Zadanie = Zadanie {
        liczbaKolumn :: Int,
        liczbaWierszy :: Int,
        paryPol :: [(Pole,Pole)]
     }

plansza :: Int -> Int -> Wolne

--plansza 0 _ = []
plansza h w = Map.fromList [ (y, xs) | y <- [1..h] ]
	where xs =  Map.fromList [ (x,()) | x <- [1..w] ]


infixl 6 \\\
(\\\) :: Wolne -> [Pole] -> Wolne
wolne \\\ [] = wolne
wolne \\\	((h,w):ys) = (updateWithKey fun h wolne) \\\ ys
	where
		fun k map2 = Just $ (alter (\_ -> Nothing) w 



rozwiaz :: Zadanie -> Generator Wynik

rozwiaz zadanie = szukaj wolne (paryPol zadanie)
	where
		wolne :: Wolne
		wolne = (plansza (liczbaKolumn zadanie) (liczbaWierszy zadanie)) \\\ (concatMap pairToList (paryPol zadanie))
		pairToList (a,b) = [a,b]
		szukaj :: Wolne -> [(Pole,Pole)] -> Generator Wynik
		szukaj wolne [] = return []
		szukaj wolne (paraPkt:reszta) =
			do
				(noweWolne,xs) <- sciezki paraPkt wolne
				ys <- (szukaj noweWolne reszta)
				return $ [xs] ++ ys
		sciezki (a,b) wolne =
			if a==b then [(wolne,[a])] else
				do
					nast <- sasiedzi a
					let (maybe, wolneBezNext) = Map.updateLookupWithKey (\_ _ -> Nothing) nast wolne
					guard (nast == b || maybe /= Nothing)
					(noweWolne,xs) <- sciezki (nast,b) (wolne Map.\\ Map.singleton nast 0)
					return (noweWolne,(a:xs))
		sasiedzi (a,b) 	= [(a,b+1),(a,b-1),(a+1,b),(a-1,b)]
		--naPlanszy (a,b) = a > 0 && a <= (liczbaKolumn zadanie) && b > 0 && b <= (liczbaWierszy zadanie)
		--_ `myElem` [] 			= False
		--(1,x) `myElem` (xs:xss) = x `elem` xs
		--(y,x) `myElem` (_:xss)	= (y-1,x) `myElem` xss
		--osiagalne [] _ = True
		--osiagalne (para:reszta) wolne = (not $ null $ sciezki para wolne) && osiagalne reszta wolne