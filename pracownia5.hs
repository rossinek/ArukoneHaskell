import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


type Generator a = [a]

type Pole = (Int,Int)
type Sciezka = [Pole]
type Wynik = [Sciezka]
type Wolne = Map Pole Int


data Zadanie = Zadanie {
        liczbaKolumn :: Int,
        liczbaWierszy :: Int,
        paryPol :: [(Pole,Pole)]
     }

plansza :: Int -> Int -> Wolne

--plansza 0 _ = []
plansza h w = Map.fromList [ ((y,x),0) | y <- [1..h], x <- [1..w] ]

{-
infixl 6 \\\
(\\\) :: Wolne -> [Pole] -> Wolne
wolne \\\ [] = wolne
wolne \\\	((h,w):ys) = (usun h w wolne) \\\ ys
	where
		usun 1 w (xs:xss) = (xs \\ [w]) : xss
		usun h w (xs:xss) = xs : usun (h-1) w xss
-}
rozwiaz :: Zadanie -> Generator Wynik

rozwiaz zadanie = szukaj wolne (paryPol zadanie)
	where
		wolne :: Wolne
		wolne = (plansza (liczbaKolumn zadanie) (liczbaWierszy zadanie)) Map.\\ (Map.fromList $ concatMap pairToList (paryPol zadanie))
		pairToList (a,b) = [(a,0),(b,0)]
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