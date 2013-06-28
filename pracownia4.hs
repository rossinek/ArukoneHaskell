import Control.Monad
import Data.List

type Generator a = [a]

type Pole = (Int,Int)
type Sciezka = [Pole]
type Wynik = [Sciezka]
type Wolne = [[Int]]


data Zadanie = Zadanie {
        liczbaKolumn :: Int,
        liczbaWierszy :: Int,
        paryPol :: [(Pole,Pole)]
     }

plansza :: Int -> Int -> Wolne

plansza 0 _ = []
plansza h w = [1..w] : plansza (h-1) w

--zad = Zadanie 3 3 [((1,1),(3,3))]
--wolne = plansza (liczbaKolumn zad) (liczbaWierszy zad) \\\ map fst (paryPol zad)

infixl 6 \\\
(\\\) :: Wolne -> [Pole] -> Wolne
wolne \\\ [] = wolne
wolne \\\	((h,w):ys) = (usun h w wolne) \\\ ys
	where
		usun 1 w (xs:xss) = (xs \\ [w]) : xss
		usun h w (xs:xss) = xs : usun (h-1) w xss


rozwiaz :: Zadanie -> Generator Wynik

rozwiaz zadanie = szukaj wolne (paryPol zadanie)
	where
		wolne :: Wolne
		wolne = plansza (liczbaKolumn zadanie) (liczbaWierszy zadanie) \\\ concatMap pairToList (paryPol zadanie)
		pairToList (a,b) = [a,b]
		szukaj :: Wolne -> [(Pole,Pole)] -> Generator Wynik
		szukaj wolne [] = return []
		szukaj wolne (paraPkt:reszta) =
			do
				--xs <- sciezki paraPkt wolne
				(noweWolne,xs) <- sciezki paraPkt wolne
				--let noweWolne = (wolne \\\ xs)
				ys <- (szukaj noweWolne reszta)
				return $ [xs] ++ ys
		
		sciezki (a,b) wolne =
			if a==b then [(wolne,[a])] else
				do
					nast <- sasiedzi a
					guard (nast == b || nast `myElem` wolne)
					--xs <- sciezki (nast,b) (wolne \\\ [nast])
					(noweWolne,xs) <- sciezki (nast,b) (wolne \\\ [nast])
					--return (a:xs)
					return (noweWolne,(a:xs))
		sasiedzi (a,b) 	= [(a,b+1),(a,b-1),(a+1,b),(a-1,b)]
		naPlanszy (a,b) = a > 0 && a <= (liczbaKolumn zadanie) && b > 0 && b <= (liczbaWierszy zadanie)
		_ `myElem` [] 			= False
		(1,x) `myElem` (xs:xss) = x `elem` xs
		(y,x) `myElem` (_:xss)	= (y-1,x) `myElem` xss
		osiagalne [] _ = True
		osiagalne (para:reszta) wolne = (not $ null $ sciezki para wolne) && osiagalne reszta wolne

{-
zad = Zadanie 3 3 [((1,1),(3,3))]
wolne' = plansza (liczbaKolumn zad) (liczbaWierszy zad) \\ map fst (paryPol zad)

szukaj :: Wolne -> [(Pole,Pole)] -> Generator Plansza

szukaj wolne [] = return []
szukaj wolne (paraPkt:reszta) =
	do
		xs <- sciezki paraPkt wolne
		ys <- (szukaj (wolne \\ xs) reszta)
		return $ [xs] ++ ys

sciezki (a,b) wolne =
	if a==b then [[a]] else
		do
			nast <- sasiedzi a
			guard $ nast `elem` wolne
			xs <- sciezki (nast,b) (wolne \\ [nast])
			return (a:xs)

sasiedzi (a,b) 	= [(a,b+1),(a,b-1),(a+1,b),(a-1,b)]

-}