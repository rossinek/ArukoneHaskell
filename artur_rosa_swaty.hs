import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment   

main :: IO ()
main = do
	args <- getArgs  
	let plik = head args
	s <- readFile plik
	let [liczbaWierszyKropka,liczbaKolumnKropka,paryPolKropka] = lines s
	let liczbaWierszy = read $ init liczbaWierszyKropka 	:: Int
	let liczbaKolumn = read $ init liczbaKolumnKropka 		:: Int
	let paryPol = read $ init paryPolKropka 				:: [(Pole,Pole)]
	let rozwiazania = rozwiaz $ Zadanie liczbaWierszy liczbaKolumn paryPol
	mapM_ print rozwiazania

type Generator a = [a]

type Pole = (Int,Int)
type Sciezka = [Pole]
type Wynik = [Sciezka]
type Wolne = Map Pole ()

data Zadanie = Zadanie {
        liczbaWierszy :: Int,
        liczbaKolumn :: Int,
        paryPol :: [(Pole,Pole)]
     }

plansza :: Int -> Int -> Wolne
plansza h w = Map.fromList [ ((y,x),()) | y <- [1..h], x <- [1..w] ]

rozwiaz :: Zadanie -> Generator Wynik
rozwiaz zadanie = szukaj wolne (paryPol zadanie)
	where
		wolne :: Wolne
		wolne = (plansza (liczbaWierszy zadanie) (liczbaKolumn zadanie)) Map.\\ (Map.fromList $ concatMap pairToList (paryPol zadanie))
		pairToList :: (Pole,Pole) -> [(Pole,())]
		pairToList (a,b) = [(a,()),(b,())]
		szukaj :: Wolne -> [(Pole,Pole)] -> Generator Wynik
		szukaj wolne [] = return []
		szukaj wolne (paraPkt:reszta) =
			do
				(noweWolne,xs) <- sciezki paraPkt wolne
				ys <- (szukaj noweWolne reszta)
				return (xs:ys)
		sciezki ::  (Pole,Pole) -> Wolne -> Generator (Wolne,Sciezka)
		sciezki (a,b) wolne =
			if a==b then [(wolne,[a])] else
				do
					nast <- sasiedzi a
					let (maybe, wolneBezNext) = Map.updateLookupWithKey (\_ _ -> Nothing) nast wolne
					guard (nast == b || maybe /= Nothing)
					(noweWolne,xs) <- sciezki (nast,b) wolneBezNext
					return (noweWolne,(a:xs))
		sasiedzi :: Pole -> Generator Pole
		sasiedzi (a,b) 	= [(a,b+1),(a,b-1),(a+1,b),(a-1,b)]
