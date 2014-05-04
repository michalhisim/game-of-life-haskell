import qualified Data.Map as Map
import System.Random

data Stav = Zije | Mrtva
  deriving (Eq,Show)

type Pozice = (Int,Int)
type Bunka = (Pozice, Stav)
type Svet = [Bunka]

nahodnyStav :: IO Stav
nahodnyStav = do
  x <- randomIO
  return (if x then Zije else Mrtva)

-- Pravidla přežití
zitra :: Stav -> Int -> Stav
zitra Mrtva 3 = Zije
zitra Zije 2 = Zije
zitra Zije 3 = Zije
zitra _ _  = Mrtva

vytvorBunku :: Pozice -> IO Bunka
vytvorBunku pozice = do 
	stav <- nahodnyStav
	return (pozice, stav)

vytvorSvet :: Int -> IO Svet
vytvorSvet velikost = vytvorSloupecRadku [] (velikost,velikost)

vytvorRaduBunek :: Svet -> Pozice -> IO Svet
vytvorRaduBunek rada (1,y) = do
	bunka <- vytvorBunku (1,y)
	return (bunka:rada)
vytvorRaduBunek rada (x,y) = do
	bunka <- vytvorBunku (x,y)
	novaRada <- vytvorRaduBunek rada (x-1,y)
	return (bunka:novaRada)

vytvorSloupecRadku :: Svet -> Pozice -> IO Svet
vytvorSloupecRadku svet (x, 1) = vytvorRaduBunek [] (x, 1)
vytvorSloupecRadku svet (x, y) = do
	radek <- vytvorRaduBunek [] (x, y)
	novySloupec <- vytvorSloupecRadku [] (x, y - 1)
	return (radek++novySloupec)

--vytiskniSvet :: Svet -> Int -> IO
vytiskniSvet svet velikost = vytiskniSloupec svet (velikost,velikost)

vytiskniBunku :: Bool -> String
vytiskniBunku True = "@"
vytiskniBunku _ = " "

--vytiskniRadek :: Svet -> Pozice -> String
vytiskniRadek svet (1, y) = vytiskniBunku (zije (Map.lookup (1, y) svet))
vytiskniRadek svet (x, y) = (vytiskniRadek svet (x - 1, y)) ++ (vytiskniBunku (zije (Map.lookup (x, y) svet)))

--vytiskniSloupec Svet -> Pozice ->String
vytiskniSloupec svet (x, 1) = print (vytiskniRadek svet (x, 1))
vytiskniSloupec svet (x, y) = do
	print (vytiskniRadek svet (x, y))
	vytiskniSloupec svet (x, y - 1)

zijSvet svet velikost = zijSloupec svet velikost (velikost, velikost)

zijRadek svet velikost (1, y) = zivotBunky svet velikost (1, y)
zijRadek svet velikost (x, y) = zijRadek (zivotBunky svet velikost (x, y)) velikost (x - 1, y)

zijSloupec svet velikost (x, 1) = zijRadek svet velikost (x, 1)
zijSloupec svet velikost (x, y) = zijSloupec (zijRadek svet velikost (x, y)) velikost (x, y - 1)


zivotBunky svet velikost pozice = if (staryStav stav) /= novyStav then Map.insert pozice novyStav svet else svet where
	stav :: Maybe Stav
	stav = Map.lookup pozice svet
	
	staryStav :: Maybe Stav -> Stav
	staryStav (Just s) = s

	novyStav :: Stav
	novyStav = zitra (staryStav stav) aktualniSousedi

	aktualniSousedi :: Int
	aktualniSousedi = pocetSousedu svet pozice velikost

zije :: Maybe Stav -> Bool
zije (Just Zije) = True
zije _ = False


pocetSousedu svet (x, y) velikost = pocet sousede where
	pocet :: [Pozice] -> Int
	pocet [] = 0
	pocet ((rx, ry):ls) = if jeZivyNaSvete (rx, ry) (stavSouseda (rx, ry)) then 1 + pocet ls else pocet ls

	jeZivyNaSvete :: Pozice -> Maybe Stav -> Bool
	jeZivyNaSvete (rx, ry) stav = if not (jeNaSvete (rx, ry)) then False else
		if not (zije stav) then False else True

	jeNaSvete :: Pozice -> Bool
	jeNaSvete (rx, ry) = naSveteRel (x, y) (rx, ry) velikost

	stavSouseda :: Pozice -> Maybe Stav 
	stavSouseda (rx, ry) = Map.lookup (x + rx, y + ry) svet

sousede = [(-1,-1),(0,-1),(1,-1),
           (-1, 0),       (1, 0),
           (-1, 1),(0, 1),(1, 1)]

naSveteRel :: Pozice -> Pozice -> Int -> Bool
naSveteRel (x, y) (rx, ry) velikost = if not (naSvete (y + ry) velikost) then False else
	if not (naSvete (x + rx) velikost) then False else True

naSvete :: Int -> Int -> Bool	
naSvete x velikost = if x < 1 then False else if x > velikost then False else True

vytiskniLinku :: Int -> String
vytiskniLinku 0 = ""
vytiskniLinku x = "-" ++ (vytiskniLinku (x - 1))

den svet velikost = do
	vytiskniSvet svet velikost
	print (vytiskniLinku velikost)
	return (zijSvet svet velikost)

rutina svet _ 0 = do return svet
rutina svet velikost cas = do
	svet2 <- (den svet velikost)
	r <- (rutina svet2 velikost (cas - 1))
	return r

main :: IO ()
main = do
	let velikost = 15
	let cas = 20

	svet <- vytvorSvet velikost

	let mapa = Map.fromList svet

	zivot <- (rutina mapa velikost cas)

	print (vytiskniLinku velikost)
