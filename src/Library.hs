module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Postre = UnPostre {
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

bizcocho :: Postre
bizcocho = UnPostre ["borracho", "fruta", "crema"] 100 25
melaza :: Postre
melaza = UnPostre ["melaza"] 50 0

type Hechizo = Postre -> Postre

incendio :: Postre -> Postre
incendio = calentar 1 . perderPesoPorc 5

immobulus :: Postre -> Postre
immobulus postre = postre { temperatura = 0}

winLeviousa :: Postre -> Postre
winLeviousa = agregarSabor "concentrado". perderPesoPorc 10

diffindo :: Number -> Postre -> Postre
diffindo porcentajePerdido postre = perderPesoPorc porcentajePerdido postre

riddikulus :: String -> Postre -> Postre
riddikulus sabornuevo = agregarSabor (reverse sabornuevo)

avaKedavra :: Postre -> Postre
avaKedavra = immobulus . sacarSabores


perderPesoPorc :: Number -> Postre -> Postre
perderPesoPorc porcentajeperdido postre = postre { peso = peso postre * (1-(porcentajeperdido/100)) }

calentar :: Number -> Postre -> Postre
calentar valortemperatura postre = postre { temperatura = temperatura postre + valortemperatura }

--agregarSabor :: String -> Postre -> Postre
--agregarSabor saborextra postre = postre { sabores = [saborextra] ++ sabores postre}

agregarSabor :: String -> Postre -> Postre
agregarSabor saborextra postre = postre { sabores = saborextra : sabores postre}

sacarSabores :: Postre -> Postre
sacarSabores postre = postre { sabores = []}

estaCongelado :: Postre -> Bool
estaCongelado  = (<=0).temperatura

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor = (>0).length.sabores


-- C)
estaListo :: Hechizo -> Postre -> Bool
--estaListo postre = not (estaCongelado postre) && tieneAlgunSabor postre && (peso postre > 0)
estaListo hechizo postre = not (estaCongelado (hechizo postre)) && tieneAlgunSabor (hechizo postre) && (peso (hechizo postre) > 0)

estanListos :: Hechizo -> [Postre] -> Bool
estanListos = all.estaListo

-- D) Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos.

promedioPostresListos :: Hechizo -> [Postre] -> Number
promedioPostresListos hechizo postres = pesoPostresListos hechizo postres / length (conjuntoDePostresListos hechizo postres)

pesoPostresListos :: Hechizo -> [Postre] -> Number
pesoPostresListos hechizo postres =  sum (map peso $ conjuntoDePostresListos hechizo postres)

conjuntoDePostresListos :: Hechizo -> [Postre] -> [Postre]
conjuntoDePostresListos = filter.estaListo

--2 MAGOS

data Mago = UnMago {
        hechizosaprendidos :: [Hechizo] ,
        horrorcruxes :: Number
        } deriving (Show, Eq)

romina :: Mago
romina = UnMago {
    hechizosaprendidos = [incendio,immobulus],
    horrorcruxes = 29
    }
edu :: Mago
edu = UnMago {
    hechizosaprendidos = [avaKedavra,diffindo 10],
    horrorcruxes = 1
    }


-- Hacer que un mago asista a una clase y PRACTIQUE un hechizo sobre un POSTRE se espera obtener el MAGO 
-- y agregar el hechizo a su lista de hechizos aprendidos
-- Ademas si el resultado del hechizo es el mismo de aplicar avadakedabra, se suma un horrorcrux

practicarHechizo :: Hechizo -> Postre -> Mago -> Mago
practicarHechizo hechizo postre mago = UnMago {
    hechizosaprendidos = hechizo : hechizosaprendidos mago,
    horrorcruxes = nuevasHorrorcruxes hechizo postre + horrorcruxes mago
}

nuevasHorrorcruxes :: Hechizo -> Postre -> Number
nuevasHorrorcruxes hechizo postre
        | hechizo postre == avaKedavra postre = 1
        | otherwise                           = 0

--B Dado un postre y un mago obtener su mejor hechizo,
-- que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.
mejorHechizoMago :: [Hechizo] -> Postre -> Mago -> Hechizo
mejorHechizoMago [hechizo] postre mago = hechizo
mejorHechizoMago (hechizo1:hechizo2:hechizos) postre mago 
        | esMejor postre hechizo1 hechizo2 = mejorHechizoMago (hechizo1:hechizos) postre mago
        | esMejor postre hechizo2 hechizo1 = mejorHechizoMago (hechizo2:hechizos) postre mago
    
esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = length(sabores (hechizo1 postre)) > length(sabores (hechizo2 postre))




{-
practicar :: Mago -> Hechizo -> Postre -> Mago
practicar hechizo postre mago = (sumarHorrorcruxSegun hechizo postre) . (aprender hechizo mago)

aprender :: Hechizo -> Mago -> Mago
aprender hechizo mago = mago{hechizosaprendidos = (hechizo : hechizosaprendidos mago)}

sumarHorrorcruxSegun :: Hechizo -> Postre -> Mago-> Mago
sumarHorrorcruxSegun hechizo postre mago
        |esEquivalenteAvaKedavra hechizo postre = sumarHorrorcrux mago
        |otherwise = mago

sumarHorrorcrux :: Mago -> Mago
sumarHorrorcrux mago = mago { horrorcruxes = 1 + horrorcruxes mago}

esEquivalenteAvaKedavra2 :: Hechizo -> Postre -> Bool
esEquivalenteAvaKedavra2 hechizo postre = hechizo postre == avaKedavra postre

esEquivalenteAvaKedavra :: Hechizo -> Postre -> Bool
esEquivalenteAvaKedavra hechizo postre = sabores postre == [] && temperatura postre == 0

-- B dado un postre yun mago obtener su mejor hechizo , es es aquel de sus hechizos que deja el postre con mas cantidad de sabores luego de usarlo. 

mejorHechizo ::  Postre -> Mago -> Hechizo
mejorHechizo postre mago = elMejor postre (hechizosaprendidos mago)

elMejor :: Postre -> [Hechizo] -> Hechizo
elMejor postre [hechizo] = hechizo
elMejor postre (primer : segundo : restohechizos)
    | esMejor postre primer segundo = elMejor postre (primer : restohechizos)
    | otherwise = elMejor postre (segundo : restohechizos)

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = length (sabores (hechizo1 postre)) > length (sabores (hechizo2 postre))
-}