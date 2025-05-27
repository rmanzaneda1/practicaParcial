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

type Hechizo = Postre -> Postre

incendio :: Postre -> Postre
incendio postre = calentar 1.perderPesoPorc 5

immobulus :: Postre -> Postre
immobulus postre = postre { temperatura = 0}

winLeviousa :: Postre -> Postre
winLeviousa postre = agregarSabor "concentrado".perderPesoPorc 10

diffindo :: Number -> Postre -> Postre
diffindo porcentajePerdido postre = perderPesoPorc porcentajePerdido postre

riddikulus :: String -> Postre -> Postre
riddikulus sabornuevo = agregarSabor(reverse sabornuevo)

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
estaCongelado postre = (<0).temperatura postre

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor postre = (>0).length.sabores postre

estaListo :: Postre -> Bool
estaListo postre = not (estaCongelado postre) && tieneAlgunSabor postre && (peso postre > 0)

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo postres = all (estaListo.hechizo)  postres


-- {ConjuntoDePostresListos :: Hechizo -> [Postre] -> [Postre]
--ConjuntoDePostresListos hechizo postres = filter (estaListo.hechizo) postres


promedio :: [Number] -> Number
promedio nums = sum nums / length nums

pesoPromedioPostresListos :: [Postre] -> Number
pesoPromedioPostresListos postres =  promedio . map peso .filter estaListo postres

--2 MAGOS

data Mago = UnMago {
	hechizosaprendidos :: [Hechizo] ,
	horrorcruxes :: Number
	} deriving (Show, Eq)
	
-- A hacer que un mago asista a una clase y PRACTIQUE un echizo sobre un POSTRE se espera obtener el MAGO y agregar el hechizo a su lista de hechizos aprendidos
-- Ademas si el resultado del echizo es el mismo de aplicar avadakedabra, se suma un horrorcrux

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

{-
data Turista = UnTurista {
    nivelCansancio :: Number
    nivelStress :: Number
    viajaSolo :: Bool
    idiomas :: [String]
} deriving (Show, Eq)


--Auxiliares
bajarCansancio :: Number -> Turista -> Turista
bajarCansancio numero  turista = Unturista{
    nivelCansancio = nivelCansancio turista - numero
}

bajarStress :: Number -> Turista -> Turista
bajarStress numero  turista = Unturista{
    nivelStress = nivelStress turista - numero
}

aumentaCansancio :: Number -> Turista -> Turista
aumentaCansancio numero turista = UnTurista {
    nivelCansancio = nivelCansancio turista + numero
}

aumentaStress :: Number -> Turista -> Turista
aumentaStress numero  turista = Unturista{
    nivelStress = nivelStress turista + numero
}


--
type Excursion :: Turista -> Turista

irAlaPlaya :: Excursion
irAlaPlaya turista = 
    | viajaSolo turista = bajarCansancio 5 turista
    | otherwise         = bajarStress 1 turista

apreciarElementoPaisaje :: String -> Turista -> Turista
pareciarElementoPaisaje elemento turista = bajarStress (length elemento) turista

salirAhablarIdioma :: String -> Turista -> Turista
salirAhablarIdioma idioma turista = turista { 
    idiomas = idioma : idiomas turista 
    }

caminar :: Number -> Turista -> Turista
caminar minutos turista = (aumentaCansancio niveldeIntensidad) . (bajarStress niveldeIntensidad) turista

niveldeIntensidad :: Number -> Number
niveldeIntensidad minutos = 0.25*minutos

paseoEnBarco :: String -> Turista -> Turista
paseoEnBarco estadoMarea turista  
    | estadoMarea == "fuerte" = (aumentaStress 6 . aumentaCansancio 10) turista
    | estadoMarea == "moderada" = turista
    | estadoMarea == "tranquila" = (salirAhablarIdioma "aleman" . apreciaElementoPaisaje "mar" . caminar 10) turista 
    | otherwise = "estado no aceptado"

Ana :: Turista
Ana = UnTurista {
    nivelCansancio = 0,
    nivelStress = 21,
    viajaSolo = False,
    idiomas = [Español]
}
Beto :: Turista
Beto = UnTurista {
    nivelCansancio = 15,
    nivelStress = 15,
    viajaSolo = True,
    idiomas = [Aleman]
}
Cathi :: Turista
Cathi = UnTurista {
    nivelCansancio = 15,
    nivelStress = 15,
    viajaSolo = True,
    idiomas = [Aleman, Catalan]
}

--2.a hacer que un turista haga una excursion, ademas de sufrir los efectos de la excursion reduce en un 10% su estress

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = bajarStressPorc.excursion turista

--2.b 
deltaExcursionSegun :: Number -> Excursion -> Turista -> Number
deltaExcursionSegun ndice excursion turista = hacerExcursion.excursion turista



-- 3 Paquetes de excursiones llamados Tours

type Tour :: [Excursion] 

data Tour = UnTour {
    nombreTour ::String
    excursiones ::[String]
}
-- completo lado b isla vecina

completo :: Turista -> Turista
completo turista =  (salirAhablarIdioma "melmequiano") . (caminar 40) . (apreciarElementoPaisaje "cascada") . (caminar 20). (aumentaStress cantidadDeExcursionesTour) turista

cantidadDeExcursionesTour :: Tour -> Number
cantidadDeExcursionesTour tour =  length.excursiones tour
-}