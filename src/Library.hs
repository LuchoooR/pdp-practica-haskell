module Library where
import PdePreludat
import GHC.Num (Num)

doble :: Number -> Number
doble numero = numero + numero

--1
esMultiploDeTres :: Number -> Bool
esMultiploDeTres numero = mod numero 3 == 0

--2
esMultiploDe :: Number -> Number -> Bool
esMultiploDe numero divisor = mod divisor numero == 0

--3
cubo :: Number -> Number
cubo numero = numero * numero * numero

--4
area :: Number -> Number -> Number
area base altura = base * altura

--5
esBisiesto :: Number -> Bool
esBisiesto anio = esMultiploDe 400 anio || (esMultiploDe 4 anio && not (esMultiploDe 100 anio))

--6
celciusToFahr :: Number -> Number
celciusToFahr celcius = (celcius * 9 / 5) + 32

--7
fahrToCelsius :: Number -> Number
fahrToCelsius fahr = (fahr - 32) * 5 / 9

--8
haceFrioF :: Number -> String
haceFrioF tempF = if fahrToCelsius tempF < 8 then "Hace frío" else "No hace frío"

--9
mcm :: Number -> Number -> Number
mcm a b = (a * b) / (gcd a b)

--10 -a
dispersion :: Number -> Number -> Number -> Number
dispersion x y z = max x (max y z) - min x (min y z)

--b
diasParejos :: Number -> Number -> Number -> Bool
diasParejos x y z = dispersion x y z < 30

diasLocos :: Number -> Number -> Number -> Bool
diasLocos x y z = dispersion x y z > 100

diasNormales :: Number -> Number -> Number -> Bool
diasNormales x y z = not (diasParejos x y z) && not (diasLocos x y z)

--11 -a
-- pesoPino :: Number -> Number
-- pesoPino altura = if (altura / 100) <= 3 
--                     then 3 * altura 
--                     else 2 * altura

pesoPino :: Number -> Number
pesoPino altura 
    | (altura / 100) <= 3 = 3 * altura 
    | otherwise = 2 * altura


--b
esPesoUtil :: Number -> Bool
esPesoUtil peso = if (pesoPino peso) > 400 && (pesoPino peso) < 1000 then True else False

--c
sirvePino :: Number -> Bool
sirvePino = esPesoUtil.pesoPino

--12
--esCuadradoPerfecto :: Number -> Number 
--esCuadradoPerfecto a = for ()

-------------------------- GUIA 2 --------------------------

-- pablo :: Number -> Number -> Number
-- pablo x y = (x*y)

--1
siguiente :: Number -> Number
siguiente numero = numero + 1

--2
mitad :: Number -> Number
mitad numero = numero / 2

--3
inversa :: Number -> Number
inversa numero = 1 / numero

--4
triple :: Number -> Number
triple numero = numero * 3

--5
esNumeroPositivo :: Number -> Bool
esNumeroPositivo numero = numero > 0 

--6
esMultiploDe' :: Number -> Number -> Bool
esMultiploDe' numero = (== 0).(mod numero)

--7
--nose

--8
inversaRaizCuadrada :: Number -> Number
inversaRaizCuadrada = inversa.sqrt

--9
--incrementMCuadradoN :: Number -> Number -> Number
--incrementMCuadradoN 

--10
esResultadoPar :: Number -> Number -> Bool
esResultadoPar n m = (even. (^m)) n


-------------------------- GUIA 3 -----------------------


--1
--type Tupla = (a, b, c)
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

--2
aplicar :: (a -> b, a -> c) -> a -> (b, c)
aplicar (f, g) x = (f x, g x)

--3
cuentaBizarra :: (Number, Number) -> Number
cuentaBizarra (n1, n2) = if n1 > n2 
                        then n1 + n2
                        else if (n2 - n1) > 10 
                            then n2 - n1
                            else if n2 > n1 
                                then n2 * n1
                                else n1 

--4
esNotaBochazo :: Number -> Bool
esNotaBochazo nota1 = if nota1 >= 6 then False else True

aprobo ::  (Number, Number) -> String
aprobo (nota1, nota2) = if esNotaBochazo nota1 && esNotaBochazo nota2 then "Bochazo" else "Aprobado"

promociono :: (Number, Number) -> Bool
promociono (nota1, nota2) = if nota1 >= 7 && nota2 >= 7 && (nota1 + nota2) >= 15 
                                then True 
                                else False

consulta :: (Number, Number) -> String
consulta notas = if (not.esNotaBochazo.fst) notas then "Aprobo el 1er parcial" else "no aprobo el 1er parcial"

--5
--a
notasFinales :: ((Number, Number),(Number, Number)) -> (Number, Number)
notasFinales ((nota1, nota2), (nota3, nota4)) = if nota1 > nota3 && nota2 > nota4
                                            then (nota1, nota2)
                                            else if nota3 > nota1 && nota4 > nota2 
                                                then (nota3, nota4)
                                                else if nota1 > nota3 && nota4 > nota2 
                                                    then (nota1, nota4)
                                                    else (nota3, nota2)

--b    no se me ocurre composición aca la vdd
consulta' :: ((Number, Number),(Number, Number)) -> Bool
consulta' notas = if notasFinales notas < (6, 6) then True else False

--c
consulta'' :: ((Number, Number),(Number, Number)) -> Bool
consulta'' notas = if (fst . fst) notas < 6 then True else False

--d
recuperoDeGusto :: ((Number, Number),(Number, Number)) -> Bool
recuperoDeGusto notas = if (fst . fst) notas >= 8 && (snd . fst) notas >= 8 then True else False

--6
esMayorDeEdad :: (String, Number) -> Bool
esMayorDeEdad = (>21).snd

--7
calcular :: (Number, Number) -> (Number, Number)
calcular tupla = ((if (even.fst) tupla then fst tupla * 2 else fst tupla),(if (odd.snd) tupla then snd tupla + 1 else snd tupla))


-------------------------- GUIA 4 --------------------------

--1
sumarLista :: [Number] -> Number
sumarLista lista = sum lista

--2
frecuenciaCardiaca :: [Number]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

--a
promedioFrecuenciaCardiaca :: Number
promedioFrecuenciaCardiaca = sumarLista frecuenciaCardiaca / 7

--b
frecuenciaCardiacaMinuto :: Number -> Number
frecuenciaCardiacaMinuto m = frecuenciaCardiaca !! (m/10)

--c
frecuenciasHastaMomento :: Number -> [Number]
frecuenciasHastaMomento m = take (m/10 + 1) frecuenciaCardiaca


--3
esCapicua :: [[Char]] -> Bool
esCapicua lista = concat lista == reverse (concat lista)

--4
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

--a
cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos = if (sum.snd.snd) duracionLlamadas > (sum.snd.fst) duracionLlamadas then "horarioNormal" else "horarioReducido"


--b
cuandoHizoMasLlamadas :: String
cuandoHizoMasLlamadas = if (length.snd.snd) duracionLlamadas > (length.snd.fst) duracionLlamadas then "horarioNormal" else "horarioReducido"

---- Orden Superior ----

--1
existsAny :: (a -> Bool) -> (a, a, a) -> Bool
existsAny f (x, y, z) = f x || f y || f z

--2
mejor :: (Number -> Number) -> (Number -> Number) -> Number -> Number
mejor f g x = max (f x) (g x)

--3
aplicarPar :: (a -> b) -> (a, a) ->  (b, b)
aplicarPar f (x,y) = (f x, f y)


--4
parDeFns :: (a -> b) -> (a -> c) -> a -> (b, c)
parDeFns f g x = (f x, g x)


-- Orden Superior + Listas 

sumarN n lista = map (+n) lista 

--1
esMultiploDeAlguno :: Number -> [Number] -> Bool
esMultiploDeAlguno n lista = any ((==0) . (mod n )) lista

--2
promedios :: [[Number]] -> [Number]
promedios listas = map promedioAuxi listas

promedioAuxi :: [Number] -> Number
--promedioAuxi lista = sum lista / length lista
promedioAuxi lista = ( / length lista) . sum $ lista


--3
-- Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, 
-- excluyendo los que sean menores a 4 que no se cuentan. P.ej. 
-- Main> promediosSinAplazos [[8,6],[6,2,6]] 
-- [7,6] 
-- Nota: Implementar una solución utilizando map/2. 
promediosSinAplazos :: [[Number]] -> [Number]
promediosSinAplazos listas = map promedioAuxi (listasFiltrads listas)

promedioMayor4 :: [Number] -> [Number]
promedioMayor4 lista = filter (>4) lista

listasFiltrads :: [[Number]] -> [[Number]]
listasFiltrads listas = map promedioMayor4 listas


--4
-- Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno. P.ej. 
-- Main> mejoresNotas [[8,6,2,4],[7,9,4,5],[6,2,4,2],[9,6,7,10]] 
-- [8,9,6,10]. 
-- Ayuda: Utilizar la función predefinida maximum/1. 
mejoresNotas :: [[Number]] -> [Number]
mejoresNotas = map maximum


--5
-- Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. 
-- Se dice que un alumno aprobó si todas sus notas son 6 o más. P.ej. 
-- Main> aprobo [8,6,2,4] 
-- False 
-- Main> aprobo [7,9,6,8] 
-- True 
-- Ayuda: Utilizar la función predefinida minimum/1. 

aprobó :: [Number] -> Bool
aprobó lista 
    | minimum lista >= 6 = True
    | otherwise = False


--6
-- Definir la función aprobaron/1, que dada la información de un curso 
-- devuelve la información de los alumnos que aprobaron. P.ej. 
-- Main> aprobaron [[8,6,2,4],[7,9,6,7],[6,2,4,2],[9,6,7,10]] 
-- [[7,9,6,7],[9,6,7,10]] 
-- Ayuda: usar la función aprobó/1. 
aprobaron :: [[Number]] -> [[Number]]
aprobaron listas = filter aprobó listas

