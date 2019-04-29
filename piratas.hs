-- --------------------------------------------------------------------------------------
--  TP Funcional Piratas
--  https://docs.google.com/document/d/1g5INP01q205eudW-EUZh4huid27yRwBdKDcAoYtUIeE
--  Rodrigo Comesaña
-- --------------------------------------------------------------------------------------

-- --------------------------------------------------------------------------------------
--  Tesoros piratas
-- --------------------------------------------------------------------------------------

import Text.Show.Functions

jackSparrow = ("Jack Sparrow", [("Brujula", 10000), ("Frasco de Arena", 0)])
davidJones = ("David Jones", [("Cajita Musical", 1)])
anneBonny = ("Anne Bonny", [("Doblones", 100), ("Frasco de Arena", 1)])
elizabethSwann = ("Elizabeth Swann", [("Moneda del cofre muerto", 100), ("Espada de Hierro", 50)])
willTurner = ("Will Turner", [("Cuchillo", 5), ("Sombrero", 250)])

nombre (_nombre, _) = _nombre
tesoros (_, _tesoros) = _tesoros
nombreTesoro (_nombre, _) = _nombre
precioTesoro (_, _precio) = _precio

cantidadTesoros _pirata = length (tesoros _pirata)

esAfortunado _pirata = (sum (map precioTesoro (tesoros _pirata))) > 10000

mismoTesoroDistintoPrecio _tesoroA _tesoroB = True
    && (nombreTesoro _tesoroA == nombreTesoro _tesoroB)
    && (precioTesoro _tesoroA /= precioTesoro _tesoroB)

pirataTieneTesoroDistintoPrecio _pirata _tesoro = 
    any (mismoTesoroDistintoPrecio _tesoro) (tesoros _pirata)

tieneMismoTesoroDistintoPrecio _pirataA _pirataB = 
    any (pirataTieneTesoroDistintoPrecio _pirataA) (tesoros _pirataB)

precioTesoroMasValioso _pirata =
    maximum(map precioTesoro (tesoros _pirata))

agregarTesoro _tesoro _pirata = (nombre _pirata, _tesoro : (tesoros _pirata))

esValioso _tesoro = (precioTesoro _tesoro) > 100

porNombre _nombre _tesoro = _nombre == (nombreTesoro _tesoro)

-- Ejemplos:
--  * perder tesoros valiosos:
--      > perderTesoros esValioso jackSparrow
--
--  * perder tesoros por nombre:
--      > perderTesoros (porNombre "Frasco de Arena") jackSparrow

perderTesoros _criterio _pirata =
    (nombre _pirata, filter (not._criterio) (tesoros _pirata))

-- --------------------------------------------------------------------------------------
--  Temporada de Saqueos
-- --------------------------------------------------------------------------------------

-- sólo saquea tesoros que cumplen esValioso
saqueoValioso _tesoro = esValioso _tesoro

-- sólo saquea tesoros con nombre especifico
saqueoEspecifico _nombreTesoro _tesoro = _nombreTesoro == (nombreTesoro _tesoro)

-- no saquea nada
saqueoConCorazon _tesoro = False

-- sólo saquea tesoros que cumplen 1 y 2
saqueoComplejo _nombreTesoro _tesoro = True
    && saqueoValioso _tesoro
    && saqueoEspecifico _nombreTesoro _tesoro

saquear _tipoSaqueo (_nombre, _tesoros) _tesoro =
    (_nombre, _tesoros ++ filter (_tipoSaqueo) [_tesoro]) 

-- --------------------------------------------------------------------------------------
--  Navegando los siete mares
-- --------------------------------------------------------------------------------------

perlaNegra = ("Perla Negra", [jackSparrow, anneBonny], saqueoComplejo "Sombrero")
holandesErrante = ("Holandes Errante", [davidJones], saqueoEspecifico "Oro")
interceptor = ("Interceptor", [elizabethSwann, willTurner], saqueoConCorazon)

islaTortuga = ("Frasco de Arena", 1)
islaDelRon = ("Botella de Ron", 25)
islaCualk = ("Frasco de Arena", 1)
ciudadX = [("Sombrero", 15), ("Sombrero", 250), ("Sombrero", 500)]

nombreBarco (_nombreBarco, _, _) = _nombreBarco
tripulacion (_, _tripulacionBarco, _) = _tripulacionBarco
tipoSaqueo (_, _, _tipoSaqueo) = _tipoSaqueo
numTripulantes _barco = length (tripulacion _barco)

subePirata _barco _pirata = 
    (nombreBarco _barco, tripulacion _barco ++ [_pirata], tipoSaqueo _barco)

bajaPirata _barco _pirata =
    (nombreBarco _barco, filter (not.(nombre _pirata==).nombre) (tripulacion _barco), tipoSaqueo _barco)

--  a)
anclarEnIslaDeshabitada _barco _isla =
    (nombreBarco _barco, map (agregarTesoro _isla) (tripulacion _barco), tipoSaqueo _barco)

--  b)
atacarCiudad _barco _ciudad =
    (nombreBarco _barco, zipWith (saquear (tipoSaqueo _barco)) (tripulacion _barco) _ciudad, tipoSaqueo _barco)

--  c) implementación simple, reutilizando atacarCiudad que respeta las preferencias de saqueo de c/u
abordarBarco _barcoA _barcoB = 
    atacarCiudad _barcoA (concat (map tesoros (tripulacion _barcoB)))
