-- --------------------------------------------------------------------------------------
--  TP Funcional Piratas
--  https://docs.google.com/document/d/1g5INP01q205eudW-EUZh4huid27yRwBdKDcAoYtUIeE
--  Rodrigo Comesaña
-- --------------------------------------------------------------------------------------

-- --------------------------------------------------------------------------------------
--  Tesoros piratas
-- --------------------------------------------------------------------------------------

jackSparrow = ("Jack Sparrow", [("Brujula", 10000), ("Frasco de Arena", 0)], saqueoComplejo)
davidJones = ("David Jones", [("Cajita Musical", 1)], saqueoConCorazon)
anneBonny = ("Anne Bonny", [("Doblones", 100), ("Frasco de Arena", 1)], saqueoValioso)
elizabethSwann = ("Elizabeth Swann", [("Moneda del cofre muerto", 100), ("Espada de Hierro", 50)], saqueoEspecifico)
willTurner = ("Will Turner", [("Cuchillo", 5), ("Sombrero", 250)], saqueoValioso)

nombre (_nombre, _, _) = _nombre
tesoros (_, _tesoros, _) = _tesoros
tipoSaqueo (_, _, _tipoSaqueo) = _tipoSaqueo
nombreTesoro (_nombre, _) = _nombre
precioTesoro (_, _precio) = _precio

cantidadTesoros _pirata = length (tesoros _pirata)

esAfortunado _pirata = (sum (map precioTesoro (tesoros _pirata))) > 10000

_mismoTesoroDistintoPrecio _tesoroA _tesoroB = True
    && (nombreTesoro _tesoroA == nombreTesoro _tesoroB)
    && (precioTesoro _tesoroA /= precioTesoro _tesoroB)

_pirataTieneTesoroDistintoPrecio _pirata _tesoro = 
    any (_mismoTesoroDistintoPrecio _tesoro) (tesoros _pirata)

tieneMismoTesoroDistintoPrecio _pirataA _pirataB = 
    any (_pirataTieneTesoroDistintoPrecio _pirataA) (tesoros _pirataB)

precioTesoroMasValioso _pirata =
    maximum(map precioTesoro (tesoros _pirata))

agregarTesoro _tesoro _pirata = (nombre _pirata, (tesoros _pirata) ++ [_tesoro], tipoSaqueo _pirata)

esValioso _tesoro = (precioTesoro _tesoro) > 100

perderTesorosValiosos _pirata = 
    (nombre _pirata, filter (not.esValioso) (tesoros _pirata), tipoSaqueo _pirata)

perderTesorosPorNombre _pirata _nombreTesoro = 
    (nombre _pirata, filter (not.(==_nombreTesoro).nombreTesoro) (tesoros _pirata), tipoSaqueo _pirata)

-- --------------------------------------------------------------------------------------
--  Temporada de Saqueos
-- --------------------------------------------------------------------------------------

saqueoValioso =    1    -- sólo saquea tesoros que cumplen esValioso
saqueoEspecifico = 2    -- sólo saquea tesoros con nombre "Sombrero"
saqueoConCorazon = 3    -- no saquea nada
saqueoComplejo =   4    -- sólo saquea tesoros que cumplen 1 y 2

saquear (_nombre, _tesoros, 1) _tesoro =
    (_nombre, _tesoros ++ (filter esValioso [_tesoro]), 1)

saquear (_nombre, _tesoros, 2) _tesoro =
    (_nombre, _tesoros ++ (filter (("Sombrero"==).nombreTesoro) [_tesoro]), 2)

saquear (_nombre, _tesoros, 3) _tesoro =
    (_nombre, _tesoros, 3)

saquear (_nombre, _tesoros, 4) _tesoro =
    (_nombre, _tesoros ++ (filter (("Sombrero"==).nombreTesoro) (filter esValioso [_tesoro])), 2)

-- --------------------------------------------------------------------------------------
--  Navegando los siete mares
-- --------------------------------------------------------------------------------------

perlaNegra = ("Perla Negra", [jackSparrow, anneBonny])
holandesErrante = ("Holandes Errante", [davidJones])
interceptor = ("Interceptor", [elizabethSwann, willTurner])

islaTortuga = [("Frasco de Arena", 1)]
islaDelRon = [("Botella de Ron", 25)]
islaCualk = [("Frasco de Arena", 1), ("Botella de Ron", 25)]
ciudadX = [("Sombrero", 15), ("Sombrero", 250), ("Sombrero", 500)]

nombreBarco (_nombreBarco, _) = _nombreBarco
tripulacion (_, _tripulacionBarco) = _tripulacionBarco
numTripulantes _barco = length (tripulacion _barco)

subePirata _barco _pirata = 
    (nombreBarco _barco, tripulacion _barco ++ [_pirata])

bajaPirata _barco _pirata =
    (nombreBarco _barco, filter (not.(nombre _pirata==).nombre) (tripulacion _barco))

--  a)
anclarEnIslaDeshabitada _barco _isla =
    (nombreBarco _barco, map (agregarTesoro (head _isla)) (tripulacion _barco))

--  b)
atacarCiudad (_nombreBarco, _tripulacion) _ciudad =
    (_nombreBarco, zipWith saquear _tripulacion _ciudad)

--  c) implementación simple, reutilizando atacarCiudad que respeta las preferencias de saqueo de c/u
abordarBarcoConPreferencias _barcoA _barcoB = 
    atacarCiudad _barcoA (concat (map tesoros (tripulacion _barcoB)))

--  c) implementación alternativa, choreo uno a uno. cada tripulante de A, le roba a 
--     un respectivo tripulante de B (con su mismo subindice). 
--     los tripulantes de A restantes que no pudieron robar mantienen sus mismas
--     pertenencias (en caso de que tripulacion A>B).
--     los tripulantes restantes de B se ignoran (en caso que tripulacion A<B).
takeLast _n _list = reverse (take _n (reverse _list))

robarTesoro _tripulacionA _tripulacionB _indice =
    (
        nombre (_tripulacionA !! _indice),
        (tesoros (_tripulacionA !! _indice)) ++ (tesoros (_tripulacionB !! _indice)),
        tipoSaqueo (_tripulacionA !! _indice)
    )

abordarBarco _barcoA _barcoB =
    (
        nombreBarco _barcoA, 
        (
            map 
            (robarTesoro (tripulacion _barcoA) (tripulacion _barcoB))
            [0..(min (numTripulantes _barcoA) (numTripulantes _barcoB))-1]
        ) 
        ++
        (
            takeLast ((numTripulantes _barcoA) - (min (numTripulantes _barcoA) (numTripulantes _barcoB))) (tripulacion _barcoA)
        )
    )
