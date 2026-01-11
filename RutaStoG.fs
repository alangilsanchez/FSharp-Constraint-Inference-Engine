namespace Busqueda

module RutaStoG =

    type estado = int * int

    type accion =
        | Arriba
        | Abajo
        | Izquierda
        | Derecha


    let inicial = (6, 4)

    let meta estado =
        estado = (3, 3)

    let costo _ _ _ = 1.0

    let restriccion (x, y) =
        let restricciones =
            [ (4,3); (4,4); (4,5); (4,6); (4,7)
              (5,2); (6,3); (6,7) ]
        List.contains (x, y) restricciones

    let enRango (x, y) =
        x >= 1 && x < 9 && y >= 1 && y < 9

    let sucesores (x, y) =
        [
            if enRango (x - 1, y) && not (restriccion (x - 1, y)) then
                yield Arriba, (x - 1, y)
            if enRango (x + 1, y) && not (restriccion (x + 1, y)) then
                yield Abajo, (x + 1, y)
            if enRango (x, y - 1) && not (restriccion (x, y - 1)) then
                yield Izquierda, (x, y - 1)
            if enRango (x, y + 1) && not (restriccion (x, y + 1)) then
                yield Derecha, (x, y + 1)
        ]

    let problema  =
        {
            inicial = inicial
            costo = costo
            meta = meta
            sucesores = sucesores
        }


    let h1 n =
        let (x, y) = n.estado
        let (gx, gy) = (3, 3)
        abs (x - gx) + abs (y - gy)
        |> float