namespace Busqueda

module Sudoku =
    type estado = int [,] //Bidimensional del mismo tamaño, 9X9
    type accion = int * int * int // fila, columna, valor

    let inicial (puzzle: int [,]) =
        Array2D.copy puzzle //Hace una copia del estado inicial, ya que las matrices son mutables

    let sucesores estado =
        let n = Array2D.length1 estado // Número de filas
        let m = Array2D.length2 estado // Número de columnas
        [
            for i in 0 .. n-1 do
                for j in 0 .. m-1 do
                    if estado.[i,j] = 0 then //Si la celda esta vacia
                        for v in 1 .. 9 do //Por cada celda vacia, coloco todos los valores posibles.
                            let nuevo = Array2D.copy estado
                            nuevo.[i,j] <- v //asigna el v en la posición (i,j) del nuevo tablero
                            yield ((i,j,v), nuevo) //accion , estado nuevo
        ]
    
    let celda_valida (estado:int[,]) fila col valor =

        let rec revisar_fila j =
            if j > 8 then true //Reviso todas la celdas de la fila
            else if estado.[fila, j] = valor then false //La fila ya tiene ese valor por lo tanto no es valido
            else revisar_fila (j + 1) //Revisa la siguiente celda en la fila

        let rec revisar_col i =
            if i > 8 then true //Reviso todas la celdas de la columna
            else if estado.[i, col] = valor then false //La columna ya tiene ese valor por lo tanto no es valido
            else revisar_col (i + 1) //Revisa la siguiente celda en la columna

        let rec revisar_caja i j =
            if i > 2 then true //Reviso todas las celdas de la caja
            else if j > 2 then revisar_caja (i + 1) 0 //Revisa la siguiente columna en la caja
            else if estado.[(fila / 3) * 3 + i, (col / 3) * 3 + j] = valor then false ////La caja ya tiene ese valor por lo tanto no es valido
            else revisar_caja i (j + 1) //Revisa la siguiente celda en la fila actual de la caja

        revisar_fila 0 && revisar_col 0 && revisar_caja 0 0 //Revisa que no este repetido el valor
    

    let h (estado:int[,]) =
        [| for i in 0 .. 8 do
            for j in 0 .. 8 do
                if estado.[i,j] = 0 then yield 1 |]
        |> Array.sum //Suma el numero de celdas vacias
        
    
    let meta (estado:int[,]) =

        let rec revisar_fila i j usados =
            if j > 8 then List.sort usados = [1..9] //Si ya revisé toda la fila, verifico que contenga los números del 1 al 9
            else revisar_fila i (j + 1) (estado.[i,j] :: usados) //Agrego el valor actual a la lista de usados y sigo revisando la fila

        let rec revisar_col j i usados =
            if i > 8 then List.sort usados = [1..9] //Si ya revisé toda la columna, verifico que contenga los números del 1 al 9
            else revisar_col j (i + 1) (estado.[i,j] :: usados) //Agrego el valor actual a la lista de usados y sigo revisando la columna

        let rec revisar_caja bi bj i j usados =
            if i > 2 then List.sort usados = [1..9] //Si revisé toda la caja, verifico que contenga los números del 1 al 9
            else if j > 2 then revisar_caja bi bj (i + 1) 0 usados //Reviso la siguiente fila dentro de la caja
            else revisar_caja bi bj i (j + 1) (estado.[bi*3 + i, bj*3 + j] :: usados) //Agrego el valor actual a la lista de usados y reviso la siguiente celda en la fila actual de la caja

        let rec revisar_todas_filas i =
            if i > 8 then true //Si revisé todas las filas, entonces todas son válidas
            else if not (revisar_fila i 0 []) then false //Si alguna fila no es válida, devuelvo false
            else revisar_todas_filas (i + 1) //Reviso la siguiente fila

        let rec revisar_todas_cols j =
            if j > 8 then true //Si revisé todas las columnas, entonces todas son válidas
            else if not (revisar_col j 0 []) then false //Si alguna columna no es válida, devuelvo false
            else revisar_todas_cols (j + 1) //Reviso la siguiente columna

        let rec revisar_todas_cajas bi bj =
            if bi > 2 then true //Si revisé todas las cajas, entonces todas son válidas
            else if bj > 2 then revisar_todas_cajas (bi + 1) 0 //Reviso la siguiente fila de cajas
            else if not (revisar_caja bi bj 0 0 []) then false //Si alguna caja no es válida, devuelvo false
            else revisar_todas_cajas bi (bj + 1) //Reviso la siguiente caja en la fila actual

        h estado = 0 && revisar_todas_filas 0 && revisar_todas_cols 0 && revisar_todas_cajas 0 0 //Verifico que no haya celdas vacías y que todas las filas, columnas y cajas sean válidas


    let costo _ _ _ = 1.0

    let problema puzzle =
        {
            inicial = inicial puzzle
            sucesores = sucesores
            meta = meta
            costo = costo
        }

    let csp (puzzle: int [,]) =
        // Todas las celdas son variables
        let variables =
            [for i in 0 .. 8 do
                for j in 0 .. 8 do
                    yield (i, j)]

        // Dominio según si la celda tiene un valor asignado
        let dominios =
            [for i in 0 .. 8 do
                for j in 0 .. 8 do
                    if puzzle.[i,j] = 0 then yield [1..9] //En caso de no tener asignado un valor, su dominio es 1 .. 9
                    else yield [puzzle.[i,j]]]

        // Restricciones binarias para cada par distinto de variables
        let pares =
            List.allPairs variables variables //Creo todas las combinaciones posibles de pares de variables
            |> List.filter (fun ((i1,j1),(i2,j2)) -> (i1,j1) <> (i2,j2)) //eliminan los pares donde una celda se compara consigo misma
            |> List.distinctBy (fun ((i1,j1),(i2,j2)) ->
                if (i1,j1) <= (i2,j2) then ((i1,j1),(i2,j2)) else ((i2,j2),(i1,j1))) //Elimina duplicados

        let restricciones =
            List.map (fun ((i1,j1),(i2,j2)) ->
                let mismaFila = i1 = i2 //Determino si las variables están en la misma fila
                let mismaCol  = j1 = j2 //Determino si las variables están en la misma columna
                let mismaCaja = (i1 / 3 = i2 / 3) && (j1 / 3 = j2 / 3) //Determino si las variables están en la misma caja

                //Función de restricción binaria
                let f (estado: CSP.estado<(int*int),int>) =
                    let dom1 = Map.find (i1,j1) estado //Obtengo el dominio actual de la primera variable
                    let dom2 = Map.find (i2,j2) estado //Obtengo el dominio actual de la segunda variable
                    let l1 = List.length dom1 //Tamaño del dominio de la primera variable
                    let l2 = List.length dom2 //Tamaño del dominio de la segunda variable

                    //Restricciones
                    //Ambas variables deben tener un valor asignado del dominio
                    (l1 > 0 && l2 > 0) &&
                    (l1 > 1 || l2 > 1 ||
                    dom1.[0] <> dom2.[0] || //Si ambas tienen un solo valor, deben ser diferentes y no estar en la misma fila,columna y caja
                     not (mismaFila || mismaCol || mismaCaja))

                CSP.Binaria (((i1,j1),(i2,j2)), f)) pares //Restricción binaria para el par de variables

        {
            CSP.variables = variables
            CSP.dominios = dominios
            CSP.restricciones = restricciones
        }

