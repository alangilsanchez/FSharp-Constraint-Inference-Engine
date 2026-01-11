namespace Busqueda

module NQueens =
    type estado = int []  //Un arreglo donde cada índice representa una columna y el valor es la fila de la reina

    type accion = unit  //No se usa en búsqueda local

    let n = 4  // Tamaño del tablero

    //Estado inicial: Distribución aleatoria de las reinas
    let rnd = System.Random()
    let inicial: estado = Array.init n (fun _ -> rnd.Next(n))

    //Función heurística: cuenta los pares de reinas en conflicto
    let rec contar_conflictos (estado: estado) (i: int) (j: int) (conflictos: int) : int =
        if i >= n - 1 then conflictos
        elif j >= n then contar_conflictos estado (i + 1) (i + 2) conflictos
        else
            let nuevos_conflictos = 
                if estado.[i] = estado.[j] || abs (estado.[i] - estado.[j]) = abs (i - j) 
                then conflictos + 1 
                else conflictos
            contar_conflictos estado i (j + 1) nuevos_conflictos

    let heuristica (estado: estado) : int =
        contar_conflictos estado 0 1 0

    //Estado meta: Cuando no hay conflictos entre reinas
    let meta (estado: estado) : bool = heuristica estado = 0

    //Generar sucesores: Mover una reina en una columna a una fila diferente
    let rec generar_sucesores (estado: estado) (col: int) (fila: int) (vecinos: list<accion * estado>) : list<accion * estado> =
        if col >= n then vecinos
        elif fila >= n then generar_sucesores estado (col + 1) 0 vecinos
        elif fila <> estado.[col] then
            let nuevoEstado: estado = Array.copy estado
            nuevoEstado.[col] <- fila
            generar_sucesores estado col (fila + 1) (((), nuevoEstado) :: vecinos)
        else
            generar_sucesores estado col (fila + 1) vecinos

    let sucesores (estado: estado) : list<accion * estado> =
        generar_sucesores estado 0 0 []

    //No importa el costo en búsqueda local
    let costo (_: estado) (_: accion) (_: estado) : float = 1.0

    //Definir el problema para ser usado en la búsqueda local
    let problema = 
        {
            inicial = inicial
            sucesores = sucesores
            meta = meta
            costo = costo
        }
