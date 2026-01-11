//Alan Gilberto Sánchez Zavala
//Modulos a utilizar
open Busqueda
open Capitulo3
open CostoUniforme
open DFSL
open IDFSL
open DFS
open BFS
open Greedy
open AStar

//OCHO CASILLAS - RESUELTO CON BUSQUEDA GRAFO - AStar
(*open OchoCasillas
printfn "\nAStar:"
busqueda_grafo (problema inicial) (AStar.estrategia OchoCasillas.h1) (AStar.key OchoCasillas.h1)
|> Option.map (fun (nodo,tiempo) -> (nodo.g, acciones nodo,tiempo))
|> Option.map (fun (costoTotal, xs,tiempo) -> (List.length xs, costoTotal,tiempo, xs))
|> printfn "%A"*)

// MUNDO ASPIRADORA - RESUELTO CON BUSQUEDA GRAFO - BFS
(*open MundoAspiradora
printfn "\nBFS:"
busqueda_grafo (problema inicial) BFS.estrategia BFS.key
|> Option.map (fun (nodo, tiempo) -> (nodo.g, acciones nodo, tiempo))
|> Option.map (fun (costoTotal, xs, tiempo) -> (List.length xs, costoTotal, tiempo, xs))
|> printfn "%A"*)

//WOLF GOAT CABBAGE - RESUELTO CON BUSQUEDA GRAFO - BFS
(*
open ZorroPatoSaco
printfn "\nBFS:"
busqueda_grafo (problema inicial) BFS.estrategia BFS.key
|> Option.map (fun (nodo) -> (nodo.g, acciones nodo))
|> Option.map (fun (costoTotal, xs) -> (List.length xs, costoTotal,xs))
|> printfn "%A"*)


//MRWOLF MRGOAT MRCABBAGE - RESUELTO CON IDFSL
(*  
open MrWolfMrGoatMrCabbage
printfn "\nIDFSL:"
IDFSL (problema inicial)
|> Option.map (fun nodo -> acciones nodo)
|> Option.map (fun xs -> List.length xs, xs)
|> printfn "%A"
*)

//CHANGO Y PLATANO - RESUELTO CON BUSQUEDA ARBOL - DFS
(*  
open ChangoYPlatano
printfn "DFS:"
busqueda_arbol (problema inicial) DFS.estrategia
|> Option.map acciones
|> Option.map (fun xs -> List.length xs, xs)
|> printfn "%A"
*)

//RutaStoG
(*busqueda_grafo RutaStoG.problema (AStar.estrategia RutaStoG.h1) (AStar.key RutaStoG.h1)|> Option.map acciones
|> printfn "%A"*)



//NReinas
(*let resultado = 
    Capitulo3.reinicio_aleatorio
        ((fun x -> -x) << 
            NReinas.h << 
                (fun nodo -> nodo.estado))
        (fun () -> NReinas.problema 8)

printfn "resultado: %A" resultado.estado
printfn "h(resultado): %A" (NReinas.h resultado.estado)
*)



//NReinas con CSP
(*
let now = System.DateTime.UtcNow

match CSP.backtracking (NReinas.csp 12) with
| Some n -> printfn "Solución:  %A" n.estado
| None -> printfn "No encontre la solución"
printfn "Tardo; %A" (System.DateTime.UtcNow - now)

let solucion = CSP.minimosConflictosReinicio (NReinas.csp 24) 200
printfn "Solución: %A" solucion
printfn "Tardo; %A" (System.DateTime.UtcNow - now)
*)


//NReinas - Resuelto con reinicio aleatorio de ascension_colinas
//printfn "%A" (NReinas.sucesores [| 0;0;0 |])
//printfn "%A" (NReinas.h [| 2; 1; 2; 1|])

(*let resultado = 
    Capitulo3.reinicio_aleatorio
        ((fun x -> -x) << 
            NReinas.h << 
                (fun nodo -> nodo.estado))
        (fun () -> NReinas.problema 8)

printfn "resultado: %A" resultado.estado
printfn "h(resultado): %A" (NReinas.h resultado.estado)*)



//SUDOKU

let puzzle = 
        array2D [
            [0; 0; 8; 0; 4; 5; 6; 0; 0];
            [0; 0; 0; 3; 0; 0; 0; 0; 0];
            [0; 7; 5; 0; 0; 8; 9; 4; 0];
            [9; 0; 1; 0; 0; 6; 0; 3; 0];
            [4; 0; 0; 0; 9; 0; 0; 0; 1];
            [0; 8; 0; 1; 0; 0; 4; 0; 6];
            [0; 6; 9; 7; 0; 0; 3; 5; 0];
            [0; 0; 0; 0; 0; 4; 0; 0; 0];
            [0; 0; 2; 5; 3; 0; 7; 0; 0];
        ]

let now = System.DateTime.UtcNow

//let sol = CSP.minimosConflictosReinicio (Sudoku.csp puzzle) 5000
let sol = CSP.backtracking (Sudoku.csp puzzle)

match sol with
| Some (nodo) ->
    printfn "Solución encontrada:\n"

    // Creamos una matriz vacía de 9x9 con ceros
    let tablero = Array2D.create 9 9 0

    // Llenamos la matriz con las asignaciones del CSP
    nodo.estado
    |> Map.iter (fun (fila, col) valores ->
        match valores with
        | [v] -> tablero.[fila, col] <- v
        | _ -> () // Ignorar dominios con múltiples valores (no solución final)
    )

    // Imprimir el tablero
    for i in 0 .. 8 do
        for j in 0 .. 8 do
            let valor = tablero.[i, j]
            printf "%d " valor
        printfn "" // Nueva línea por fila

| None ->
    printfn "No se encontró solución."

printfn "Tarda: %A" (System.DateTime.UtcNow - now)


