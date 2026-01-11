namespace Busqueda

module IDFSL =
    let IDFSL problema =
        let rec loop depth =
            match 
                Capitulo3.busqueda_grafo
                    problema
                    (DFSL.estrategia depth) 
                    DFSL.key
                with
            | None -> loop (depth + 1)
            | Some n -> Some n
        loop 0