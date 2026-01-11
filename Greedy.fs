namespace Busqueda
// Cola  prioritaria implementada en Map<'key, 'value>
//Map esta implementado mediante un arbol roji-negro

module Greedy =
    //funcion heuristica
    //f(n) = h n
    //Funcion de proyeccion
    let key h n = h n, n.estado
    let estrategia h =
        {
            sacar = fun map ->
                        match Map.tryPick(fun k n -> Some (k,n)) map with
                        | Some (k,n) -> Some (n,Map.remove k map)
                        | None -> None
            insertar = fun map n -> Map.add (h n, n.estado) n map
            vacia = Map.empty
        }