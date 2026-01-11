namespace Busqueda
// Cola  prioritaria implementada en Map<'key, 'value>
//Map esta implementado mediante un arbol roji-negro

module AStar =
    //f(n) = n.g + h n
    //Funcion de proyeccion
    let key h n = n.g + h n,n.estado
    let estrategia h =
        {
            sacar = fun map ->
                        match Map.tryPick(fun k n -> Some (k,n)) map with
                        | Some (k,n) -> Some (n,Map.remove k map)
                        | None -> None
            insertar = fun map n -> Map.add (n.g + h n, n.estado) n map
            vacia = Map.empty
        }