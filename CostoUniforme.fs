namespace Busqueda
// Cola  prioritaria implementada en Map<'key, 'value>
//Map esta implementado mediante un arbol roji-negro

module CostoUniforme =

    let key n = n.g, n.estado
    let estrategia =
        {
            insertar = fun map n -> Map.add (key n) n map
            sacar = fun map ->
                        match Map.tryPick(fun k n -> Some (k,n)) map with
                        | Some (k,n) -> Some (n,Map.remove k map)
                        | None -> None
            vacia = Map.empty
        }