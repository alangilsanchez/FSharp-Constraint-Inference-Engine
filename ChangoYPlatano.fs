namespace Busqueda

module ChangoYPlatano =
    open Busqueda

    type lugar = Puerta | Ventana | Centro

    type estado = lugar * lugar * bool * bool

    type acciones = Caminar of lugar | Empujar of lugar | Subir | Tomar

    let inicial = (Puerta, Ventana, false, false)

    let sucesores x = 
        let caminar = function | (_, b, false, h) -> 
                                [(Caminar Puerta, (Puerta, b, false, h))
                                 (Caminar Ventana, (Ventana, b, false, h))
                                 (Caminar Centro, (Centro, b, false, h))]
                               | _ -> []
        let empujar = function | (m, b, false, h) when m = b -> 
                                [(Empujar Puerta, (Puerta, Puerta, false, h))
                                 (Empujar Ventana, (Ventana, Ventana, false, h))
                                 (Empujar Centro, (Centro, Centro, false, h))]
                               | _ -> []
        let subir = function | (m, b, false, h) when m = b -> 
                                [(Subir, (m, b, true, h))]
                             | _ -> []
        let tomar = function | (Centro, b, true, false) -> 
                                [(Tomar, (Centro, b, true, true))]
                             | _ -> []
        caminar x @
        empujar x @
        subir x @
        tomar x

    let meta = function 
        | (_, _, _, true) -> true
        | _ -> false

    let costo _ _ _ = 1.0
    
    let problema inicial = 
              {
                     inicial = inicial
                     sucesores = sucesores
                     meta = meta
                     costo = costo
              }

    let prueba algoritmo inicio =
        algoritmo (problema inicio)
