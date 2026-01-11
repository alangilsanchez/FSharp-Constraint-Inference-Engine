namespace Busqueda

module ZorroPatoSaco =
    type estado = int * int * int * int //int 0 = lado izquierdo, 1 = lado derecho
                //  B    c      G    w
    type accion = 
        | MoverZorro
        | MoverPato
        | MoverMaiz
        | Vacio
    
    let inicial = (0, 0, 0, 0)

    let meta estado =
        estado = (1, 1, 1, 1)

    
    let costo _ _ _ = 1.0

    let seguro (b, m, p, z) =
        not ((p = z && b <> p) || (p = m && b <> p)) 
    
    let sucesores estado =
        match estado with
        | (0, c, g, w) -> 
            [ 
                if c = 0 && seguro (1, 1, g, w) then MoverMaiz, (1, 1, g, w)
                if g = 0 && seguro (1, c, 1, w) then MoverPato, (1, c, 1, w) 
                if w = 0 && seguro (1, c, g, 1) then MoverZorro, (1, c, g, 1)
                if seguro (1, c, g, w) then Vacio, (1, c, g, w)
            ]
            
        | (1, c, g, w) -> 
            [ 
                if c = 1 && seguro (0, 0, g, w) then MoverMaiz, (0, 0, g, w)
                if g = 1 && seguro (0, c, 0, w)  then MoverPato, (0, c, 0, w) 
                if w = 1 && seguro (0, c, g, 0) then MoverZorro, (0, c, g, 0)
                if seguro (0, c, g, w) then Vacio, (0, c, g, w)
            ]
           
        | _ ->
            [

            ]  
       
    let problema inicial = 
            {
                inicial = inicial
                sucesores = sucesores
                meta = meta
                costo = costo
            }