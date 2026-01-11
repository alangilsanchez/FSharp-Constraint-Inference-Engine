namespace Busqueda

module MundoAspiradora =
    type estado = bool * bool * int

    type accion = 
        | Left
        | Right
        | Suck
    
    let inicial = (true, true, 1)

    let meta estado =
        match estado with
        | (false, false, _) -> true
        | _ -> false

    
    let costo _ _ _ = 1.0

    
    let sucesores estado =
        match estado with
        | (true, b, 0) -> 
            [ 
                Right, (true, b, 1)
                Suck, (false, b, 0)
            ]
        | (false, b, 0) ->
            [ 
                Right, (false, b, 1)
            ]
        | (a, true, 1) ->
            [ 
                Left, (a, true, 0)
                Suck, (a, false, 1)
            ]
        | (a, false, 1) ->
            [ 
                Left, (a, false, 0)
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