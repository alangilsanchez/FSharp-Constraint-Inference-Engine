namespace Busqueda

module OchoCasillas =
    type estado = int * int * int * int * int * int * int * int * int

    type accion = 
        | Left
        | Right
        | Up
        | Down
    
    let inicial = (7, 2, 4, 5, 0, 6, 8, 3, 1)

    let inicial_2 = (1, 2, 3, 4, 5, 6, 0,7, 8)
    let meta estado = 
        estado = (1, 2, 3, 4, 5, 6, 7, 8, 0)
    
    let costo _ _ _ = 1.0

    
    let sucesores estado =
        match estado with
        | (0, x1, x2,
           x3, x4, x5,
           x6, x7, x8) ->
            [ 
            Down,(x3, x1, x2,
                  0, x4, x5,
                  x6, x7, x8)
            Right,(x1, 0, x2,
                   x3, x4, x5,
                   x6, x7, x8)
            ]
        | (x1, 0, x2,
           x3, x4, x5,
           x6, x7, x8) ->
            [ 
            Left, (0, x1, x2,
                  x3, x4, x5,
                  x6, x7, x8)
            Right,(x1, x2, 0,
                   x3, x4, x5,
                   x6, x7, x8)
            Down, (x1, x4, x2,
                   x3, 0, x5,
                   x6, x7, x8) 
            ]
   
        | (x1, x2, 0,
           x3, x4, x5,
           x6, x7, x8) ->
            [ 
            Left, (x1, 0, x2,
                   x3, x4, x5,
                   x6, x7, x8)
            Down, (x1, x2, x5,
                   x3, x4, 0,
                   x6, x7, x8)
            ]
        
        | (x1, x2, x3,
           0, x4, x5,
           x6, x7, x8) ->
            [ 
            Up,(0, x2, x3,
                x1, x4, x5,
                x6, x7, x8)
            Down,(x1, x2, x3,
                  x6, x4, x5,
                   0, x7, x8)
            Right,(x1, x2, x3,
                  x4, 0, x5,
                  x6, x7, x8)
            ]
        
        | (x1, x2, x3,
           x4, 0, x5,
           x6, x7, x8) ->
            [ 
            Up, (x1, 0, x3,
                 x4, x2, x5,
                 x6, x7, x8)
            Down, (x1, x2, x3,
                   x4, x7, x5,
                   x6, 0, x8)
            Left, (x1, x2, x3,
                   0, x4, x5,
                   x6, x7, x8)
            Right, (x1, x2, x3,
                    x4, x5, 0,
                    x6, x7, x8)
            ]
        
        | (x1, x2, x3,
           x4, x5, 0,
           x6, x7, x8) ->
            [ 
            Up, (x1, x2, 0,
                 x4, x5, x3,
                 x6, x7, x8)
            Down, (x1, x2, x3,
                   x4, x5, x8,
                   x6, x7, 0)
            Left, (x1, x2, x3,
                   x4, 0, x5,
                   x6, x7, x8)
            ]
      
        | (x1, x2, x3,
           x4, x5, x6,
            0, x7, x8) ->
            [ 
            Up, (x1, x2, x3,
                 0, x5, x6,
                 x4, x7, x8)
            Right, (x1, x2, x3,
                    x4, x5, x6,
                    x7, 0, x8)
            ]
        
        | (x1, x2, x3,
           x4, x5, x6,
           x7, 0, x8) ->
            [ 
            Up, (x1, x2, x3,
                 x4, 0, x6,
                 x7, x5, x8)
            Left, (x1, x2, x3,
                   x4, x5, x6,
                    0, x7, x8)
            Right, (x1, x2, x3,
                    x4, x5, x6,
                    x7, x8, 0)
            ]
   
        | (x1, x2, x3,
           x4, x5, x6,
           x7, x8, 0) ->
            [ 
            Up, (x1, x2, x3,
                 x4, x5, 0,
                 x7, x8, x6)
            Left, (x1, x2, x3,
                   x4, x5, x6,
                   x7, 0, x8)
            ]
        | _ ->
            [ 
            
            ]

    let h1 n =
       let (x1,x2,x3,x4,x5,x6,x7,x8,_) = n.estado
       let cuenta x y = if x <> y then 1 else 0
       cuenta x1 1 +
       cuenta x2 2 +
       cuenta x3 3 +
       cuenta x4 4 +
       cuenta x5 5 +
       cuenta x6 6 +
       cuenta x7 7 +
       cuenta x8 8 
       |> float
    let problema inicial = 
              {
                     inicial = inicial
                     sucesores = sucesores
                     meta = meta
                     costo = costo
              }
