namespace Busqueda

module MrWolfMrGoatMrCabbage =
    type estado = int * int * int * int * int //int 0 = lado izquierdo, 1 = lado derecho
                //L      Y     W      G    C

    type accion = 
        | CruzarYouSolo
        | CruzarMrWolfSolo
        | CruzarMrGoatSolo
        | CruzarMrCabbageSolo
        | CruzarYouConMrWolf
        | CruzarYouConMrGoat
        | CruzarYouConMrCabbage
        | CruzarMrWolfConMrGoat
        | CruzarMrWolfConMrCabbage
        | CruzarMrGoatConMrCabbage

    
    let inicial = (0, 0, 0, 0, 0)

    let meta estado =
        estado = (1, 1, 1, 1, 1)

    let costo _ accion _ =
        match accion with
        | CruzarYouSolo -> 1.0
        | CruzarMrWolfSolo -> 2.0
        | CruzarMrGoatSolo -> 5.0
        | CruzarMrCabbageSolo -> 10.0
        | CruzarYouConMrWolf -> 2.0
        | CruzarYouConMrGoat -> 5.0
        | CruzarYouConMrCabbage -> 10.0
        | CruzarMrWolfConMrGoat -> 5.0
        | CruzarMrWolfConMrCabbage -> 10.0
        | CruzarMrGoatConMrCabbage -> 10.0

    let sucesores estado =
        match estado with
        | (0, y, w, g, c) -> 
            [
                if y = 0 then CruzarYouSolo, (1, 1, w, g, c)
                if w = 0 then CruzarMrWolfSolo, (1, y, 1, g, c)
                if g = 0 then CruzarMrGoatSolo, (1, y, w, 1, c)
                if c = 0 then CruzarMrCabbageSolo, (1, y, w, g, 1)
                if y = 0 && w = 0 then CruzarYouConMrWolf, (1, 1, 1, g, c)
                if y = 0 && g = 0 then CruzarYouConMrGoat, (1, 1, w, 1, c)
                if y = 0 && c = 0 then CruzarYouConMrCabbage, (1, 1, w, g, 1)
                if w = 0 && g = 0 then CruzarMrWolfConMrGoat, (1, y, 1, 1, c)
                if w = 0 && g = 0 then CruzarMrWolfConMrCabbage, (1, y, 1, g, 1)
                if g = 0 && c = 0 then CruzarMrGoatConMrCabbage, (1, y, w, 1, 1)
            ]
            
        | (1, y, w, g, c) -> 
            [ 
                if y = 1 then CruzarYouSolo, (0, 0, w, g, c)
                if w = 1 then CruzarMrWolfSolo, (0, y, 0, g, c)
                if g = 1 then CruzarMrGoatSolo, (0, y, w, 0, c)
                if c = 1 then CruzarMrCabbageSolo, (0, y, w, g, 0)
                if y = 1 && w = 1 then CruzarYouConMrWolf, (0, 0, 0, g, c)
                if y = 1 && g = 1 then CruzarYouConMrGoat, (0, 0, w, 0, c)
                if y = 1 && c = 1 then CruzarYouConMrCabbage, (0, 0, w, g, 0)
                if w = 1 && g = 1 then CruzarMrWolfConMrGoat, (0, y, 0, 0, c)
                if w = 1 && g = 1 then CruzarMrWolfConMrCabbage, (0, y, 0, g, 0)
                if g = 1 && c = 1 then CruzarMrGoatConMrCabbage, (0, y, w, 0, 0)
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