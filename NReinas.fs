namespace Busqueda

module NReinas =
    type estado = int []
    type accion = unit

    let estado_inicial (rnd:System.Random) n = 
        [|1 .. n|]
        |> Array.map (fun _ -> rnd.Next(n))

    let sucesores estado = 
        let st = [|0 .. Array.length estado - 1|]
        let moverReina i =
            st
            |> Array.map (fun k ->
                Array.mapi 
                    (fun j r -> if j <> i then r else k)
                    estado)
        st
        |> Array.collect moverReina
        |> Array.toList
        |> List.map (fun estado -> (), estado)

    let atacan ((x1,y1),(x2,y2)) =
        y1 = y2 ||
        x2-x1 = y2-y1 ||
        x2-x1 = y1-y2

    let h estado = 
        let reinas = 
            estado
            |> Array.mapi (fun i y -> (i, y))
        let pares = 
            Array.allPairs reinas reinas
            |> Array.filter (fun (r1,r2) -> r1 <> r2)
        Array.sumBy
            (fun par -> if atacan par then 1 else 0)
            pares / 2

    let meta estado = 
        h estado = 0

    let costo _ _ _ = 1.0
    
    let problema n =
        let rnd = System.Random()
        {
            inicial = estado_inicial rnd n
            sucesores = sucesores
            meta = meta
            costo = costo
        }
    

    let csp n =
        let variables = [0 .. n - 1]
        let dominio = [0 .. n - 1]
        let restricciones = 
            let pares = List.allPairs variables variables
                        |> List.filter (fun (v1,v2) -> v1 <> v2)
                        |> List.distinctBy (fun (v1,v2) -> if v1 <= v2
                                                           then (v1,v2)
                                                           else (v2,v1))
            List.map (fun (v1, v2) -> 
                    let f (estado:CSP.estado<int,int>) =
                        let dom1 = Map.find v1 estado
                        let dom2 = Map.find v2 estado
                        let l1 = List.length dom1
                        let l2 = List.length dom2
                        (l1 > 0 && l2 > 0) &&
                        (l1 > 1 || l2 > 1 ||
                         not (atacan ((v1,dom1.[0]), (v2,dom2.[0]))))
                    CSP.Binaria ((v1, v2), f)) pares
        {CSP.variables = variables
         CSP.dominios  = [for _ in 1 .. n do yield dominio]
         CSP.restricciones = restricciones}