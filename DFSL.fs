namespace Busqueda

module DFSL =
    open Pila
    let estrategia l =
        {
            insertar = 
                fun pila n ->
                    if n.profundidad <= l
                    then push pila n
                    else pila
            sacar = pop
            vacia = vacia
        }

    let key = fun n -> n.profundidad, n.estado