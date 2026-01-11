namespace Busqueda

module DFS =
    open Pila
    let estrategia =
        {
            insertar = push
            sacar = pop
            vacia = vacia
        }
    let key =  fun n -> n.estado