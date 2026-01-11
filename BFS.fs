namespace Busqueda

module BFS =
    open Cola
    let estrategia =
        {
            insertar = enqueue
            sacar = dequeue
            vacia = vacia
        }

    let key n = n.estado