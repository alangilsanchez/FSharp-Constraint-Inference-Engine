namespace Busqueda
//Representar una cola mediante una lista
// elementos se insertan al inicio de la lista

module Pila =
    let vacia = []
    let push pila n = n :: pila
    let pop = function
        | n :: pila -> Some(n, pila)
        | [] -> None
