namespace Busqueda
// Representar una cola mediante una lista
// elementos se insertan al final de la lista

//module Cola = 
 //   let vacia = []
   // let enqueue cola n = cola @ [n]
    //let dequeue = function
      //  | n :: cola -> Some (n, cola)
        //| [] -> None


//Cola para que se ejecuten en tiempo constante constante
// Representar una cola mediante dos lista
//(frente, trasera)

module Cola = 
    let vacia = [], []
    let enqueue (frente, trasera) n =
        (frente, n :: trasera) //Complejidad computacional constante
    let rec dequeue = function
        | (n :: frente, trasera) -> Some (n, (frente, trasera))
        | ([], n :: trasera) -> dequeue (List.rev (n :: trasera), []) //Operacion costosa
        | ([], []) -> None
