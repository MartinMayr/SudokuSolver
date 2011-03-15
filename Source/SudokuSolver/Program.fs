open System.Collections.Generic

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let GetRows = id
let GetCols = transpose
let GetBoxes = id // Homework

let isAllUnique numbers = 
    let set = new HashSet<_>()
    numbers
    |> Seq.filter ((<>) 0)
    |> Seq.forall set.Add

let isMatrixValid matrix =
    let rows = GetRows matrix 
    let cols = GetCols matrix
    let boxes = GetBoxes matrix

    rows @ cols @ boxes
      |> Seq.forall isAllUnique

let listContainsZero = List.forall ((<>) 0)

let isComplete matrix =
    matrix
      |> Seq.exists listContainsZero

let rec fillFirstEmptyPositionWith matrix number =
    let m = ref number
    let replaceZero z =
        match z with
            | 0 ->
                let g = !m
                m := 0
                g
            | f -> id f
    let changeList = List.map replaceZero
    List.map changeList matrix

let fillFirstEmptyPosition (matrix : int list list) =
    let replaceFunction = fillFirstEmptyPositionWith matrix
    [ 1 .. 9 ]
        |> List.map replaceFunction

let rec solve matrix =
    if isComplete matrix then [matrix] else
    matrix
      |> fillFirstEmptyPosition
      |> List.filter isMatrixValid
      |> List.map solve
      |> List.concat
    

let testData =
    [[ 1;  2;  3;  4]
     [ 5;  6;  7;  8]
     [ 9; 10; 11; 12]
     [13; 14; 15; 16]]

let printData l =
    l
        |> Seq.iter (fun n -> printfn "%A" n)

let transform (l : int list list) =

    l

testData
    |> transform
    |> printData

System.Console.ReadKey() |> ignore