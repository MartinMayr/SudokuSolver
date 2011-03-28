open System.Collections.Generic

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let removeElements l count =
    let rec r = fun counter list ->
        if counter = count then
            list
        else
            list |> List.tail |> r (counter + 1)
    r 0 l

let rec buildTuples (l : int list list) =
    if l.Length = 0 then
        []
    else
        let f = List.zip3 (List.head l) (List.nth l 1) (List.nth l 2)
        List.append f (buildTuples (removeElements l 3))

let buildListFromTupel (a, b, c) =
    [a ; b; c]

let rec combineTuples (l : (int * int * int) list) =
    if l.Length = 0 then
        []
    else
        let f = List.head l
        let s = List.nth l 1
        let t = List.nth l 2
        (buildListFromTupel f @ buildListFromTupel s @ buildListFromTupel t) :: combineTuples(removeElements l 3)

let GetRows = id
let GetCols = transpose
let GetBoxes l = 
    l
    |> buildTuples
    |> combineTuples

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

let listContainsZero = List.exists ((=) 0)

let isComplete matrix =
    matrix
      |> Seq.exists listContainsZero
      |> not

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
    if isComplete matrix then [matrix]
    else
        matrix
          |> fillFirstEmptyPosition
          |> List.filter isMatrixValid
          |> List.map solve
          |> List.concat

let testData =
    [[0; 0; 8;  3; 0; 0;  6; 0; 0]
     [0; 0; 4;  0; 0; 0;  0; 1; 0]
     [6; 7; 0;  0; 8; 0;  0; 0; 0]

     [0; 1; 6;  4; 3; 0;  0; 0; 0]
     [0; 0; 0;  7; 9; 0;  0; 2; 0]
     [0; 9; 0;  0; 0; 0;  4; 0; 1]

     [0; 0; 0;  9; 1; 0;  0; 0; 5]
     [0; 0; 3;  0; 5; 0;  0; 0; 2]
     [0; 5; 0;  0; 0; 0;  0; 7; 4]]

let printCell i cell =
    printf "%A " cell
    if (i % 3) = 2 then printf " "

let printRow i row =
    Seq.iteri printCell row

    printfn ""
    if (i % 3) = 2 then printfn ""

let printGrid grid =
    grid
    |> Seq.iteri printRow
    printfn ""

let printData l =
    l
    |> Seq.iter printGrid

testData
    |> solve
    |> printData

System.Console.ReadKey() |> ignore