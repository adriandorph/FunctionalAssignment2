open System

//2.1.1
let rec downto1 n : int list =
    if n = 0 then
        []
    else 
        n::downto1 (n - 1)


//2.1.2
let rec downto2 : int -> int list =
    function
    | 0 -> []
    | n -> n :: downto2 (n-1)


//2.2
let rec removeOddInd =
    function
    | [] -> []
    | x::y::xys -> x :: (removeOddInd xys)
    | x::xs -> x::(removeOddInd xs)


//2.3
let rec combinePairs =
     function
     | [] -> []
     | [x] -> []
     | x::y::xys -> ((x,y) :: combinePairs xys)


//2.4
type complex = float * float

let mkComplex x y :complex =
    (x,y)

let complexToPair complex =
    let a,b = complex
    (a,b)

let (|+|) (c1:complex) (c2:complex) =
    let a,b = c1
    let c,d = c2
    mkComplex 
        (a + c) 
        (b + d)

let (|*|) c1 c2 =
    let a,b = c1
    let c,d = c2
    mkComplex 
        (a * c - b * d)
        (b * c + a * d)

let (|-|) c1 c2 : complex =
    let a,b = c1
    let c,d = c2
    mkComplex
        (a - c)
        (b - d)    

let (|/|) c1 c2 : complex =
    let c,d = c2
    let c2Inverse = mkComplex (c / (c * c + d * d)) (-d / (c * c + d * d))
    c1 |*| c2Inverse

//2.5
let explode1 (str:string) =
    List.ofArray (str.ToCharArray())

let rec explode2 (str:string) =
    match str.Length with
    | 0 -> []
    | _ -> (str.Chars 0)::(explode2 (str.Remove(0,1)))

//2.6
let implode list =
    List.foldBack((fun a acc -> a.ToString() + acc)) list "" 

let implodeRev list =
    List.fold((fun acc a -> a.ToString() + acc)) "" list

//2.7
let toUpper (str:string) =
    List.foldBack((fun a acc ->  (Char.ToUpper a).ToString() + acc)) (explode2 str) "" 

//2.8
let rec ack (a,b) =
    match a,b with
    | (0,_) -> (b + 1)
    | (_, 0) -> (ack (a-1, 1))
    | (_,_) -> (ack (a-1,(ack (a , b-1))))

//2.9


[<EntryPoint>]
let main args =
    printfn "2.1 downto1 : Expected: %A Actual: %A" [3;2;1;] (downto1 3)
    printfn "2.1 downto2 : Expected: %A Actual: %A" [3;2;1;] (downto2 3)
    printfn "2.2 removeOddInd : Expected: %A Actual: %A" [0;2;4] (removeOddInd [0;1;2;3;4;])
    printfn "2.3 combinePairs : Expected %A Actual : %A" [(1,2);(3,4)] (combinePairs [1;2;3;4;5])
    printfn "2.4 mkComplex : Expected: %A Actual: %A" (1.0,2.0) (mkComplex 1.0 2.0)
    printfn "2.4 complexToPair : Expected: %A Actual: %A" (1.0,2.0) (complexToPair (1.0,2.0))
    printfn "2.5 explode1 : Expected: %A Actual: %A" ['F';'#';'I';'S';'N';'I';'C';'E'] (explode1 "F#ISNICE")
    printfn "2.5 explode1 : Expected: %A Actual: %A" ['F';'#';'I';'S';'N';'I';'C';'E'] (explode2 "F#ISNICE")
    printfn "2.6 implode : Expected: %A Actual: %A" "F#ISNICE" (implode ['F';'#';'I';'S';'N';'I';'C';'E'])
    printfn "2.6 implodeRev : Expected: %A Actual: %A" "ECINSI#F" (implodeRev ['F';'#';'I';'S';'N';'I';'C';'E'])
    printfn "2.7 toUpper : Expected: %A Actual: %A" "F#ISNICE" (toUpper "f#isnice")
    printfn "2.8 ack: Expected: %A Actual: %A" 57 (ack (2,27))
    0