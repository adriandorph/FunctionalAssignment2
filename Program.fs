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
let rec removeOddIdx =
    function
    | [] -> []
    | x::y::xys -> x :: (removeOddIdx xys)
    | x::xs -> x::(removeOddIdx xs)


//2.3
let rec combinePair =
     function
     | [] -> []
     | [x] -> []
     | x::y::xys -> ((x,y) :: combinePair xys)


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
let time f =
  let start = System.DateTime.Now
  let res = f ()
  let finish = System.DateTime.Now
  (res, finish - start)

let timeArg1 f a =
    time (fun () -> f a)

//2.10
let rec downto3 f n e =
    match n with
    | n when n > 0 -> downto3 f (n - 1) (f n e)
    | _ -> e

let fac n =
    downto3 (fun x y -> x * y) n 1

let range (g:(int -> 'a)) (n:int) :'a list  =
    let f = fun x y -> y |> List.append [(g x)]
    (downto3 f n [])

//2.11
type word = (char * int) list

let hello : word = [('H',4);('E',1);('L',1);('L',1);('O',1)]

type squareFun = word -> int -> int -> int

//2.12
let letterPoint (word:word) pos =
    let _ , point = word.[pos]
    point

let singleLetterScore word pos points =
    (letterPoint word pos) + points

let doubleLetterScore word pos points  =
    (letterPoint word pos) * 2 + points

let tripleLetterScore word pos points =
    (letterPoint word pos) * 3 + points

//2.13
let doubleWordScore _ _ points =
    points * 2

let tripleWordScore _ _ points =
    points * 3

//2.14
let isVowel (character:char) = 
    "aeiouAEIOU".Contains(character)

let oddConsonants (word:word) _ points =
    let f = (fun x acc -> 
        let character, _ = word.Item (x-1)
        if isVowel character then
            acc
        else
            acc + 1
    )
    if (downto3 f word.Length 0) % 2 = 0 then
        points
    else
        -points

//2.15
type square = (int * squareFun) list

[<EntryPoint>]
let main args =
    printfn "2.1 downto1 : Expected: %A Actual: %A" [3;2;1;] (downto1 3)
    printfn "2.1 downto2 : Expected: %A Actual: %A" [3;2;1;] (downto2 3)
    printfn "2.2 removeOddIdx : Expected: %A Actual: %A" [0;2;4] (removeOddIdx [0;1;2;3;4;])
    printfn "2.3 combinePairs : Expected %A Actual : %A" [(1,2);(3,4)] (combinePair [1;2;3;4;5])
    printfn "2.4 mkComplex : Expected: %A Actual: %A" (1.0,2.0) (mkComplex 1.0 2.0)
    printfn "2.4 complexToPair : Expected: %A Actual: %A" (1.0,2.0) (complexToPair (1.0,2.0))
    printfn "2.5 explode1 : Expected: %A Actual: %A" ['F';'#';'I';'S';'N';'I';'C';'E'] (explode1 "F#ISNICE")
    printfn "2.5 explode2 : Expected: %A Actual: %A" ['F';'#';'I';'S';'N';'I';'C';'E'] (explode2 "F#ISNICE")
    printfn "2.6 implode : Expected: %A Actual: %A" "F#ISNICE" (implode ['F';'#';'I';'S';'N';'I';'C';'E'])
    printfn "2.6 implodeRev : Expected: %A Actual: %A" "ECINSI#F" (implodeRev ['F';'#';'I';'S';'N';'I';'C';'E'])
    printfn "2.7 toUpper : Expected: %A Actual: %A" "F#ISNICE" (toUpper "f#isnice")
    printfn "2.8 ack: Expected: %A Actual: %A" 57 (ack (2,27))
    printfn "2.9 time test with ack: %A" (time (fun () -> ack (3, 11)))
    printfn "2.9 timeArg1 test with ack: %A" (timeArg1 ack (3, 11))
    printfn "2.10 downto3: Expected: %A Actual: %A" 6 (downto3 (fun x y -> x + y) 3 0)
    printfn "2.10 fac: Expected: %A Actual: %A" 24 (fac 4)
    printfn "2.10 range: Expected: %A Actual %A" [1; 2; 6; 24] (range fac 4)
    printfn "2.11 hello: Expected: %A Actual %A" [('H',4);('E',1);('L',1);('L',1);('O',1)] hello
    printfn "2.12 singleLetterScore: Expected: %A Actual: %A" 1 (singleLetterScore hello 4 0)
    printfn "2.12 singleLetterScore: Expected: %A Actual: %A" 5 (singleLetterScore hello 4 4)
    printfn "2.12 doubleLetterScore: Expected: %A Actual: %A" 2 (doubleLetterScore hello 4 0)
    printfn "2.12 doubleLetterScore: Expected: %A Actual: %A" 5 (doubleLetterScore hello 4 3)
    printfn "2.12 tripleLetterScore: Expected: %A Actual: %A" 3 (tripleLetterScore hello 4 0)
    printfn "2.12 tripleLetterScore: Expected: %A Actual: %A" 5 (tripleLetterScore hello 4 2)
    printfn "2.12 doubleWordScore: Expected: %A Actual: %A" 4 (doubleWordScore hello 4 2)
    printfn "2.13 tripleWordScore: Expected: %A Actual: %A" 6 (tripleWordScore hello 4 2)
    printfn "2.14 oddConsonants: Expected: %A Actual: %A" -8 (oddConsonants hello 0 8)
    0