module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес
let email = "semkozloff@gmail.com"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-8

// *** Первая часть

let factorial n =
    let rec factorial' i acc =
        match i with
        | 0 | 1 -> acc
        | _ -> factorial' (i - 1) (acc * i)
    factorial' n 1

let fTailor x : float = exp (2. * x) // функция, которую раскладываем
let tailorCoeff n = pown 2. n / float (factorial n)
let n, a, b = 20., 0.1, 0.6 // интервал

// сохранение и домножение коэффициента
let tailor x : Result = 
    let rec taylor' previousTerm (i : int) sum = 
        if (abs (sum - fTailor x) < delta) then (sum, i) else
            let nextTerm = 2. * x * previousTerm / float i
            taylor' nextTerm (i + 1) (sum + nextTerm)
    taylor' 1. 1 1.

// вычисление коэффициента напрямую
let tailorA x : Result = 
    let rec tailorA' (i : int) sum =
        if (abs (sum - fTailor x) < delta) then (sum, i) else
            tailorA' (i + 1) (sum + (tailorCoeff i) * pown x i) 
    tailorA' 0 0.

let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

printTailor ()

// *** Вторая часть

let iter f f' a b : Result = 
    let k = 1. / (f' a)
    let rec iter' (r : Result) : Result = 
        match r with 
        | x, i  when abs (f x) < delta -> r
        | x, i -> iter' (x - k * (f x), i + 1)   
    iter' (a, 0)   

let newton f f' a b : Result = 
    let rec newton' (r : Result) : Result =
        match r with
        | x, i when abs (f x) < delta -> r
        | x, i -> newton' (x - (f x) / (f' x), i + 1)
    newton' (a, 0)

let dichotomy =
    let rec dichotomyA i (f : float -> float) (f' : float -> float) (a : float) (b : float) : Result =
        let mid = (a + b) / 2.
        if abs (f mid) < delta then (mid, i) else
            if (f mid) * (f a) < 0. then dichotomyA (i + 1) f f' a mid else dichotomyA (i + 1) f f' mid b
    dichotomyA 0 
    
let printSolve () =
    let print f f' a b = 
        [iter; newton; dichotomy] 
        |> List.map (fun func -> func f f' a b) 
        |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)
    
    print (fun x -> 0.1 * (x ** 2.) - x * (log x)) 
          (fun x -> 0.2 * x - (log x) - 1.) 
          1. 2.
    print (fun x -> (tan x) - ((tan x) ** 3.) / 3. + ((tan x) ** 5.) / 5. - 1. / 3.)
          (fun x -> ((tan x) ** 4. - (tan x) ** 2. + 1.) / ((cos x) ** 2.))
          0. 0.8
    print (fun x -> (acos x) - sqrt (1. - 0.3 * x ** 3.))
          (fun x -> (0.45 * x ** 2.) / (sqrt (1. - 0.3 * x ** 3.)) - 1. / (sqrt (1. - x ** 2.)))
          0. 1.

printSolve ()

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.143.158:13666/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main ()