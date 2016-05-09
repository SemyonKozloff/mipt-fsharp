module lab2

open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

let email = "semkozloff@gmail.com"

let explode (s:string) =
  [for c in s -> c]

type Token =
  | OpenBrace | CloseBrace
  | OpenBracket | CloseBracket
  | Colon | Comma
  | String of string
  | Number of int
  | Boolean of bool
  | Null

let tokenize source =
  let rec parseString acc = function
    | '\\' :: '"' :: t -> parseString (acc + "\"") t
    | '\\' :: 'n' :: t -> parseString (acc + "\n") t
    | '"' :: t -> acc, t
    | c :: t -> parseString (acc + c.ToString()) t
    | _ -> failwith "Malformed string."
 
  let rec token acc = function
    | (x :: _) as t when List.exists ((=)x) [')'; ':'; ','; ']'] -> acc, t
    | w :: t when Char.IsWhiteSpace(w) -> acc, t
    | [] -> acc, [] // end of list terminates
    | c :: t -> token (acc + (c.ToString())) t

  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t
    | '{' :: t -> tokenize' (OpenBrace :: acc) t
    | '}' :: t -> tokenize' (CloseBrace :: acc) t
    | '[' :: t -> tokenize' (OpenBracket :: acc) t
    | ']' :: t -> tokenize' (CloseBracket :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '"' :: t -> // start of string
      let s, t' = parseString "" t
      tokenize' (String s :: acc) t'    
    | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
    | d :: t -> // остались числа
      let n, t' = token (d.ToString()) t
      tokenize' (Number (try Convert.ToInt32 n with e -> 0)  :: acc) t'
    | [] -> List.rev acc
    | _ -> failwith "Tokinzation error"
  tokenize' [] source

// Тип данных для представления JSON-объектов.
type JSON =
  | Object of (string * JSON) list
  | Array of JSON list
  | Number of int
  | String of string
  | Boolean of bool
  | Null

// Разбор JSON-строки в объект.
let rec parse json =
  let rec parse' json =
    let rec parseObject list = function
      | CloseBrace :: t -> (Object (List.rev list)), t
      | Comma :: t -> parseObject list t
      | Token.String s :: Colon :: t ->
        let a, t = parse' t
        parseObject ((s, a) :: list) t
      | _ -> failwith "Incorrect object"
    let rec parseArray list = function
      | CloseBracket :: t -> (Array (List.rev list)), t
      | Comma :: t -> parseArray list t
      | ob -> 
        let a, t = parse' ob
        parseArray (a :: list) t  
    match json with
      | OpenBrace :: t -> parseObject [] t
      | OpenBracket :: t -> parseArray [] t
      | Token.Null :: t -> JSON.Null, t
      | Token.String s :: t -> JSON.String s, t
      | Token.Number s :: t -> JSON.Number s, t
      | Token.Boolean s :: t -> JSON.Boolean s, t
      | _ -> failwith "Incorrect identification"
  match parse' json with
    | res, [] -> res
    | _ -> failwith "Wrong JSON structure"

// Сериализация объекта обратно в строку.
let rec stringify = function
  | Object list -> "{" + (String.concat "," (List.map (fun (x, y) -> "\"" + x + "\":" + (stringify y)) list)) + "}"
  | Array list -> "[" + (String.concat "," (List.map stringify list)) + "]"
  | Number int -> string int
  | String string -> "\"" + string + "\""
  | Boolean bool -> if bool then "true" else "false"
  | Null -> "null" 

// Генерация случайного JSON-объекта.
let generate () = 
  let random = new Random()
  let randomString length = 
    let chars = Array.append [|'A'..'Z'|]  [|'0'..'9'|]
    System.String ([| for i in 1..length -> chars.[random.Next(chars.Length)] |])

  let rec gen i =
    match random.Next(i) with
        | 0 -> Boolean false
        | 1 -> Boolean true
        | 2 -> Null
        | 3 -> Number(random.Next(100))
        | 4 -> String(randomString (random.Next(20)))
        | 5 -> Array [for i in 0..random.Next(i) -> gen (abs(i - 1))]
        | _ -> Object [for i in 0..random.Next(i) -> (randomString (random.Next(20)), gen (abs(i - 1)))]
  gen 20

// Добавление произвольного примитивного элемента в дерево.
let rec insert (tree : JSON) (path : int list) (key : string, value : JSON) = 
    match path with 
        | [] -> 
            match tree with 
                | Object list -> Object((key, value) :: list)
                | Array list -> Array(value :: list)
                | _ -> failwith "Incorrect path"
        | h :: t -> 
            let replace list n x =
                let rec replace' list n acc x = 
                    match list with 
                        | h :: t when n = 0 -> (List.rev acc) @ (x :: t)
                        | h :: t -> replace' t (n - 1) (h :: acc) x
                        | _ -> failwith "Out of range"
                replace' list n [] x
            match tree with 
                | Object list -> Object(replace list h (fst list.[h], insert (snd list.[h]) t (key, value)))
                | Array list -> Array(replace list h (insert list.[h] t (key, value)))
                | _ -> failwith "Incorrect path"

// Тесты


// 1
let json1 = """
{
  "a": [1, 2, 3, 4, 5]
  "b": "string"
  "c": {
    "d": [1,2,3]
    "e": [9, 8, 7, 6, 5, 4]
  }
}
"""

let tree1 = json1 |> explode |> tokenize |> parse

tree1
insert tree1 [2] ("key", String("value")) 
insert tree1 [0] ("age", Number(19)) 

let modTree1 = insert tree1 [2; 1] ("answer", Number(42))
stringify modTree1


// 2
let json2 = """
{
   "firstName": "Иван",
   "lastName": "Иванов",
   "address": {
       "streetAddress": "Московское ш., 101, кв.101",
       "city": "Ленинград",
       "postalCode": 101101
   },
   "phoneNumbers": [
       "812 123-1234",
       "916 123-4567"
   ]
}
""" 

let tree2 = json2 |> explode |> tokenize |> parse

let modTree2 = insert tree2 [2] ("country", String("USSR"))
stringify modTree2

// 3
let tree3 = generate () // вставка произойдет только если корневой JSON является объектом или массивом 

let modTree3 = insert tree3 [] ("randomKey", Boolean(true))
stringify modTree3


let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.143.158:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main ()