type alphabet = ITA1 | ITA2 | ITA3 | ITA4 | SITOR
type mode = LetterMode | FigureMode | Both
type sub_command = 
        Five of         bool * bool * bool * bool * bool
        | Six of        bool * bool * bool * bool * bool * bool
        | Seven of      bool * bool * bool * bool * bool * bool * bool
type command = Command of mode * sub_command

let all_alphabets = [ ITA1 ; ITA2 ; ITA3 ; ITA4 ; SITOR ]
let alphabet_to_string a = match a with
| ITA1  -> "ita1"
| ITA2  -> "ita2"
| ITA3  -> "ita3"
| ITA4  -> "ita4"
| SITOR -> "sitor"

module CharMap = Map.Make(Char)

let bool_to_char b = match b with
| false -> ' '
| true  -> '.'

let mode_to_string m = match m with
| LetterMode -> "LETTER"
| FigureMode -> "FIGURE"
| Both       -> "BOTH  "

let sub_command_to_string s = match s with 
| Five (a, b, c, d, e) -> 
                Printf.sprintf 
                        "[%c%c%c%c%c]" 
                        (bool_to_char a) 
                        (bool_to_char b)
                        (bool_to_char c)
                        (bool_to_char d)
                        (bool_to_char e)
| Six  (a, b, c, d, e, f) -> 
                Printf.sprintf
                        "[%c%c%c%c%c%c]" 
                        (bool_to_char a) 
                        (bool_to_char b)
                        (bool_to_char c)
                        (bool_to_char d)
                        (bool_to_char e)
                        (bool_to_char f)
| Seven(a, b, c, d, e, f, g) -> 
                Printf.sprintf
                        "[%c%c%c%c%c%c%c]" 
                        (bool_to_char a) 
                        (bool_to_char b)
                        (bool_to_char c)
                        (bool_to_char d)
                        (bool_to_char e)
                        (bool_to_char f)
                        (bool_to_char g)

let command_to_string (c: command) = match c with
| Command(m, sc) -> Printf.sprintf 
        "%s - %s" 
        (mode_to_string m) 
        (sub_command_to_string sc)

let pretty_print_map (m: command CharMap.t) = 
        CharMap.iter (fun k v -> 
                Printf.printf "[%c] -> %s\n" k (command_to_string v)) m

let five_00000 = Five(false, false, false, false, false)
let five_00001 = Five(false, false, false, false, true)
let five_00010 = Five(false, false, false, true,  false)
let five_00011 = Five(false, false, false, true,  true)
let five_00100 = Five(false, false, true,  false, false)
let five_00101 = Five(false, false, true,  false, true)
let five_00110 = Five(false, false, true,  true,  false)
let five_00111 = Five(false, false, true,  true,  true)
let five_01000 = Five(false, true,  false, false, false)
let five_01001 = Five(false, true,  false, false, true)
let five_01010 = Five(false, true,  false, true,  false)
let five_01011 = Five(false, true,  false, true,  true)
let five_01100 = Five(false, true,  true,  false, false)
let five_01101 = Five(false, true,  true,  false, true)
let five_01110 = Five(false, true,  true,  true,  false)
let five_01111 = Five(false, true,  true,  true,  true)
let five_10000 = Five(true,  false, false, false, false)
let five_10001 = Five(true,  false, false, false, true)
let five_10010 = Five(true,  false, false, true,  false)
let five_10011 = Five(true,  false, false, true,  true)
let five_10100 = Five(true,  false, true,  false, false)
let five_10101 = Five(true,  false, true,  false, true)
let five_10110 = Five(true,  false, true,  true,  false)
let five_10111 = Five(true,  false, true,  true,  true)
let five_11000 = Five(true,  true,  false, false, false)
let five_11001 = Five(true,  true,  false, false, true)
let five_11010 = Five(true,  true,  false, true,  false)
let five_11011 = Five(true,  true,  false, true,  true)
let five_11100 = Five(true,  true,  true,  false, false)
let five_11101 = Five(true,  true,  true,  false, true)
let five_11110 = Five(true,  true,  true,  true,  false)
let five_11111 = Five(true,  true,  true,  true,  true)

let ita2 : command CharMap.t = CharMap.(
        empty
        |> add 'A'  (Command (LetterMode, five_11000))
        |> add 'B'  (Command (LetterMode, five_10011))
        |> add 'C'  (Command (LetterMode, five_01110))
        |> add 'D'  (Command (LetterMode, five_10010))
        |> add 'E'  (Command (LetterMode, five_10000))
        |> add 'F'  (Command (LetterMode, five_10110))
        |> add 'G'  (Command (LetterMode, five_01011))
        |> add 'H'  (Command (LetterMode, five_00101))
        |> add 'I'  (Command (LetterMode, five_01100))
        |> add 'J'  (Command (LetterMode, five_11010))
        |> add 'K'  (Command (LetterMode, five_11110))
        |> add 'L'  (Command (LetterMode, five_01001))
        |> add 'M'  (Command (LetterMode, five_00111))
        |> add 'N'  (Command (LetterMode, five_00110))
        |> add 'O'  (Command (LetterMode, five_00011))
        |> add 'P'  (Command (LetterMode, five_01101))
        |> add 'Q'  (Command (LetterMode, five_11101))
        |> add 'R'  (Command (LetterMode, five_01010))
        |> add 'S'  (Command (LetterMode, five_10100))
        |> add 'T'  (Command (LetterMode, five_00001))
        |> add 'U'  (Command (LetterMode, five_11100))
        |> add 'V'  (Command (LetterMode, five_01111))
        |> add 'W'  (Command (LetterMode, five_11001))
        |> add 'X'  (Command (LetterMode, five_10111))
        |> add 'Y'  (Command (LetterMode, five_10101))
        |> add 'Z'  (Command (LetterMode, five_10001))
        |> add '0'  (Command (FigureMode, five_01101))
        |> add '1'  (Command (FigureMode, five_11101))
        |> add '2'  (Command (FigureMode, five_11001))
        |> add '3'  (Command (FigureMode, five_10000))
        |> add '4'  (Command (FigureMode, five_01010))
        |> add '5'  (Command (FigureMode, five_00001))
        |> add '6'  (Command (FigureMode, five_10101))
        |> add '7'  (Command (FigureMode, five_11100))
        |> add '8'  (Command (FigureMode, five_01100))
        |> add '9'  (Command (FigureMode, five_00011))
        |> add '?'  (Command (FigureMode, five_10011))
        |> add ':'  (Command (FigureMode, five_01110))
        |> add '-'  (Command (FigureMode, five_11000))
        |> add '+'  (Command (FigureMode, five_10001))
        |> add '/'  (Command (FigureMode, five_10111))
        |> add '='  (Command (FigureMode, five_01111))
        |> add '('  (Command (FigureMode, five_11110))
        |> add ')'  (Command (FigureMode, five_01001))
        |> add '.'  (Command (FigureMode, five_00111))
        |> add ','  (Command (FigureMode, five_00110))
        |> add '\'' (Command (FigureMode, five_10100))
        |> add ' '  (Command (Both,       five_00100))
        |> add '\r' (Command (Both,       five_00010))
        |> add '\n' (Command (Both,       five_01000))
)

