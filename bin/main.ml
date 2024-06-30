open Alphabets

exception UnmatchedAlphabet of string

let usage = "baudot_translator -a <alphabet> -o <output> <input>"

let alpha = ref ITA2
let input_file = ref ""
let output_file = ref ""

let set_alphabet name = match String.lowercase_ascii name with  
| "ita1"  -> alpha := ITA1
| "ita2"  -> alpha := ITA2
| "ita3"  -> alpha := ITA3
| "ita4"  -> alpha := ITA4
| "sitor" -> alpha := SITOR
| x       -> raise (
        UnmatchedAlphabet 
        (Printf.sprintf "Unknown alphabet type : %s\n" x)
)

let anon_fun filename = input_file := filename
let speclist = [
        (
                "-a", 
                Arg.Symbol (
                        List.map alphabet_to_string all_alphabets, 
                        set_alphabet
                ), 
                "Alphabet to use"
        );
        (
                "-o", 
                Arg.Set_string output_file,
                "Output file name"
        )]

(*let _main = 
        let out_channel = open_out !output_file in
        let in_channel  = open_in  !input_file  in
        
        pretty_print_map ita2;

        close_out out_channel;
        try
                close_in  in_channel
        with e -> 
                close_in_noerr in_channel;
                raise e
*)
let _entry = Arg.parse speclist anon_fun usage;
        Printf.printf
                "Alphabet : %s\nInput : %s\nOutput : %s\n"
                (alphabet_to_string !alpha)
                !input_file
                !output_file
                
let () = pretty_print_map ita2
