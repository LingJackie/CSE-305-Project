let interpreter ( (input : string), (output : string )) : unit =
   (*takes in a pair of strings =>      string * string   *)
   
  (* Here we open an input channel for first argument, input, 
     and bind it to a variable ic so that we can refer it 
     later in loop_read function. *)
  let ic = open_in input in

  (* Use the second argument as file name to open an output channel,
     and bind it to variable oc for later reference. *)
  let oc = open_out output in 

  (* Helper function: file input function. It reads file line by line
     and return the result as a list of string.  *)
  let rec loop_read acc =
      (* We use try with to catch the End_of_file exception. *)
      try 
          (* Read a line from ic. Build a new list with l::acc
             and pass to next recursive call. *)
          let l = input_line ic in loop_read (l::acc)
      with
        (* At the end of file, we will reverse the string since
           the list is building by keeping add new element at the 
           head of old list. *)
      | End_of_file -> List.rev acc in

  (* Helper functions: file output function. It takes a bool/string value and
     write it to the output file.                                                              gonna need more of these for strings int and other stuff*)
  let file_write_bool bool_val = Printf.fprintf oc "%b\n" bool_val in
  let file_write_string string_val = Printf.fprintf oc "%s\n" string_val in

  (* This variable contains the result of input file from helper 
     function, loop_read. Please remember this is a list of string. 
     THIS IS WHERE ALL OF THE READ STUFF IS STORED *)
  let ls_str = loop_read [] in 

  (* ***** Code From Here, Replace () above and write your code ***** *)

  (*top of stack corresponds head of list*)
   let stack = [] in



   let split_to_list (command_line : string): string list = String.split_on_char ' ' command_line in 

   (*examples: ["push"; "1"]       -->    ("push", "1")  
               ["push"; "1"; "3"]  -->    ("push", "1")
   *)
   let rec convert_to_pair (command_lst : string list): string * string =
      match command_lst with
      | [] -> ("","")
      | _::[] -> ("","")
      | one :: two :: tl -> (one, two) in

   (*list_of_commands is ls_str*)
   let rec create_list_of_pairs (list_of_commands : string list): (string * string) list =
      match list_of_commands with
      | [] -> []
      | hd :: tl ->  convert_to_pair (split_to_list hd) :: create_list_of_pairs tl  in

   let pairList = create_list_of_pairs ls_str in 

   let rec hmmmm ( p_list : (string * string) list ): string list =
      match p_list with
      | [] -> []
      | ("", _)::_ -> []
      | (_, _)::_ -> []
      | ("push", value) :: tl ->value:: (hmmmm tl )




      
     (**-----------------------------------------------------------------------------------------------*) 
      (*has to be uppercase for some reason*)
      type command = Push of string | Pop | Add | None 

let rec split_to_list (command_lines : string list): string list = 
    match command_lines with
    | [] -> []
    | hd :: tl -> (String.split_on_char ' ' hd)@(split_to_list tl) 
    
  
let rec convert_to_commands(command_string_lst : string list): command list=
    match command_string_lst with
    | [] -> []
    | _ :: [] -> []
    | hd :: mid :: tl -> (match hd with
                        | "push" -> Push(mid)::(convert_to_commands tl)
                        | "pop" -> Pop :: (convert_to_commands (mid :: tl))
                        | "add" -> Add :: (convert_to_commands (mid :: tl))
                        | _ -> (convert_to_commands (mid :: tl))  )
    | hd :: tl -> Pop::(convert_to_commands tl)
(*st is the stack*)
let pop_top(st : string list): string list =
   match st with
   |[] -> []
   |hd :: tl -> tl
let rec executeCommand(command_lst : command list)(st : string list): string list = 
    match command_lst with
    | [] -> []
    | hd :: tl -> match hd with
                        | Push(x) -> st@(executeCommand tl st)@[x]
                        | Pop -> executeCommand tl st
                        | Add -> executeCommand tl st
                        | _ -> executeCommand tl  st



let stuff = ["push 1"; "push 2"; "pop"; "add";"push 6"; "pop";] 
let yeh = split_to_list stuff 
let hm = convert_to_commands yeh 
let sh = executeCommand hm 

(**-----------------------------------------------------------------------------------------------*) 


type command = Push of string | Pop | Add | Sub | None  

(*test if string is an int**)
let is_int s = try ignore (int_of_string s); true with _ -> false





let pop_top(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: tl -> tl 
let sub_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                          (string_of_int (int_of_string mid - int_of_string hd))::tl
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl 
let add_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                           (string_of_int (int_of_string hd + int_of_string mid))::tl
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl 
(**converts the string list into command list backwards ------- You'll know y*)
let rec convert_to_commands(command_string_lst : string list): command list=
   match command_string_lst with
   | [] -> []
   | hd :: mid :: tl -> (match hd with
                     | "push" -> (convert_to_commands tl)@[Push(mid)]
                     | "pop" ->  (convert_to_commands (mid :: tl))@[Pop]
                     | "add" ->  (convert_to_commands (mid :: tl))@[Add]
                     | "sub" ->  (convert_to_commands (mid :: tl))@[Sub]
                     | _ -> (convert_to_commands (mid :: tl))  ) 
   | hd :: tl -> (match hd with
                     | "pop" ->  (convert_to_commands tl)@[Pop]
                     | "add" ->  (convert_to_commands tl)@[Add]
                     | "sub" ->  (convert_to_commands tl)@[Sub]
                     | _ -> (convert_to_commands tl)  )
   | _ :: [] -> []



let rec executeCommand(command_lst : command list): string list = 
   match command_lst with
   | [] -> []
   | hd :: tl -> (match hd with
                       | Push(x) -> x::(executeCommand tl)
                       | Pop -> pop_top (executeCommand tl)
                       | Add -> add_op(executeCommand tl) 
                       | Sub -> sub_op(executeCommand tl) 
                       | _ -> executeCommand tl ) 
   | _ :: [] -> []

   let test = ["push";"1"; "push"; "2"; "pop"; "add";"push"; "6"; "pop";] 
   let convert = convert_to_commands test
   let sh = executeCommand convert 

   let stack2 = executeCommand (convert_to_commands(["push";"1"; "push"; "2"; "pop"; "add";"push"; "6"; "pop";"pop";"pop"]))
   let stack3 = executeCommand (convert_to_commands(["push";"1"; "push"; "2"; "add";"push"; "6"]))
   let stack4 = executeCommand (convert_to_commands(["push";"1"; "push"; "2"; "add";"push"; "6"; "sub"]))
   let stack4 = executeCommand (convert_to_commands(["push";"1"; "push"; ":error:"; "push"; "6"; "sub"]))
(**-----------------------------------------------------------------------------------------------*) 

   (**pangram stuff*)
   let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';
                  'm';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in


   (*Helper   if is a pangram then the and operation on all letters will be true 
         if there is a single false ie a letter is not there then all of it will be false
      String.contains s c tests if character c appears in the string s*)            
   let rec check (sentence : string) (alpha : char list): bool = 
      match alpha with 
      | [] -> true
      | hd :: tl -> String.contains sentence hd && check sentence tl in

   let rec bool_list (sentence_list : string list) (alpha : char list): bool list =
      match sentence_list with
      | [] -> []
      | hd :: tl -> check hd alpha :: bool_list tl alpha in

   let rec print_bool_list (b_lst : bool list) : unit =
      match b_lst with
      | [] -> ()
      | hd :: tl -> file_write hd; print_bool_list tl in
   
   let bl = bool_list ls_str alphabet in print_bool_list bl in
   interpreter ("input.txt", "output.txt")