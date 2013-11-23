(* ML program for adjusting page numbers in the index.
*)

val os= open_out "index.tex"

infix isin

fun (n:int) isin(lower,upper) = n >= lower andalso n <= upper


fun transform_page_no (n:int): int= 
  if n< 60 then n
  else if n isin (60,64) then n+1
  else if n=65 then 67
  else if n=66 then 69
  else if n isin(67,68) then n+4
  else if n isin(69,75) then n+4
  else if n isin(76,88) then n+5
  else if n isin(89,93) then n+6
  else (* n >= 94*) 
     (output(std_out, "warning: did nothing to: " ^ Int.string n); n)
        

fun readall(): string list = 
  let val is = open_in "oldindex.tex"
      fun loop() = 
          if end_of_stream is then [] else input(is, 1) :: loop()
  in loop() end

fun is_digit s = ord s <= ord "9" andalso ord s >= ord "0"
fun getwhile f (acc,rest as []) = (rev acc, rest)
  | getwhile (f:string -> bool) (acc: string list,(x:string) :: xs) = 
       if f x then getwhile f (x::acc, xs) else (rev acc, x::xs)
fun cycle xs : unit=
    case xs of
      [] => ()
    | _ => let val (not_number: string list, rest: string list) = getwhile (not o is_digit) ([],xs)
               val _ = output(os, implode not_number)
               val (number, rest) = getwhile is_digit ([],rest)
               val OK(page: int,_) = IntParse.parse (implode number)
               val page' = transform_page_no page
           in 
               output(os, Int.string page');
               cycle rest
           end
  
fun run() = (cycle(readall()) handle _ =>(); close_out os);
        

