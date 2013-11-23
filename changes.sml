infix to;

(* structure to bring code written for NJ ML into Standard ML *)

structure correction=  
struct
  val input = input (* for NJ ML:  fn (is,n)=> input is n; *)
  val output= output (* for NJ ML: fn (os,s)=> output os s; *)
  exception Hd and Tl
  fun hd[] = raise Hd
    | hd(x::_) = x
  and tl[] = raise Tl
    | tl(_::x) = x
  fun length[] = 0
    | length(_::x) = 1 + length x
  fun print s = output(std_out, s)
  fun intToString i =  
      (if i<0 then "~" else "") ^ natToString (abs i)

  and natToString n =
      let val d = n div 10 in
        if d = 0 then chr(ord"0" + n)
        else natToString(d) ^ chr(ord"0" + (n mod 10))
      end
  val makestring = intToString
end;

structure setup =
struct
  val stand_alone=false;
  val first_page = "80";
  val idxfile = "root.idx";
  val latexfile = "changes.tex";
  val source_files = ["root.tex", "preface.tex","intro.tex", "syncor.tex",
                      "synmod.tex", "statcor.tex", "statmod.tex",
                      "dyncor.tex", "dynmod.tex", "prog.tex", 
                      "app1.tex", "app2.tex", "app3.tex",
                      "app4.tex", "overloading.tex", "app5.tex",
                      "whatisnew.tex", "index.tex"]
  fun pickup l = true (*List.member (implode l)  
                 ["{\\thetypabbr}",
                  "{\\thenoimptypes}",
                  "{\\thefixtypos}",
                  "{\\thenostrsharing}",
                  "{\\thenoinfixinspec}",
                  "{\\thenofuncspec}",
                  "{\\thenoeqtypespec}",
                  "{\\thenolocalspec}",
                  "{\\thenoopenspec}",
                  "{\\thenoclosurerestriction}",
                  "{\\thenotypexplication}",
                  "{\\thesingleincludespec}"]*)
end; (*setup*)

structure io_util=
struct
  open correction;
  exception Read of string;
  fun read{is=is:instream,s=s:string}:string=
  (* returns the string that is between the beginning of is
     and the first occurrence of s. If s does not occur on
     is, Read(s) is raised *)
    if s="" then ""
    else let val sl = explode s;
             val first_s = hd sl;
             val buf = ref ([]:string list)
             (* we always have |buf|<=|s| *)
             val result = ref ([]:string list)
             (* reads up to first_s, which after the call
                will be in the buffer *)
             fun findchr () =
             if !buf = [] then
                   let val  first_in = input(is,1)
                   in  if      first_in = "" then raise Read(s)
                       else if first_in = first_s then (*found*)
                         (buf:= [first_in]; mask())
                       else (* store up first_in*)
                            (result:=first_in::(!result);
                             findchr())
                   end
             else
                   let val first_in = hd (!buf)
                   in  if      first_in = first_s then (*found*) mask()
                       else (result:=first_in::(!result);
                             buf:=tl(!buf); findchr())
                   end
             (* given that the first letters match, check
                that the remaining match *)
             and mask() =
                if length(!buf)>size(s) then
                  output (std_out, ("\n error in mask: buf= "^ (implode(!buf))
                          ^ ", looking for: " ^ s))
                else 
                 (* expand buffer to be of the same length as sl *)
                 (if length(!buf)<size s then 
                     buf := !buf @ explode(input(is,(size s - length (!buf))))
                  else (*same size*) ();
                  if length(!buf)<size s (*eof*) then raise Read  (s)
                  else if !buf = sl then () (*finished*)
                       else (*failure, store up one char*) 
                         (result:=(hd(! buf))::(!result);
                          buf:= tl(!buf); findchr())
                 )
         in findchr(); implode(rev(!result)) end;               

  exception Skip of string;

  fun skip{is=is:instream,s=s:string}=
  (* skips everything on is up to and including the first
     occurrence of s. If s does not occur on is, Skip(s) is
     raised and eof will be true*)
      (read{is=is,s=s}; ()) handle Read  s=> raise Skip  s

end; (*io_util*)      


structure table=
struct
  type conversion_table = (string*string)list
  val empty = []
  exception Insert of {idxkey:string}
  fun insert{idxkey=idxkey:string,pageref=pageref:string,
             ct=[]:conversion_table}=
      [(idxkey,pageref)]
    | insert{idxkey,pageref,ct as((x,y)::rest)}=
      if idxkey=x then raise Insert  {idxkey=idxkey}
                  else (x,y):: insert{idxkey=idxkey,pageref=pageref,ct=rest};


  exception Lookup of string
  fun lookup{idxkey=idxkey:string,ct=[]:conversion_table}=       
      raise Lookup  idxkey
    | lookup{idxkey,ct=ct as (x,y)::rest}=       
      if x=idxkey then y else lookup{idxkey=idxkey,ct=rest}
end; (*table*)

functor Index(X : sig
  exception Skip of string;
  val skip: {is:instream,s:string}->unit
  (* skips everything on is up to and including the first
     occurrence of s. If s does not occur on is, Skip(s) is
     raised and eof will be true*)

  exception Read of string;
  val read: {is:instream,s:string}->string
  (* returns the string that is between the beginning of is
     and the first occurrence of s. If s does not occur on
     is, Read(s) is raised *)

  type conversion_table;
     (* conversion from the idx keys to page references *)
  val empty: conversion_table;
  exception Insert of {idxkey:string}
  val insert: {idxkey:string,pageref:string,ct:conversion_table}->
               conversion_table
     (* inserts pageref under the key idxkey in ct. If idxkey already
        is present, Insert is raised *)
  exception Lookup of string
  val lookup: {idxkey:string,ct:conversion_table}->string
     (* returns the pageref recorded in ct under the entry of
        idxkey, if such an entry is present. Otherwise, Lookup(idxkey)
        is raised *)
  val stand_alone : bool
     (* depending on whether the latex file is to be included in the
        root file or not *)

  val source_files: string list

  val pickup: string list -> bool

  val first_page: string
     (* string giving number of first page of index, 
        in case it is stand_alone *)

  val idxfile: string;
  val latexfile: string end
)=
struct
  open X;
  val itemCount = ref 0; (* number of items in the index - 
                            one per item or subitem *)
  val refCount = ref 0;  (* number of page references *)

  open correction;
  val  outbuf = ref "";
  fun warning(s:string)= outbuf:= !outbuf ^ ("\nWARNING: " ^ s);



  fun build_conversion_table():conversion_table =
    (* returns a conversion table obtained from the idxfile by
       repeatedly seeking out lines of the form
         \indexentry{idxkey}{pageref}
    *)
  let val idxfile=open_in idxfile;
      fun readRest(ct:conversion_table):conversion_table=
          if end_of_stream(idxfile) then ct
          else
          (skip{is=idxfile,s="\\indexentry{"};
           readRest(insert{idxkey = read{is=idxfile , s="}{"   },
                           pageref = read{is=idxfile, s= "}" },
                           ct     = ct
                          }
                    handle Insert  {idxkey}=> (warning("the idxkey " ^idxkey^  
                                                 " is defined more than once");
                                          ct)
                   )
           handle Read  s=> (warning ("eof while looking for " ^ s); ct)
          )handle Skip  _ => ct;
      val result = readRest(empty)
  in
      close_in idxfile; result
  end (*build_conversion_table*)     
  
      val latexfile:outstream = open_out latexfile;
      datatype inputItem = p of string (* single page reference *)
                         | op to of string*string (* interval *)
    
      val (buffer,buflength)=(ref"",ref 0);
      fun resetBuf() = (buffer:=""; buflength:=0);
      fun emptyBuf() = (output( latexfile, !buffer);resetBuf());
      fun addToBuf(s:string)=
        (buffer:= !buffer ^ s;
         buflength:= !buflength + size s;
         if !buflength>5000 then emptyBuf() else ()
        )
      infix to;
    

      val ct = build_conversion_table();

      datatype 'a option = None | Some of 'a
      datatype change = INSERT of {page: string,  new: string}
                      | REPLACEMENT of {page: string , old: string, new: string}
                      | DELETION of {page: string , old: string}
                      | NOTE of {page: string , comment: string}

      (* get_changes(filename, n, acc) accumulates the changes found in the (tex) file <filename>
         onto the list acc, using n to count how many changes have already be found *)

      exception Abort
      fun get_changes(filename: string) (n : int, acc: change list)= 
          let val l = explode(StringParse.fromFile filename) handle _ => 
                       (output(std_out, "Cannot open" ^ filename); raise Abort)
              val lbrack = ref 0
              val lineno= ref 0
              fun inc() = lineno:= !lineno+1
              fun get_brack(level:int, rest) =
                   case (level, rest) of
                     (n , "\n" :: rest) => 
                          let val (new, rest) = (inc(); get_brack(n, rest))
                          in ("\n" :: new, rest)
                          end	
                   | (0, "}"::rest1) => (["}"], rest1)
                   | (n, "}"::rest2) => 
                          let val (new, rest) = get_brack(n-1, rest2)
                          in ("}" :: new, rest)
                          end
                   | (n, "{"::rest2) => 
                          let val (new, rest) = (get_brack(n+1, rest2))
                          in ("{" :: new, rest)
                          end
                   | (n, "\\" :: "{" :: rest) =>
                          let val (new, rest) = get_brack(n, rest)
                          in ("\\" :: "{"  :: new, rest)
                          end
                   | (n, "\\" :: "\\" :: "}" :: rest) =>
                          let val (new, rest) = get_brack(n, "}" ::rest)
                          in ("\\" :: "\\" :: "}"  :: new, rest)
                          end
                   | (n, "\\" :: "}" :: rest) =>
                          let val (new, rest) = get_brack(n, rest)
                          in ("\\" :: "}"  :: new, rest)
                          end
                   | (n, ch ::rest) => 
                          let val (new, rest) = get_brack(n, rest)
                          in (ch :: new, rest)
                          end
                   | (n, []) => (output(std_out, "Not bracketed change: " ^ filename 
                                        ^ " started line " ^ Int.string (!lbrack)); raise Abort)

              fun get_bracketed(rest) = (lbrack:= !lineno;get_brack(~1,rest))

              fun position() = filename ^ " line: " ^ Int.string (!lineno) ^ ": "

              fun cycle ("\\" :: "i" :: "n" :: "s" :: "e" :: "r" :: "t" :: "i" :: "o" :: "n" :: rest , n, acc) =
                     let val (cond: string list, rest1) = get_bracketed(rest)
                         val (new: string list, rest1) = get_bracketed(rest1)
                         
                     in 
                        if pickup cond
                        then let val page_ref: string = lookup{idxkey = "ins" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^"Cannot find key: " ^ "ins" ^ Int.string n ^ "\n"); "???")
                             in 
                                 cycle(rest1, n+1, INSERT{page = page_ref,  new = implode new}::acc)
                             end
                        else 
                           cycle(rest1, n, acc)
                     end
                |  cycle ("\\" :: "a"::"d"::"h"::"o"::"c"::"i" :: "n" :: "s" :: "e" :: "r" :: "t" :: "i" :: "o" :: "n" :: rest , n, acc) =
                     let val (cond: string list, rest1) = get_bracketed(rest)
                         val (_, rest1) = get_bracketed rest1  (*skip dimension*)
                         val (new: string list, rest1) = get_bracketed(rest1)
                         
                     in 
                        if pickup cond
                        then let val page_ref: string = lookup{idxkey = "ins" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^ "Cannot find key: " ^ "ins" ^ Int.string n ^ "\n"); "???")
                             in 
                                 cycle(rest1, n+1, INSERT{page = page_ref,  new = implode new}::acc)
                             end
                        else 
                           cycle(rest1, n, acc)
                     end
                | cycle ("\\" :: "r" :: "e" :: "p" :: "l" :: "a" :: "c" :: "e" :: "m" :: "e" :: "n" :: "t" :: rest , n, acc) =
                     let val (cond: string list, rest1) = get_bracketed(rest)
                         val (old: string list, rest1) = get_bracketed(rest1)
                         val (new: string list, rest1) = get_bracketed(rest1)
                         
                     in 
                        if pickup cond
                        then let val page_ref: string = lookup{idxkey = "repl" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^"Cannot find key: " ^ "repl" ^ Int.string n^ "\n");"???")
                             in cycle(rest1, n+1, REPLACEMENT{page = page_ref,  old = implode old, new = implode new}::acc)
                             end
                        else cycle(rest1, n, acc)
                     end
                | cycle ("\\" :: "a" :: "d" :: "h" :: "o" :: "c":: "r" :: "e" :: "p" :: "l" :: "a" :: "c" :: "e" :: 
                         "m" :: "e" :: "n" :: "t" :: "l" :: rest , n, acc) =
                     let val (cond: string list, rest) = get_bracketed(rest)
                         val (_, rest) = get_bracketed(rest)  (* skip dimension *)
                         val (old: string list, rest1) = get_bracketed(rest)
                         val (new: string list, rest1) = get_bracketed(rest1)
                         
                     in 
                         if pickup cond
                         then let val page_ref: string = lookup{idxkey = "repl" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^ "Cannot find key: " ^ "repl" ^ Int.string n^ "\n");"???")
                              in cycle(rest1, n+1, REPLACEMENT{page = page_ref,  old = implode old, new = implode new}::acc)
                              end
                         else cycle(rest1, n, acc)
                     end
                | cycle ("\\" :: "a" :: "d" :: "h" :: "o" :: "c":: "r" :: "e" :: "p" :: "l" :: "a" :: "c" :: "e" :: 
                         "m" :: "e" :: "n" :: "t" :: "t" :: "e" :: "x":: "l" :: rest , n, acc) =
                     let val (cond: string list, rest) = get_bracketed(rest)
                         val (_, rest) = get_bracketed(rest)  (* skip dimension *)
                         val (old: string list, rest1) = get_bracketed(rest)
                         val (new: string list, rest1) = get_bracketed(rest1)
                         
                     in 
                         if pickup cond
                         then let val page_ref: string = lookup{idxkey = "repl" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^ "Cannot find key: " ^ "repl" ^ Int.string n^ "\n");"???")
                              in cycle(rest1, n+1, REPLACEMENT{page = page_ref,  old = implode old, new = implode new}::acc)
                              end
                         else cycle(rest1, n, acc)
                     end
                | cycle ("\\" :: "d" :: "e" :: "l" :: "e" :: "t" :: "i" :: "o" :: "n" :: rest , n, acc) =
                     let val (cond: string list, rest1) = get_bracketed(rest)
                         val (old: string list, rest1) = get_bracketed(rest1)
                         
                     in if pickup cond
                        then let val page_ref: string = lookup{idxkey = "del" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^ "Cannot find key: " ^ "del" ^ Int.string n^ "\n");"???")
                             in cycle(rest1, n+1, DELETION{page = page_ref, old = implode old}::acc)
                             end
                        else cycle(rest1, n, acc)
                     end
                | cycle ("\\" :: "a"::"d"::"h"::"o"::"c"::"d" :: "e" :: "l" :: "e" :: "t" :: "i" :: "o" :: "n" :: rest , n, acc) =
                     let val (cond: string list, rest1) = get_bracketed(rest)
                         val (_, rest1) = get_bracketed rest1  (* skip dimension *)
                         val (old: string list, rest1) = get_bracketed(rest1)
                         
                     in if pickup cond
                        then let val page_ref: string = lookup{idxkey = "del" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^ "Cannot find key: " ^ "del" ^ Int.string n^ "\n");"???")
                             in cycle(rest1, n+1, DELETION{page = page_ref, old = implode old}::acc)
                             end
                        else cycle(rest1, n, acc)
                     end
                | cycle ("\\" :: "n" :: "o" :: "t" :: "e" :: rest, n, acc) = 
                     let val (cond: string list, rest1) = get_bracketed(rest)
                         val (comment: string list, rest1) = get_bracketed(rest1)
                         
                     in if pickup cond
                        then let val page_ref: string = lookup{idxkey = "note" ^ Int.string n, ct = ct}
                                                 handle _ => (output(std_out, position() ^ "Cannot find key: " ^ "note" ^ Int.string n^ "\n");"???")
                             in cycle(rest1, n+1, NOTE{page = page_ref, comment = implode comment}::acc)
                             end
                        else cycle(rest1, n, acc)
                     end

                | cycle ("\n" :: rest, n, acc) = (inc(); cycle(rest,n,acc))
                | cycle (ch :: rest, n ,acc) = cycle (rest, n, acc)
                | cycle ([ ], n, acc) = (n,acc)
          in
             cycle(l,n,acc)
          end
                
      fun get_changes_many_files l =
          let val (n,acc) = List.foldL get_changes (1, []) l
          in output(std_out, "Found " ^ Int.string n ^ " changes\n");
             acc
          end

      val all_changes = rev(get_changes_many_files source_files)

      fun write_change os (change: change) = 
          case change of 
            INSERT{page,  new} => 
               output(os, "\n%\n\\insertionPage{" ^ page ^ "}" ^ new^ "\n")
          | REPLACEMENT{page,  old, new} => 
               output(os, "\n%\n\\replacementPage{" ^ page ^ "}" ^ old ^ new^"\n")
          | DELETION{page,  old} => 
               output(os, "\n%\n\\deletionPage{" ^ page ^ "}" ^ old^"\n")
          | NOTE{page, comment} => 
               output(os, "\n%\n\\notePage{" ^ page ^ "}" ^ comment^"\n")

      val _ = (* produce latex files with list of changes *)
              List.apply (write_change latexfile) all_changes

      type interval = {frompage: int, topage: int}
      exception StringListToInt;
      fun stringToInt s = stringListToInt(rev(explode s),1)
      and stringListToInt ([],ten_to_x) = 0
        | stringListToInt ((s::rest),ten_to_x) = 
          (ord s - ord "0")*ten_to_x + stringListToInt(rest,10*ten_to_x)

      fun convert(i:inputItem):interval=
        (case i of 
             p s=> let val n = stringToInt s in {frompage=n,topage=n} end
          | s to s' =>{frompage= stringToInt s, topage= stringToInt s'})
      fun lookupItem(i:inputItem):inputItem =
        (case i of
             p(s) => p(lookup{idxkey=s,ct=ct})
           | s to s' => lookup{idxkey=s,ct=ct}  to
                        lookup{idxkey=s',ct=ct}
       )handle Lookup  idxkey=> (warning ("the idxkey " ^ idxkey ^ 
                                          " is not defined by the idxfile");
                                 p "0")
                       
(*
      fun entry(kind:string,key:string,l:inputItem list)=
       (addToBuf("\n" ^ kind ^ key ^ (if l=[] then " " else ", ")); 
        itemCount:= !itemCount+1;
        if (!itemCount mod 10) = 0 then output(std_out,"\n.")else 
                                        output(std_out,".");
        print_entries (compress (map (convert o lookupItem) l))); 

   
      fun item(key:string)(l:inputItem list)= 
       (print (key ^ "\n"); entry ("\\item ",key,l));
      fun subitem(key:string)(l:inputItem list)= entry ("\\subitem ",key,l);
*)
 

      fun --(s:string)=
      addToBuf("\n\\indexspace\n\\parbox{65mm}{\\hfil{\\large\\bf " 
               ^s^ "}\\hfil}\n\\indexspace");
(*old
  val _ =  
      if stand_alone then
        (addToBuf("\\documentstyle[a4,12pt,twoside]{article}");
         addToBuf("\n\\include{mac}");
         addToBuf("\n\\pagestyle{headings}");
         addToBuf("\n\\begin{document}");
         addToBuf("\n\\setcounter{page}{" ^ first_page ^"}")
        )
      else ((addToBuf("\\addcontentsline{toc}{section}{\\protect\\numberline{}{Index}}");
            addToBuf("\n\\label{index-sec}"));
      addToBuf("\n\\begin{theindex}"));
*)



      fun terminate() = ( 
         (* addToBuf("\n\\end{theindex}"); *)
         (* if stand_alone then addToBuf("\n\\end{document}") else (); *)
         emptyBuf();
         close_out latexfile; 
         output(std_out, !outbuf);
         {items = !itemCount, pagerefs= !refCount})
  
  
end; (*Index*)

structure Run= Index(struct open setup io_util table end);

val it= Run.terminate();
