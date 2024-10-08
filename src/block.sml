structure Block : BLOCK = struct
fun map (f : 'a Block -> 'b Block) (block : 'a Block) : 'b Block =
    case block of
        (Par _) => f block
      | (Pre _) => f block 
      | (Heading _) => f block
      | (Quote blocks) => (Quote (List.map (map f) blocks))
      | (UList items) => (UList (List.map
                                     (fn lines =>
                                         List.map (map f) lines)
                                     items))
      | (OList items) => (OList (List.map
                                     (fn lines =>
                                         List.map (map f) lines)
                                     items));

fun app f block =
    case block of
        (Par _) => f block
      | (Pre _) => f block 
      | (Heading _) => f block
      | (Quote blocks) => (List.app (app f) blocks)
      | (UList items) => (List.app
                              (fn lines =>
                                  List.app (app f) lines)
                              items)
      | (OList items) => (List.app
                              (fn lines =>
                                  List.app (app f) lines)
                              items);

fun serialize f =
    fn (Par items) => ("Par ["^
                       (String.concatWith ", "
                                          (List.map Inline.serialize
                                                    items))^
                       "]")
  | (Pre {code,language,is_example}) =>
    ("Pre {code = \""^(f code)^
     "\", is_example = " ^
     (if is_example then "true" else "false")^
     ", language = "^
     (case language of
          NONE => "NONE"
        | (SOME m) => ("SOME "^m))^
     "}")
  | (Heading (lvl, x)) =>
    ("Heading ("^(Int.toString(lvl))^
     ", [" ^
     (String.concatWith "," (List.map Inline.serialize x)) ^
     "])")
  | (Quote blocks) =>
    ("Quote [" ^
     (String.concatWith ", " (List.map (serialize f) blocks)) ^
     "]")
  | (UList items) => 
    ("UList [" ^
     (String.concatWith
          ", "
          (List.map
               (fn subblocks =>
                   "["^(String.concatWith ", "
                                          (List.map (serialize f)
                                                    subblocks))^"]")
               items)) ^
     "]")
  | (OList items) =>
    ("OList [" ^
     (String.concatWith
          ", "
          (List.map
               (fn subblocks =>
                   "["^(String.concatWith ", "
                                          (List.map (serialize f)
                                                    subblocks))^"]")
               items)) ^
     "]");


end;
