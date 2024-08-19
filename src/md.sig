datatype Inline =
           Text of string
         | Emph of Inline list
         | Bold of Inline list
         | Code of string
         | Link of { link_url : string
                   , link_desc : Inline list }
         | Anchor of string  (* [#anchor-name] *)
         | Image of { img_src : string
                    , img_alt : string
                    };

(*
In a list, each <li> may consist of multiple blocks.
And a <ol> (resp., <ul>) is just a list of <li> elements.
*)
datatype 'a Block =
         Par of Inline list
       | Pre of 'a * (string option)
       | Heading of int * (Inline list)
       | Quote of ('a Block list)
       | UList of (('a Block list) list)
       | OList of (('a Block list) list);

signature MD = sig
  val parse : string -> (string Block) list;
  val parse_block : string list -> string Block list;
  val parse_inline : string -> Inline list;
end;
