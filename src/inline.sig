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
signature INLINE = sig
  val map : (Inline -> Inline) -> Inline -> Inline;
  val app : (Inline -> unit) -> Inline -> unit;
  val serialize : Inline -> string;
end;

