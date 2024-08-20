structure Inline : INLINE = struct

fun map (f : Inline -> Inline) (elt : Inline) : Inline =
  case elt of
      (Text _) => f elt
    | (Emph x) => Emph (List.map (map f) x)
    | (Bold x) => Bold (List.map (map f) x)
    | (Code _) => f elt
    | (Link {link_url,link_desc}) => Link {link_desc=List.map (map f) link_desc
                                          , link_url = link_url }
    | (Anchor _) => f elt
    | (Image _) => f elt;

fun app f elt =
  case elt of
      (Text _) => f elt
    | (Emph x) => List.app (app f) x
    | (Bold x) => List.app (app f) x
    | (Code _) => f elt
    | (Link {link_url,link_desc}) => List.app (app f) link_desc
    | (Anchor _) => f elt
    | (Image _) => f elt;
    

fun serialize elt =
  case elt of
      (Text s) => ("Text \""^s^"\"")
    | (Emph x) => ("Emph "^(serialize_strings
                                (List.map serialize x)))
    | (Bold x) => ("Bold "^(serialize_strings
                                (List.map serialize x)))
    | (Code x) => "Code \""^x^"\""
    | (Link {link_url,link_desc}) =>
      ("Link {link_url=\""^
       link_url^"\","^
       "link_desc="^(serialize_strings
                         (List.map serialize link_desc))^"}")
    | (Anchor x) => "Anchor \""^x^"\""
    | (Image {img_src,img_alt}) => "Image {img_src=\""^
                                   img_src^"\","^
                                   "img_alt="^img_alt^"}";

end;
