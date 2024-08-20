(*
TODO: Look at the POSIX utility conventions for formatting
flags, https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
*)

(* output_file : string -> string -> string

Determine the output file's name given the `input_name` of the
input file and the `output_path` for the directory where we wish
to produce the output.

WARNING: this will OVERWRITE any pre-existing HTML files.
*)
fun output_file input_name "" =
    (if is_md input_name
     then md_to_html input_name
     else input_name^".html")
  | output_file input_name output_path =
    (if OS.FileSys.isDir output_path
     then (let
            val p = OS.FileSys.fullPath output_path;
            val name = output_file input_name "";
          in
            OS.Path.fromUnixPath
                ((OS.Path.toUnixPath p) ^"/"^
                 (OS.Path.file name))
          end)
     else if String.isSuffix ".html" (string_to_lower output_path)
     then output_path
     else (output_path ^ ".html"));

fun read_file path =
    let
      val inStream = TextIO.openIn path;
      val txt = TextIO.inputAll inStream;
    in
      TextIO.closeIn inStream;
      txt
    end;

fun write_to_file output_name contents =
    let
      val outStream = TextIO.openOut output_name;
    in
      TextIO.output(outStream, contents);
      TextIO.flushOut outStream;
      TextIO.closeOut outStream
    end;

(*
generate_html : string -> string -> string -> unit

Parses the `input_name` as Markdown, then generates HTML5 with
the appropriate header modified by the `css_links`, and sticks
the resulting HTML5 text in `output_name`.
*)
fun generate_html input_name output_name css_links =
  let
    val s = html5 (read_file input_name) css_links "";
  in
    print ("Writing "^output_name^"\n");
    write_to_file output_name s
  end;

(* list_dir_contents : string -> bool -> string list

If the `bool` parameter is `true`, then this lists either the
absolute paths for all the file and folders appearing in a given
directory name.

If the `bool` parameter is `false`, then this lists all the
**names** of the files and folders appearing in the directory
name. 

Expects a relative or absolute path to the directory.
*)
fun list_dir_contents dirname is_abs_path=
  let
    val p = OS.FileSys.fullPath dirname;
    val dir = OS.FileSys.openDir dirname;
    fun read_iter acc =
        (case OS.FileSys.readDir dir of
             NONE => (OS.FileSys.closeDir dir;
                      acc)
           | SOME(name) => read_iter
                               ((if is_abs_path
                                 then OS.Path.joinDirFile
                                          { dir = p
                                          , file = name}
                                 else name) ::
                                acc))
  in
    read_iter []
  end;

(* get_css_files : string -> string list

Produces a list of the absolute paths for all the .css files
appearing in the given directory.

The `css_dir` may be a relative path like "docs/css" or an
absolute path "/home/alex/src/website/docs/css", it doesn't
matter.
 *)
fun get_css_files css_dir =
    let
      fun is_css (name : string) =
          String.isSuffix ".css" (string_to_lower name);
    in
      List.filter is_css (list_dir_contents css_dir true)
    end;

(*
include_css_files : string_list -> string -> string

Produces the HTML snippet for including the CSS files in the
desired output file's path.

When there are no CSS files passed, it produces the empty string.
*)
fun include_css_files [] output_path = ""
  | include_css_files (css_files : string list) output_path =
    let
      val output_dir = OS.Path.dir output_path;
      val css_dir = OS.Path.dir (hd css_files);
      val relative_css_path = OS.Path.mkRelative
                                  { path = css_dir
                                  , relativeTo = output_dir
                                  };
      fun emit_link css_file =
          ("<link type=\"text/css\" " ^
           "rel=\"stylesheet\" "^
           "href=\""^(OS.Path.mkRelative
                          { path = css_file
                          , relativeTo = output_dir
                     })^"\" />");
    in
      String.concatWith "\n" (map emit_link css_files)
    end;

(*
partition_arguments : string ->
                      string list ->
                      (string list, string list)

Given the command line arguments and a flag which is supposed to
be followed by a parameter (like "--css dir"), partition the
arguments into `(flags, everything else)`.

The `rest` will remain in the order passed.

It will also handle cases like "--css=dir" or `<flag>=<value>`
correctly treating it as "-css dir" (respectively `<flag> <value>`).

Comma-separated values are supported provided there is NO
WHITESPACE separating them. So `val1,val2,val3` is OK, but
`val1, val2,    val3` is NOT.
*)
fun partition_arguments (flag : string) args =
    let
      fun get_val x =
          String.extract(x, String.size(flag)+1, NONE);
      fun add_val v coll =
          if String.isSubstring "," v
          then (map string_trim
                    (String.tokens (fn c => #"," = c) v)) @ coll
          else v::coll;
      fun opt_part_iter (flags,rest) [] = (flags, rev rest)
        | opt_part_iter (flags,rest) (x::[]) =
          if String.isPrefix (flag ^ "=") x
          then (add_val (get_val x) flags, rev rest)
          else (flags, rev (x::rest))
        | opt_part_iter (flags,rest) (x::y::xs) =
          (if flag = x
           then opt_part_iter (add_val y flags,rest) xs
           else if String.isPrefix (flag ^ "=") x
           then opt_part_iter (add_val (get_val x) flags, rest) (y::xs)
           else opt_part_iter (flags,x::rest) (y::xs))
    in
      opt_part_iter ([], []) args
    end;

val css_dir_args = partition_arguments "--css";

fun css_dirs_options (opts : string list) =
    css_dir_args opts;

fun recursive_opt (opts : string list) =
    partition_arguments "--r" opts;

(* output_opts : string list -> (string list, string list)

Returns an at most singleton in the first component.

If there are multiple "--output" options passed into the
command line arguments, then this will exit the program
immediately with failure.
 *)
fun output_opt (opts : string list) =
  case partition_arguments "--output" opts of
      (x::y::xs, rest) =>
          (print("ERROR: multiple outputs passed in\n");
           print("       whereas a maximum of 1 --output allowed.\n");
           print("BAILING OUT!\n");
           OS.Process.exit OS.Process.failure
          )
    | (out, rest) => (out, rest);

fun usage () =
 (print("\nmd --- converts a markdown file to HTML5\n\n");
  print("Optional arguments:\n\n");
  print("--css <dir>, --css=<dir> will include all the CSS files\n");
  print("    found in the directory. Accepts comma-separated\n");
  print("    values (with NO SPACES), or multiple appearances.\n");
  print("\n");
  print("--output <path>, --output=<path> will produce the HTML\n");
  print("    file(s) in the path\n");
  print("    NOTE: at most one --output option allowed\n");
  print("\n");
  print("--r <dir>, --r=<dir> will recursively build all HTML files\n");
  print("    for each .md file found in the directory, then move\n");
  print("    to all subdirectories. Accepts comma-separated\n");
  print("    values (with NO SPACES), or multiple appearances.\n");
  print("    \n");
  print("    NOTE: when combined with --output, this will create\n");
  print("    correspondingly named directories in the --output's dir\n");
  print("    (if needed).\n");
  print("\n");
  print("Example: to recursively build everything found in directories\n");
  print("'lab00', 'lab01', 'lab02' and produce output in corresponding\n");
  print("subdirectories of 'docs/', run:\n");
  print("    md --output docs --r lab00,lab01,lab02\n");
  print("\n"));

(*
generate_one : string list -> string -> string -> unit

Given a list of CSS files, an output directory, and a markdown
file, generate the HTML5 in the specified output directory with
links to all the given CSS files.
*)
fun generate_one css_files output_dir md_file =
    let
      val output_name = (output_file md_file output_dir);
      val css_files_elt = (include_css_files css_files output_name);
    in
      generate_html md_file
                    output_name
                    css_files_elt
    end;

(* Given a directory's absolute path `dir`,
and a new name for a new subdirectory `name`, produce
the subdirectory and return the absolute path to it. *)
fun mkdir_in dir name =
    let
      val p = OS.Path.fromUnixPath
                  ((OS.Path.toUnixPath dir) ^"/"^ name);
    in
      if OS.FileSys.access(p, [])
      then p
      else (print("Making directory "^p^"\n");
            OS.FileSys.mkDir p;
            p)
    end;

(*
recur_on_dir : string list -> string -> string -> unit

Working through the contents of `src_dirname`, generate an HTML5
file for each Markdown file found, recursively calling this
function when encountering subdirectories of `src_dirname` while
creating corresponding subdirectories in the `out_path`.

The `css_files` are expected to be the list of absolute paths to
CSS files.

The `out_path` and `src_dirname` may be relative or absolute
paths, we end up determining their absolute paths as the first
step and work with those anyways.
*)
fun recur_on_dir css_files out_path src_dirname =
  let
    val outdir = OS.FileSys.fullPath out_path;
    val abs_src = OS.FileSys.fullPath src_dirname;
    val contents = list_dir_contents abs_src false;
  in
    app (fn child =>
          let
            val file = (OS.Path.joinDirFile
                            { dir = abs_src
                            , file = child });
          in
            if OS.FileSys.isDir file
            then recur_on_dir css_files
                              (mkdir_in outdir child)
                              file
            else if is_md file
            then generate_one css_files outdir file
            else ()
          end)
        contents
  end;

(*
run : string list -> unit

Given a list of command line arguments, generate the appropriate
HTML5 output.
*)
fun run args =
  let
    val (css,r1) = css_dirs_options args;
    val css_files = List.concat (map get_css_files css);
    val (out,r2) = case output_opt r1 of
                       ([name],r2) => (name,r2)
                     | (_,r2) => ("./", r2);
    val (recursive,rest) = recursive_opt r2;
  in
    (if null recursive
     then ()
     else app (fn srcdir =>
                  let
                    val outdir = mkdir_in (OS.FileSys.fullPath out)
                                          (OS.Path.file srcdir);
                  in
                    recur_on_dir css_files outdir srcdir
                  end)
              recursive);
    (if null rest
     then ()
     else app (generate_one css_files out) rest)
  end;

fun main () =
  let
    val args = CommandLine.arguments();
  in
    if null args
    then usage()
    else run args;
    OS.Process.exit(OS.Process.success)
  end;
