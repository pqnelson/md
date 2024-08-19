fun output_file input_name =
    if String.isSuffix ".md" input_name
    then (String.substring(input_name,0,String.size(input_name)-2)^
          "html")
    else input_name^".html";

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
      print("Writing contents to "^output_name^"\n");
      TextIO.output(outStream, contents);
      TextIO.flushOut outStream;
      TextIO.closeOut outStream
    end;

fun generate_html input_name =
    let
      val s = html5 (read_file input_name) "" "";
      val output_name = (output_file input_name);
    in
      write_to_file output_name s
    end;

fun main () =
    let
      val args = CommandLine.arguments();
      val _ = app (fn s => print (s^"\n")) args;
    in
      print("Passed "^(Int.toString (length args))^" files\n");
      app generate_html args;
      OS.Process.exit(OS.Process.success)
    end;
