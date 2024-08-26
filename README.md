A Markdown parser written in Standard ML. It emits valid HTML 5.

This isn't full Markdown, but a "common sense" subset of Markdown. The
supported fragment supports a lot, but not all, of Markdown (notably,
**not** tables).

The goal is to use this as part of a larger tool for literate
programming where we are developing a _tower_ of metalanguages for doing
mathematics. 

I need to consider [bootstrappable best practices](https://bootstrappable.org/best-practices.html)

# License

This is all written using the MIT License.

# Supported fragment

```
    _italics_
    **bold**
    _**italicized bold**_
    **_emboldened(?) italics_**
    [this is a link](url)
    ![image description](url)
    `inline code`
    
    ```language
    code
    ```
    
    > block quote
    
    - unordered lists
    - item 2
    
    + alt unordered lists
    + item 2
    
    1. ordered lists
    2. item
    
    # Header 1
    ## Header 2
    ### Header 3
    #### Header 4
    ##### Header 5
    ###### Header 6
```

# Building this

Right now, this works with MLton and MLkit.

You could use [sml-buildscripts](https://github.com/cannam/sml-buildscripts)
to support Poly/ML.

Simply run:

```bash
md$ mlton md.mlb
md$ ./md

md --- converts a markdown file to HTML5

Optional arguments:

--css <dir>, --css=<dir> will include all the CSS files
    found in the directory. Accepts comma-separated
    values (with NO SPACES), or multiple appearances.

--output <path>, --output=<path> will produce the HTML
    file(s) in the path
    NOTE: at most one --output option allowed

--r <dir>, --r=<dir> will recursively build all HTML files
    for each .md file found in the directory, then move
    to all subdirectories. Accepts comma-separated
    values (with NO SPACES), or multiple appearances.
    
    NOTE: when combined with --output, this will create
    correspondingly named directories in the --output's dir
    (if needed).

Example: to recursively build everything found in directories
'lab00', 'lab01', 'lab02' and produce output in corresponding
subdirectories of 'docs/', run:
    md --output docs --r lab00,lab01,lab02
```

# Using this

The function you want to look at is in [html5.sml](src/html5.sml),
namely the `html5` function. It expects a string representation of the
Markdown as well as the header and footer elements you want to include,
then it will emit the HTML5 as a string.

See the [unit tests](./test/html5_test.sml) for example usage.

Right now, there's a [md.mlb](./md.mlb) file which will construct a `md`
binary. It will take the name of `input.md` files, then produce
`input.html` output files. (The output file is determined by checking if
the input is a name that ends with `.md` --- and then replacing it with
`.html`; otherwise, it just appends a `.html` to whatever the input name is.)

# Running the unit tests

This depends on [SML-Xunit](https://github.com/pqnelson/sml-xunit)
being in a sibling directory. If it's 
not, then just clone it, and modify line 3 of the [tests.mlb](./tests.mlb) file
to point to wherever `$SML_Xunit/xunit.mlb` is located.

# See also

There are probably many other Markdown parsers which are more
optimal. But they don't fit the need we have (to be a part of a larger
toolkit for literate programs in a tower of metalanguages).

For example,
[ocsiblog](https://github.com/mfp/ocsiblog/blob/master/simple_markup.ml)
has a nifty parser in OCaml.
