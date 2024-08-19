A Markdown parser written in Standard ML. It emits valid HTML 5.

This isn't full Markdown, but a "common sense" subset of Markdown. The
supported fragment supports a lot, but not all, of Markdown (notably,
**not** lists, images, tables).

The goal is to use this as part of a larger tool for literate
programming where we are developing a _tower_ of metalanguages for doing
mathematics. 

# License

This is all written using the MIT License.

# Supported fragment

```
    _italics_
    **bold**
    _**italicized bold**_
    **_emboldened(?) italics_**
    [this is a link](url)
    `inline code`
    
    ```language
    code
    ```
    
    > block quote
    
    - unordered lists
    + alt unordered lists
    1. ordered lists
    
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

# Using this

The function you want to look at is in [html5.sml](src/html5.sml),
namely the `html5` function. It expects a string representation of the
Markdown as well as the header and footer elements you want to include,
then it will emit the HTML5 as a string.

See the [unit tests](./test/html5_test.sml) for example usage.

# See also

There are probably many other Markdown parsers which are more
optimal. But they don't fit the need we have (to be a part of a larger
toolkit for literate programs in a tower of metalanguages).

For example,
[ocsiblog](https://github.com/mfp/ocsiblog/blob/master/simple_markup.ml)
has a nifty parser in OCaml.