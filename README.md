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
    *italics*
    _italics_
    **bold**
    __bold__
    ***italicized bold***
    [this is a link](url)
    `inline code`
    <!-- comment -->
    
    ```language
    code
    ```
    
    > block quote
    
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

# See also

There are probably many other Markdown parsers which are more
optimal. But they don't fit the need we have (to be a part of a larger
toolkit for literate programs in a tower of metalanguages).

For example,
[ocsiblog](https://github.com/mfp/ocsiblog/blob/master/simple_markup.ml)
has a nifty parser in OCaml.