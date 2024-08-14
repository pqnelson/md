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

