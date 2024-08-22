# LaTeX support
- [ ] Need to skip `$...$`, `$$...$$`, `\(...\)`, `\[...\]` from being
      parsed in Markdown as anything other than plain text.
      
      One first stab might be: if we encounter a `$`, check if the next
      character is also a `$`, then find the next occurrence of `$$` and
      advance the parser after it (which would then make it bold,
      italicized, or whatever).
- [ ] If `$`, `\(`, `\)`, `\[`, `\]`, etc., occur within a code or `pre`
      block, then we should escape it --- change `$` to `&dollar;`,
      change `\(` to `&bsol;&lpar;`,
      `\)` to `&bsol;&rpar;`,
      `\[` to `&bsol;&lbrack;`, `\]` to `&bsol;&rbrack;`,
      etc.
- [ ] Research katex versus mathjax.
      - KaTeX [autorender](https://katex.org/docs/autorender)

# Titles
- [ ] Support "metadata" like Jekyll, Hakyl, etc.? This would be
      indicated by starting with a line `---` followed by YAML-like data,
      terminated by a standalone line whose contents is only `---`
      
      We could just ignore all other metadata for now, and just use the
      "title" (if given).
- [ ] The `<title>` needs to be populated with...something!
      If we do not use Jekyll-like metadata, then should we just use the
      first `<h1>` contents?

# MINOR: Avoid needless substrings
- [ ] A lot of the parsing module works with invoking `String.extract`
      and `String.substring`, which generates a lot of substrings and
      uses a lot of memory.
      
      Measure the memory usage of building a lot
      of Markdown files to see if this is actually a problem or not...
- [ ] This might be fine, but consider using 
      [`SUBSTRING`](https://smlfamily.github.io/Basis/substring.html)
      module instead?
- [ ] Consider a `StringBuilder` module? This would have an array of
      characters, a `length` integer keeping track of how many entries
      have been used so far, a `capacity` integer tracking how many
      entries the array has, and when it needs more...it just allocates
      a new array (with twice the capacity), copies the old array, and
      keeps on working. We extract a string from it once the dust has
      settled using `MONO_ARRAY.copy` (to "shrink to fit"), then
      `MONO_ARRAY.vector` (to generate the vector). 