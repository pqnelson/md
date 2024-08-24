- [ ] Need to make sure that TeX literals do not accidentally capture
      the end of a bold italic text. Although I cannot imagine anyone
      writing `**` in TeX, that is not the same as it being
      impossible.
      
      It is far more likely that someone would write something like
      `Einstein did not prove $E=mc^{2}$ but _instead proved $E^{2}c^{2}=\eta^{\mu\nu}p_{\mu}p_{\nu}$ which is far more useful!_`
      I guess this should be rendered as:
      `Einstein did not prove $E=mc^{2}$ but <em>instead proved $E^{2}c^{2}=\eta^{\mu\nu}p_{\mu}p_{\nu}$ which is far more useful!</em>`
      or something similar.
- [X] Need to trim `*` suffix on programming language names when
      transforming code fenced blocks --- these are supposed to be
      "example usages" (or more precisely, a "do not include this"
      signifier to the Markdown engine).

# Titles
- [X] Support "metadata" like Jekyll, Hakyl, etc.? This would be
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
