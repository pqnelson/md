# Major

## Features

### Reachability Convenience Macros

- [ ] Have a `[[footer]]` macro which will expand to the appropriate
      links; this is the `[back] [up] [next]` links at the bottom of
      most pages which are not `index.md` pages.
      - The links can be given as YAML frontmatter `next: file-name.md`,
        `back: file-name.md`, `up: file-name.md`
- [ ] Have the `title: name` YAML frontmatter autogenerate a leading
      `# name` in the article body
- [ ] Support a `[[project:toc]]` which will generate the links to
      files in the order suggested by the frontmatter `next` linkages.
- [ ] Consider having a `[[dir:tree]]` macro which will generate the
      directory tree for `index.md` pages, recursively using the
      `[[dir:tree]]` calculated from subdirectory `index.md` pages.
      - This, together with the previous few features, would
        automatically guarantee that the pages generated would be
        reachable. 
      - A script double checking that every `.md` file has a `next: ...`
        (and/or is the target of a `next: ...`) fronmatter, or is an
        `index.md` file, would suffice for checking the files are
        linked and reachable

# Minor

## Bugs
- [ ] Inline code should preserve whitespace. Right now,
      `     foo bar    ` and ` foo bar ` translated to HTML render
      indistinguishable results.

## Features
- [ ] Inline code, for Github pages, uses `` `code snippet`{:.language} ``
      for highlighting inline code. This seems fine, I should try to
      adhere to this spec.
      
      Pandoc uses `` `code snippet`{.language} `` --- perhaps I should try
      to support both?
      
      See:
      - https://kramdown.gettalong.org/syntax.html#span-ials
      - https://pandoc.org/MANUAL.html#verbatim
