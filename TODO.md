- [ ] Inline code should preserve whitespace. Right now,
      `     foo bar    ` and ` foo bar ` translated to HTML render
      indistinguishable results.
- [ ] Inline code, for Github pages, uses `` `code snippet`{:.language} ``
      for highlighting inline code. This seems fine, I should try to
      adhere to this spec.
      
      Pandoc uses `` `code snippet`{.language} `` --- perhaps I should try
      to support both?
      
      See:
      - https://kramdown.gettalong.org/syntax.html#span-ials
      - https://pandoc.org/MANUAL.html#verbatim
