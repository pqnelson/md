There are a lot of things we _could_ add to a Markdown engine.

Here are a few.

# Titles

Right now, there is no way to generate the `<title>` tag in the
header. I should probably support this.

Either use the first `<h1>` tag's contents, or I'd need to support a
Jekyll-like header.

# Syntax highlighting

This is a bog. Don't get bogged down.

The trick is to tokenize the code blocks, then highlight each reserved
word appropriately.

HOWEVER, the way this engine is designed, we need to **also** escape any
characters which could accidentally capture HTML tags (i.e., replace `<`
with `&lt;`, `>` with `&gt;`, and so on).

Right now, the code blocks are _just_ strings with the language name, so
ostensibly we could relegate this to another program or library.

However, I am trying to also link identifiers to their definition, and
create anchors for new definitions. Since I'm working mostly in Standard
ML, this will be simple.

## Literate Programming

"Eating your own dog food", I should rewrite this entire program as a
literate program which can create its own source code, and its own
documentation. 

I need to think about "code chunks" some more. WEB-like literate
programming use "code chunks", the term for "codefenced blocks" which
are extracted as source code. **But** there is an optional _name_ for
the code chunk, typically written in angled brackets `<<Example code
chunk name>>=` (which is pretty printed in TeX, or HTML, or..., as
`⟨Example code chunk name⟩≡...`. Later, we can "insert" these code chunk
names, writing code which looks like pseudocode, for example:

```sml
⟨Methods for the Foobar data structure⟩=

fun process_inputs input =
    if ⟨invalid input⟩
    then ⟨handle error⟩
    else ⟨default case⟩;
```

This can be frustrating to debug for compiler errors, since the "tangled
code" (i.e., the code extracted from this) has different line numbers
than the literate WEB program.

There is also a problem where we may want "local" name chunks which are
available within a WEB file, but not outside the file. I think the way
around this is to have an extension, where we write `<<<Local name chunk>>>`
with three angled brackets.

## Metadata for code fences

I may want to highlight syntax code, but not create any anchors for
identifiers. Why? Well, think about giving example usage of functions,
this is different than sourcecode which is part of a program.

I think the current consensus is to use `{...}` for key-value pairs,
something like:

    ```language {key-1=val-1, ..., key-N = val-N}
    code code code
    code code code
    code code code
    ```

Perhaps this is do-able.

# Alerts

Github markdown supports alerts of the form:

```
> [!NOTE]
> Useful information that users should know, even when skimming content.

> [!TIP]
> Helpful advice for doing things better or more easily.

> [!IMPORTANT]
> Key information users need to know to achieve their goal.

> [!WARNING]
> Urgent info that needs immediate user attention to avoid problems.

> [!CAUTION]
> Advises about risks or negative outcomes of certain actions.
```

It seems like this could be extended to support arbitrary "environments"
like theorems, remarks, corollaries, etc.