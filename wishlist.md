There are a lot of things we _could_ add to a Markdown engine.

Here are a few.

# Syntax highlighting

This is a bog. Don't get bogged down.

# Lists

Perhaps lists will be supported, perhaps not. One of the difficulties is
that it is mildly "context dependent". When writing multiple paragraphs
in a list, e.g.,

```
- item 1
- item 2 has multiple paragraphs.
  
  This second paragraph continues the second item.
  
  And this is a third paragraph, for revenge!
- item 3
```

We need to "remember" that when parsing the second item that "two
spaces" are needed to "continue" the item.

That requires a lot of state and work. Work that I would prefer to
avoid.

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