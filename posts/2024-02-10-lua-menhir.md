---
title: "Parsing Lua in Lua with OCaml"
date: 2024-02-10 01:00:00
description: "I port a Menhir parser to work in Lua"
tags: lua ocaml computercraft
---

As regular readers will no doubt be aware (and tired of me mentioning!), one of my main passion projects is [CC:
Tweaked](https://github.com/cc-tweaked/CC-Tweaked), a Minecraft mod which adds programmable computers to the game. While
most of my time is spent on general bug fixing and maintenance, at the end of 2022[^date] I sat down and put together a
list of features that I thought would help improve the user experience of the in-game interface.

[^date]: Yes, this is another very late blog post - I did the brunt of this work in early 2023.

One of improvements I really wanted to make was to error messages. Most of the people using the mod are beginners to
programming, and so I want to make the experience as friendly as possible.

There's several ways we could go about this. The obvious route would be to modify Lua's compiler, trying to improve the
quality of parse errors. However, there's a limit of how much we can do there - we have to preserve the API, which
prevents us from providing richer error messages[^scared].

[^scared]: I'm also a little scared by the Lua parser. It's a single pass parser, meaning it skips building an AST and
  emits bytecode directly. It's honestly pretty neat, but also makes it incredibly complex, and I'd like to not make it
  worse.

Instead, the approach I went for was to handle these errors inside our Lua code. When we get a syntax error from the Lua
compiler, we'll re-parse it using a custom parser and provide a friendlier error.

Of course, this now means we need to write our own parser[^recursive]. I've written several Lua parsers over the years,
the most recent being for [illuaminate], another project of mine. illuminate also had a problem with bad quality parser
errors, so this felt like a good way to kill two birds with one stone.

[illuaminate]: https://github.com/Squiddev/illuaminate

[^recursive]: I did actually get a significant way through writing a recursive descent parser before I got fed up.  I
  found the error-handling code I was writing was getting increasingly unwieldy. This is almost definitely a case of me
  doing something wrong, but got me looking for other options.

I've written a [more in-depth post][lrgrep post] into the illuaminate side of things. I'd really recommending reading
that first, if you've not already! In it, we use the [Menhir] parser generator to parse Lua, and then [lrgrep] to handle
errors.

[lrgrep post]: /2024/02/09/lrgrep.html
[menhir]: https://gallium.inria.fr/~fpottier/menhir/
[lrgrep]: https://github.com/let-def/lrgrep

Unfortunately, illuaminate is written in OCaml, and so all this work isn't especially useful if we want to use it from
Lua.

Or maybe it is? As you may recall, LR(1) parsers work by consulting a big lookup table and using those to decide whether
to push or pop items. It's worth noting that the code for doing this isn't actually generated by Menhir — it's been
hand-written beforehand. In fact, the lookup tables are the *only* thing that Menhir generates.[^actions]

[^actions]: Well, and the semantic actions (the user-provided code which actually builds the AST). As we just want to
  provide error messages, we can ignore those.

You can probably see where this is going now. We can use Menhir to generate our lookup table (Menhir actually has a
[really nice API to do this](https://gallium.inria.fr/~fpottier/menhir/manual.html#sec92)), and then write our own
generic LR(1) parser in Lua which uses these lookup tables.

The story is pretty similar for lrgrep. Instead of a lookup table, lrgrep generates a tiny bytecode, which is then
consumed by an OCaml interpreter. We can port this interpreter to Lua, and things Just Work(TM)!

![A screenshot of ComputerCraft. This reports the same error as mentioned in the lrgrep post, where the user has used
`=` instead of `==` (the full input is `if a = b then`). We highlight this error, and suggest using `==` to check if two
values are equal.](/assets/img/posts/2024-02-10-if-then.png)

I really wish I could make this blog post more exciting. I love these sorts of projects — ones which seem a little silly
("sure! let's convert OCaml to Lua"), but solve a genuine problem. Well, a problem I made up for myself at least. Alas,
the code in question is really quite simple, and there's only so much I can do about that!

If you want to check out the original code, it [is available on GitHub](https://github.com/SquidDev/lua_menhir). I
should warn you it is very bare-bones — it does enough for my use case, and nothing more. The parser doesn't even
support semantic actions!
