---
title: The Amulet backend
date: 2019-08-30 19:35
description: "An exploration of how Amulet's backend works"
tags: haskell amulet compilers lua
math: true
---

[Amulet][amulet] is a small little language [Amélia] and I have been working on for a wee while. It's
syntactically pretty similar to most MLs, albeit with a type system closer to Haskell's.

However, unlike most MLs or Haskell, Amulet is designed to compile to Lua - a lightweight, dynamically typed language
commonly used for embedded scripting. Sadly, this transformation from Amulet into Lua is rather complex, and despite my
well-meaning comments, the implementation is still quite opaque.

Any time I come to fix a bug, I'm still a little confuddled with what's _really_ going on, so I thought I'd put that to
rest and explain the internals of Amulet's codegen once and for all.

## Some background
Let's start with a little bit of background information about Amulet and the internals of the compiler. When using
Amulet, you read and write a relatively high level language, which we internally refer to as "Syntax". However, this
language is quite complex, and trying to optimise it ends up being incredibly difficult.

Instead, we convert all Amulet code into an intermediate representation referred to as "Core". This is a much simpler
and more stable language, which makes the optimiser easier to maintain.

```haskell
data Atom
  = Ref Var Type -- ^ x, y, z
  | Lit Literal -- ^ 0, true, "hello"

data Term
  = Atom Atom
  | App Atom Atom -- ^ f x
  | TyApp Atom Type -- ^ f {t} (Type application)
  | Lam Var Type Term -- ^ λ x, f x (Term and type application)
  | Let Binding Term -- ^ let x = y in z
  | Match Atom [(Pattern, Term)] -- ^ match x with | (y, Just z) -> ...
  | Values [Atom] -- ^ (| ... |) (Used to bundle multiple arguments together for uncurried functions.)

data Binding
  = One Var Term -- ^ Acyclic binding group
  | Many [(Var, Term)] -- ^ Mutually recursive binding
```

Core is, for all intents and purposes, in [A-normal form][anf]. Instead of having a deeply nested tree of expressions,
we have one long line of `let`s, each binding a single term. This is great for optimisation, as any intermediate
computation is bound to a variable.

Let's look at a small program, which just counts up from 1 to 5:

```ml
external val io_write : string -> unit = "io.write"
external val to_string : 'a -> string = "tostring"

let go n =
  if n > 5 then () else
  io_write(to_string n ^ "!\n")
  go (n + 1)

let () = go 1
```

We'll only focus on the `go` function for now, it's pretty simple to see how this will compile to Core:

```ml
let rec go : int -> unit = λ (n : int).
  let a : (| int, int |) = (| n, 5 |) in
  let b : bool = > a in
  match b {
   true -> unit;
   false ->
     let c : int -> string = to_string {int} in
     let d : string = c n in
     let e : (| string, string |) = (| d, "!\n" |) in
     let f : string = ^ e in
     let g : unit = io_write f in
     let h : (| int, int |) = (| n, 1 |) in
     let i : int = + h in
     go i
  }
```

It's a bit much to take in, but we've got pretty much what you'd expect - a long chain of `let`s, bundling variables into
our unboxed tuples, and then calling various functions.

The problem we're faced with, is how to compile this to Lua? A simple, but naive system would be a very literal
translation - each `let` becomes a `local` statement, `match` becomes an `if`-`elseif` chain. But that's clearly a
terrible idea - we'll end up with unnecessarily verbose code.

Ideally[^ideal], we'd end up with something almost identical to the original input:

```lua
local function go(n)
  if n > 5 then
    return nil
  end
  io_write(to_string(n) .. "!\n")
  return go(n + 1)
end
```

## A couple of musings
One interesting to note about the Amulet backend, is that it's not really _compiling_ source code[^compiling], but is
closer to a decompiler - taking a rather low-level[^low] IR and converting it back into readable source code. Now,
arguably the difference is moot. However, it turns out that a lot of research has been on how to decompile these sorts
of IRs into idiomatic code, and so we've now got a whole wealth of inspiration to draw from.

First of all, like many compiler problems, we'll start off with a graph. We want to convert every "leaf" term into a
node on our graph, with the edges representing dependencies between the nodes:


```{.dot title="Dependency graph for the `go` function."}
digraph G {
  rankdir = "BT";
  clusterrank = "local";
  ordering = "in";

  subgraph cluster_2 {
    label = "go";
    labelloc = "b";

    a[label="let a = (| n, 5 |)"];

    b[label="let b = > a"];
    b -> a[weight=2];

    x[label="match b with"];
    x -> b[weight=2,pos="0,0!"];

    za[label="()"];
    za -> x[style=dashed];

    zb[label="Match Arm"];
    zb -> x[style=dashed];
  }

  subgraph cluster_1 {
    label = "Match arm";
    labelloc = "b";

    c[label="let c = to_string {int}"];

    d[label="let d = c n"]
    d -> c[weight=2];

    e[label="let e = (| d, \"!\\n\")"];
    e -> d[weight=2];

    f[label="let f = ^ e"];
    f -> e[weight=2];

    g[label="let g = io_write f"];
    g -> f[weight=2];
    g -> d[style=dashed];

    h[label="let h = (| n, 1 |)"];
    h -> g[weight=2,style=invis,shape=none];

    i[label="let i = + h"];
    i -> h[weight=2];

    y[label="go i"];
    y -> i[weight=2];
    y -> g[style=dashed];
  }
}
```

In this image, we have two kinds of dependencies:

 - **Data dependencies:** Represented as a solid line, this represents when one variable is free in (or used) other
   term.

 - **Control-flow dependencies:** The dashed line states that one term's side effects must be executed before
   another's. Each impure term depends on the previous impure one, meaning we end up with a spine of dashed lines down
   through the program.

Within the actual compiler, both kinds of dependencies are treated the same.

The observant of you may have noticed that this graph is a directed acyclic graph. This is an important invariant to
preserve throughout the compilation process - we must _never_ introduce a loop into the graph. That'd mean an expression
depends on itself, which is clearly impossible!

## Handling expressions
Now we've got this graph, our approach is quite simple - we'll pick a node, and try to convert it into a Lua
expression. The translation rules are pretty much what you'd expect, but there's a couple of odd things going on:

 - _Technically_ we produce lists of Lua expressions. Most terms produce singleton lists, but unboxed tuples produce
   lists with the same arity as the tuple.

 - If we consume a variable which is used _exactly_ once, then we try to inline its definition. We'll use the Lua
   expression(s) immediately, and merge the variable's node into our own.

Let's step through how we'd translate the "Match arm" sub-graph in our above example. We'll just work from top to
bottom. Thankfully, we start off with something simple - type applications are entirely discarded.

```{.dot title="Translating a type application"}
digraph G {
  rankdir = "RL";
  clusterrank = "local";
  ordering = "in";

  c[label="let c = <to_string>"];

  d[label="let d = c n"]
  d -> c[weight=2];

  e[label="let e = (| d, \"!\\n\")"];
  e -> d[weight=2];

  f[label="let f = ^ e"];
  f -> e[weight=2];

  g[label="let g = io_write f"];
  g -> f[weight=2];
  g -> d[style=dashed];
}
```

`c` is only used once, so can be emitted inline in `d`. As mentioned earlier, this is an unboxed tuple, so we emit a
list of expressions.


```{.dot title="Merging expressions"}
digraph G {
  rankdir = "RL";
  clusterrank = "local";
  ordering = "in";

  d[label="let d = <to_string(n)>"]

  e[label="let e = (| d, \"!\\n\")"];
  e -> d[weight=2];

  f[label="let f = ^ e"];
  f -> e[weight=2];

  g[label="let g = io_write f"];
  g -> f[weight=2];
  g -> d[style=dashed];
}
```

Likewise, `d` can be merged into `e`. We need to be careful to replace any dependencies on `d`. In the actual
implementation, we just alias `d` and `e` within the graph.


```{.dot title="Merging expressions again, and rewriting dependencies."}
digraph G {
  rankdir = "RL";
  clusterrank = "local";
  ordering = "in";

  e[label="let e = <to_string(n), \"!\\n\">"];

  f[label="let f = ^ e"];
  f -> e[weight=2];

  g[label="let g = io_write f"];
  g -> f[weight=2];
  g -> e[style=dashed];
}
```

Let's continue this for the rest of the "Match arm" sub-graph. We'll finish off this expression, and also the call to
`go`.

```{.dot title="The whole of the `match` arm compiled to Lua"}
digraph G {
  rankdir = "BT";
  clusterrank = "local";
  ordering = "in";

  g[label="let g = <io_write(to_string(n) .. \"!\\n\")>"];

  y[label="let y = <go(i + 1)>"];
  y -> g[style=dashed];
}
```

We're sort of in the same place that we started off - we've a DAG of terms, with the dependencies between them. The
crucial difference is that this graph is entirely composed of Lua code. We can trivially convert this into a program by
performing a topological sort, and then stitching the statements together.

## Preserving the invariant
Consider the slightly more complex Amulet program:

```ml
let with_file fn name =
  let file = input_file name
  let result = fn file
  close_in file
  result
```

Let's convert this into a graph again.

```{.dot title="`with_file`'s body as a Core graph"}
digraph G {
  rankdir = "BT";
  clusterrank = "local";
  ordering = "in";

  file[label="let file = input_file name"];

  result[label="let result = fn file"];
  result -> file[style=dashed];
  result -> file;

  a[label="let a = close_in file"];
  a -> result[style=dashed];
  a -> file;

  b[label="result"];
  b -> result;
  b -> a[weight=2,style=dashed];
}
```

One mildly surprising part of this graph, is that our final `result` has a _control-flow_ dependency on `close_in
file`. As this term will be converted into a `return` statement, we need to ensure that every other term has executed
before this one.

The first couple of Lua transformations are pretty simple. We can't inline `file`, as it's is used multiple times, so we
convert it into a `local` binding instead:

```{.dot title="The initial translation of `with_file`"}
digraph G {
  rankdir = "BT";
  clusterrank = "local";
  ordering = "in";

  file[label="<local file = input_file(name)>"];

  result[label="let result = <fn(file)>"];
  result -> file[style=dashed];
  result -> file;

  a[label="let a = <close_in(file)>"];
  a -> result[style=dashed];
  a -> file;

  b[label="result"];
  b -> result;
  b -> a[weight=2,style=dashed];
}
```

Now, we only need to emit this final `result`. This should be pretty simple - it's only used once, so we can simply inline it.

```{.dot title="A malformed graph, due to merging `result`"}
digraph G {
  rankdir = "BT";
  clusterrank = "local";
  ordering = "in";

  file[label="<local file = input_file(name)>"];

  a[label="let a = <close_in(file)>"];
  a -> b[style=dashed];
  a -> file;

  b[label="<fn(file)>"];
  b -> a[weight=2,style=dashed];
  b -> file[style=dashed];
}
```

Sadly, things aren't this simple - our graph now has loops in it - we need to read from the file before closing it, and
so cannot move it to the return point. Instead, we fall back to the default behaviour of variables - binding them and
using it:

```{.dot title="The final graph, ready for emitting."}
digraph G {
  rankdir = "BT";
  clusterrank = "local";
  ordering = "in";

  file[label="<local file = input_file(name)>"];

  result[label="<local result = fn(file)>"];
  result -> file[style=dashed];
  result -> file;

  a[label="let a = <close_in(file)>"];
  a -> result[style=dashed];
  a -> file;

  b[label="<return result>"];
  b -> result;
  b -> a[weight=2,style=dashed];
}
```

## Handling control flow
So far, we've only discussed how to compile long chains of `let`s. How do we handle control flow - namely `match`?
Thankfully, the actual implementation is very simple:

 1. Bind the test to a variable.
 2. For each arm, treat the body as a fresh graph, and generate a sequence of statements as above.
 3. Convert each pattern to an expression, and generate an `if`-`elseif` chain.

Some additional complexity is introduced by trying to generate more idiomatic Lua, but most of it is just pattern
matching against common cases (`and`/`or`/`not` expressions, guard-`if`s, etc..).

One thing to consider, is how we treat the following code:

```ml
let x = if a then b else c
print x
```

It's pretty simple to see _what_ Lua code should be generated, but how do we get there?

```lua
local x
if a then x = b else x = c end
print(x)
```

Our solution is pretty simple: whenever we emit a graph, we provide "yield descriptor". This effectively
describes what we should do with the value that evaluating this term yields. There's four possible options:

 - **Return:** If we're emitting the body of a function, we clearly need to return the final value.
 - **Discard:** If we're on the right-hand-side of a `let` term, whose variable is never used, then we can just discard
   the value - only evaluating it for its side effects.
 - **Declare and Store:** These two are closely related. When we're a term on the RHS of a `let` term, we'll either
   declare the variable or just write to it, assuming it has already been declared.

When we come to generate a `match` term, we push through our yield descriptor to the child arms. For instance, if the
whole match expression is returned, this is equivalent to returning within each arm.

However, if we need to declare a variable, we can't go declaring within each arm - otherwise it won't be visible
outside! Instead, we pre-declare our variable, and then store inside each arm.

## Closing thoughts
Firstly, I must apologise. This article was very much written for the other developers of Amulet, in an attempt clarity
how the backend works. I'm not sure I've even succeeded there. Amulet is full of _theoretically_ well designed bits of
code, which in practice end up being very confusing, and even harder to maintain. That said, if you do have any
questions, don't hesitate to get [in contact](/#contact)!

There's clearly a lot of work still to be done on the codegen. One obvious missing feature is that self-tail-recursive
function are not compiled to loops. We're still discussing whether this should be done directly, or by augmenting Core
with join points or [CwCwW][cwc]'s $\mathscr{C}$ operator, but it should hopefully appear in the next five years.

[amulet]: https://github.com/amuletml/amulet/ "Amulet's GitHub repository"
[Amélia]: https://amelia.how/ "Amélia's personal website and blog. Plenty of Amulet stuff here too!"
[bug]: https://github.com/tmpim/amulet/issues/163 "Issue #163 on GitHub - Ordering can ignore IO in non-inlined cases"
[anf]: https://en.wikipedia.org/wiki/A-normal_form
[cwc]: https://www.cs.purdue.edu/homes/rompf/papers/cong-preprint201811.pdf "Compiling with Continuations, or without? Whatever."

[^ideal]: Well, somewhat ideal. We still don't convert tail-recursive functions into loops, and variable names are often
          terrible.
[^compiling]: Well, obviously it is. After all, we're still converting from one representation to another.
[^low]: Low-level in the same way Java bytecode is. We're not dealing with assembly here!

