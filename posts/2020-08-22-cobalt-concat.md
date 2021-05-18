---
title: Optimising Lua's string concatenation
date: 2020-05-08 16:42:00
description: A writeup of the recent work to optimise monitor rendering.
tags: cobalt lua compilers
math: true
---

One of my more practical projects is [Cobalt], a Java implementation of the Lua 5.1 virtual machine. I'm often working
on it, improving the code base [or bringing it closer to PUC Lua][tweaking]. One recent improvement I've made is with
_string concatenation_.

Before we get into the weeds, let's talk about how strings are implemented in Lua (both PUC[^1] and Cobalt). While
there's some small optimisations[^2], in essence strings are just a byte array. This makes concatenation very easy - you
just make a new array and copy the two across:

```java
byte[] concatenate(byte[] a, byte[] b) {
  byte[] out = new byte[a.length + b.length];
  System.arraycopy(a, 0, out, 0, a.length);
  System.arraycopy(b, 0, out, a.length, b.length);
  return out;
}
```

Apologies for the Java, but it seemed the easiest to get the point across. Now, it's not hard to see that concatenating
two strings of length $m$ and $n$ takes $O(m + n)$ time. 

This naive approach is a little problematic when performing multiple concatenations though - `a .. b .. c` would first
compute `b .. c`[^3], and then `a .. $temp`. Thankfully Lua isn't that silly - it detects these chains of concatenations
and builds the final string in one sweep.

However, it's not quite as rosy when working with loops. For instance, let's write a function which converts a table to
a string:

```lua
local function display(tbl)
  local out = "{ "
  for i = 1, #tbl do
    out = out .. tostring(tbl[i]) .. ", "
  end
  return out .. " }"
end
```

Lua can't see that all these concatenations can be merged into one, and so this code ends up being running in $O(n^2)$ -
each iteration of the loop needs to allocate a new string of size $O(n)$. Doing this with a table of size $1 \times
10^5$ takes 13 seconds on my machine, $2 \times 10^5$ takes 111 seconds.

The common solution to this is simple - append these substrings to a table, and get use `table.concat` to build a final
result at the end. This is standard practice in Java or C#, and is incredibly effective - the same $2 \times 10^5$ table
takes a little over a second to `display`.

However, if you were to write our initial naive code in JavaScript, you wouldn't see any issues - it runs in a fraction
of a second. While some of that may be the JIT being incredibly smart, there's also some clever usage of data
structures.

The insight here is that we don't actually _need_ to allocate a full byte array right now. We'll need to at some point,
when we come to operate on the string, but that's only the final result. Instead, let's represent our intermediate
string as a [_Rope_][rope].

Ropes are effectively just a binary tree, with the leaves containing the strings which make up the full rope. As
concatenation is just building a new tree from two smaller ones, it's simple. And, more importantly, fast - it runs in
$O(1)$.

```java
class StringLike {}
class String extends StringLike { byte[] contents; }
class Rope extends StringLike { StringLike left, right; }

Rope concat(StringLike left, StringLike right) {
  return new Rope(left, right);
}
```

Now, building an byte array from a rope still takes $O(n)$ time, but it (generally) only needs to happen once for the
final string, rather than every iteration of the loop.

After [applying this optimisation to Cobalt][patch], our above examples now run in under a second. The code is now
linear, so tables of size $5 \times 10^6$ are still relatively speedy - taking 19 seconds. That said, there's still room
for improvement - `table.concat` is still faster, albeit no longer quite as dramatically.

[Cobalt]: https://github.com/SquidDev/Cobalt "Cobalt's GitHub repository."
[tweaking]: /2019/03/08/tweaking-cc-tweaked.html "Tweaking CC: Tweaked"
[puc]: https://www.lua.org/ "Lua's website."
[patch]: https://github.com/SquidDev/Cobalt/pull/44 "The relevent PR"
[rope]: https://en.wikipedia.org/wiki/Rope_(data_structure) "Ropes on Wikipedia"

[^1]: I'm using PUC Lua to distinguish the [standard Lua distribution][puc] compared with alternative implementations like
    LuaJIT, Rembulan or even Cobalt.
[^2]: For instance, many Lua implementations cache the string's hash.
[^3]: String concatenation is right-associative in Lua. We'll get onto that in a moment.
