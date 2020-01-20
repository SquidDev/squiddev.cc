# [![](/assets/img/ico/favicon-32x32.png) squiddev.cc][squid]

This is my personal website, currently hosted at [squiddev.cc][squid] and
[joncoates.co.uk][jonathan].

This website is written generated using [Hakyll][hakyll], and can be built with
`stack run -- build`. It's only a statically generated site, so isn't especially
interesting, though there's some things which may be of value to a Hakyll user:

 - Compiling `.dot` files to `.png`.
 - Inline compiling of `dot` code blocks and LaTeX math to SVG.
 - Syntax highlighting for other languages. This is not currently used, but may
   be useful.
 - Draft posts.
 - Cache busters for asset files.
 - HTML minification. Of questionable usefulness, but a neat idea.

[squid]: https://squiddev.cc
[jonathan]: https://joncoates.co.uk
[hakyll]: https://jaspervdj.be/hakyll/
