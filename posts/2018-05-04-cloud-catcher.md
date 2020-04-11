---
title: "cloud-catcher: A web interface for ComputerCraft"
date: 2018-05-04 16:38:00
description: "Cloud-catcher is a web interface for ComputerCraft, which allows you to interact with remote computers through your browser."
tags: minecraft computercraft web
math: true
---

One of the more frustrating things about ComputerCraft, is when you try to run your program on a server. Your little
script which ran so perfectly in your local world, is now erroring left, right and centre. And this leaves you with no
alternative but to face the unending pain which is the in-game editor.

Surely there must be a better solution? Presenting, [Cloud Catcher][cloud]: A web interface for ComputerCraft

![Running `paint` within cloud-catcher.](/assets/img/posts/2018-05-04-run-paint.png)

cloud-catcher is a website which allows you to remotely interface with a computer, viewing the terminal and editing
files. Simply download the client script, pass in the provide token and lo! You should be able to interact with this
remote terminal just like any other computer. Well, mostly (see the fine print at the bottom).

That's not all though, enter `cloud edit cloud.lua`, and you should be greeted with an editor. You can then edit this
file to your heart's content: press Ctrl-S to save it, and mirror your changes back to the original computer.

One other rather neat feature is the ability to share sessions. On the top left, you can see your token: click it and
you'll be provided with a URL which can be shared with anybody else. Finally, the multiplayer notepad that everyone
wanted.

![Editing remote files](/assets/img/posts/2018-05-04-editor.png)

If you find a bug, have any suggestions, or just feel like helping out do [check out the repository][repo]. There's a
[whooping big todo list][todo] on the issue tracker, which might give you an idea of what's planned for the future.

Also a big thanks to 1lann and GravityScore for their work on Mimic. Cloud Catcher's renderer is heavily based off of
it.

[cloud]: https://cloud-catcher.squiddev.cc/ "The cloud-catcher website"
[repo]: https://github.com/SquidDev-CC/cloud-catcher "View the repository on GitHub"
[todo]: https://github.com/SquidDev-CC/cloud-catcher/issues/1 "cloud-catcher's todo list"
