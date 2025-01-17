---
title: "How to write a Minecraft Gradle plugin - Part 1: Downloading Minecraft"
date: 2024-01-26 21:23:00
description: "I continue on my never-ending quest to reinvent the wheel, and this time bring you along for the ride."
tags: minecraft java gradle
---

I love writing build tooling. It's a bit silly really, but there's something immensely satisfying about spending ages
fighting whatever build system I'm using, fitting together all sorts of weird code, to end up with something which saves
me 30 seconds once a month.

One build system I end up fighting a lot of the time is Gradle. Gradle is one of the biggest Java build tools out there
(definitely the main one for Minecraft modding) and I have a bit of a love/hate relationship with it. It's an incredibly
powerful tool, and when it works it's incredible. It's just also very easy to bump into its rough edges.

Many of these rough edges are exposed within the various Minecraft Gradle plugins, such as ForgeGradle or Fabric's
Loom. These projects often need to jump through weird hoops to make everything work together.


I often look at these
projects and go "why didn't you just do xxx?".
