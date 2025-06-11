+++
title = "GSoC 2025 Week #1"
date = "2025-06-08"

[taxonomies]
tags=["scala","metals","gsoc"]
+++

## Introduction

This is going to be _(hopefully)_ a weekly series of blog posts covering the technical details of the stuff implemented by me and my wonderful mentor [@tgodik](https://github.com/tgodzik) for Scala Center GSOC 2025. This will be the first post in the series and I'll cover what has been done so far in the first week of the coding period.

The first week has a bit of a special circumstance since even though the coding period began in June 2nd of this year, I asked my mentor if we could start early and have an extra month for free 😅.

In hindsight that was the best thing I could've done since that extra time was proved to be really helpful and was put to good use.

## Stuff Accomplished in the first week

Okay, so far me and my mentor have managed to get a couple of stuff done. The high level steps that needed to be done are as follows:

1. Get client/editor and metals to recognize `.scala.html` files.
2. Set up mapping for goto/hover etc

For the first point, detecting `.scala.html` files in vscode via adding an entry in the [metals-vscode's](https://github.com/scalameta/metals-vscode) `package.json` was really straightforward

```json
  {
    "id": "scala",
    "extensions": [
      ".scala.html"
    ]
  }
```

Another thing we needed to do was allow metals itself recognize twirl's files, we did this via adding the following snippet

```scala
def isTwirlTemplate: Boolean = filename.endsWith(".scala.html")
```

in some key locations inside [CommonMtagsEnrichments.scala](https://github.com/scalameta/metals/blob/main/mtags-shared/src/main/scala/scala/meta/internal/mtags/CommonMtagsEnrichments.scala) and in [ScalametaCommonEnrichments.scala](https://github.com/ajafri2001/metals/blob/main/mtags/src/main/scala/scala/meta/internal/mtags/ScalametaCommonEnrichments.scala). This makes it so we can easily have logic specific to twirl templates in metals itself.

{{ note(header="Key Point!", body="It is important to note that twirl templates are compiled to scala source code at compile time, by default under `target/scala-x.x.x/twirl/main/html/*.template.scala`") }}

Next we needed to add support for compiling twirl files via the [sbt-twirl](https://github.com/playframework/twirl/tree/main/sbt-twirl/src) plugin, sbt-twirl expects twirl files to be under `src/main/twirl/*.scala.html` this is still being done and hopefully it gets resolved in week 2 😅.

To implement LSP specific stuff like hover/goto-definition, we need a way to "map" between twirl templates and the scala source code it compiles down to. Thankfully, the compiled source files have additional generated content that is appended at the end of the file and it is really helpful for mapping, and an example would be here: 

```txt
/*
  -- GENERATED --
  SOURCE: src/main/twirl/example1.scala.html
  HASH: 65c3b28a0faadb5f545e21fb821007050023d27d
  MATRIX: 601->1|709->16|746->27|770->31
  LINES: 15->1|20->2|20->2|20->2
  -- GENERATED --
 */

```

As you can see in the "Lines" row here, this implies that line 1 of the twirl file maps to line 15 of the compiled scala file, now whenever the the client request information for twirl file, we can simply give them this line number. We aren't taking into account character number here, but that's alright. We can do it next week or the next week after it :)

Okay, now to extract this information out of the compiled file, we can use regex to do something like this, which sorta works.

```scala
val pattern = """(\d+)->(\d+)""".r
val number_matching =
pattern.findAllIn(templateString).toList

// Use latter half of matched regex; first half corresponds to the "Matrix" row
val numbers = number_matching.drop(number_matching.length / 2)

// Map[Int, Int](1 -> 15, 2 -> 20, 3 -> 21)  SourceFile -> CompiledFile
val mappingMap: Map[Int, Int] = numbers.map { s =>
val parts = s.split("->")
val a = parts(0).toInt
val b = parts(1).toInt
b -> a
}.toMap
```

## Next Week Goals

My goal for the next week is, to have thorough tests on the mapping and achieve reasonable sbt integration for metals, and that basic stuff like hover works.
