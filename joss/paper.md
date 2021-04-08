---
title: 'gm: R Package for Creating Music'
tags:
  - R
  - musical score
  - algorithmic composition
  - music programming
authors:
  - name: Renfei Mao
    orcid: 0000-0002-2841-9570
    affiliation: 1
affiliations:
  - name: Independent Researcher
    index: 1
date: 8 April 2021
bibliography: paper.bib
---

# Summary

`gm` is an R package for creating music and generating musical scores and
audio files. It provides a simple and intuitive high-level language which
makes it easy to describe music. It converts the language to musical scores
and audio files, without users considering the technical details of musical 
notation. It works and embeds generated music in R Markdown documents, R
Jupyter Notebooks and RStudio. The potential uses of `gm` include algorithmic
composition, music programming, communication of musicological research
results and teaching music theory.


# Statement of need

For R users familiar with R package `ggplot2` [@ggplot2] and R Markdown
[@rmarkdown], `gm` is like "gmplot" and "R Musicdown". These two analogies
also echo with two interpretations of the name "gm", which are "grammar of
music" and "generate music". Accordingly, `gm` has two main features:

First, it provides a high-level language which makes it easy to describe music
structures without being overwhelmed by music notation details. Functions in
`gm` takes R's basic data structures like vector and list as inputs, which
makes it easy to create music programmatically. The language has a grammar
a bit similar to `ggplot2`'s, so users can create music interactively by
repeatedly adding components and checking the resulted music.

Second, `gm` works in R Markdown documents, R Jupyter Notebooks [@rjupyter]
and RStudio [@rstudio]. It automatically embeds generated music in generated
document files, which makes `gm` a good tool for reproducible research 
[@goodman_what_2016] and music theory pedagogy.

Additionally, `gm` uses [MusicXML](https://www.w3.org/2017/12/musicxml31/)
to represent musical scores internally and calls 
[MuseScore](https://musescore.org/) to convert MusicXML to other file formats.
Users can export created music to MusicXML files and open them in MuseScore or
other notation software for fine tuning the scores.


# Examples

The following examples demonstrate the basic use of `gm`.

Load `gm`:

```r
library(gm)
```

Initialize an empty `Music` object:

```r
m <- Music()
```

Add a 4/4 time signature and a musical line:

```r
m <- m +
  Meter(4, 4) +
  Line(pitches = list("C5"), durations = list("whole"))
```

Print `m` to get a brief description of it:

```r
m

#> Music
#> 
#> Line 1
#> 
#> * as part 1 staff 1 voice 1
#> * of length 1
#> * of pitch C5
#> * of duration 4
#> 
#> Meter 4/4
```

Convert `m` to musical score and audio file:
 
```r
show(m, to = c("score", "audio"))
```
 
![An example `gm` figure demonstrating conversion of created music to score.](assets/score.png)
 
![An example `gm` figure demonstrating conversion of created music to audio file.](assets/audio.png)

Add a tempo mark:

```r
m <- m + Tempo(120)
show(m)
``` 

![An example `gm` figure demonstrating adding tempo mark.](assets/tempo.png)

Add another musical line:

```r
m <- m + Line(
  pitches = list("C3", "G3"),
  durations = list("half", "half")
)

show(m)
```

![An example `gm` figure demonstrating adding musical line.](assets/line.png)

Check [Complete Guide to gm](https://flujoo.github.io/gm/articles/gm.html)
for more examples.


# Comparison

At the time of writing, the author knows only `tabr` [@tabr] that has similar
purposes and similar level of functionality to `gm`, among other R packages.

However, there are some noticeable differences:

- Besides music generation, `tabr` also provides functions for music analysis
and manipulation.
- `tabr` can generate guitar tabs and add more music notation symbols to
score than `gm` can. `gm` is not yet for professional level music notation.
- `gm` embeds generated music in R Markdown documents and R Jupyter Notebooks,
while `tabr` exports it.
- `gm` uses R's basic data structures to represent music, while `tabr` uses
strings.
- `gm`'s language is simpler than `tabr`'s, it is more straightforward
to add components such as musical lines in `gm`. This and the above difference
make `gm` more suitable for music programming and algorithmic composition.

In Python, `music21` [@music21] is a mature library which provides abundant
tools for music generation and analysis. For its music generation 
functionality, `music21` can generate more professional musical scores than
`gm` can, and also works in Python Jupyter Notebooks. However, at its core,
`gm`'s language is far simpler and more intuitive. For example, in `music21`, 

- it's cumbersome to define notes and add them to musical
lines,
- users have to be concerned about musical score structures when they only
want to express music structures at a higher level,
- creation of complex and nested tuplets is not straightforward,
- ...

In summary, `gm` is a good tool for expression of high-level music structures
and communication of generated music. It's not yet for professional level
music notation.


# Acknowledgement

To my wife, without whom there would be no this package.


# References
