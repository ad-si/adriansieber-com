+++
title = "Getting Started with GraphViz on Mac"
draft = true

[taxonomies]
tags = ["GraphViz", "macOS"]
+++

# {{page.title}}

- lately draw some graphs
- lot of things weren't clear

## Installation

`brew install graphviz --with-librsvg --with-pango`

for svg output and html style labels

> In addition, all of these markups are currently only available via the cairo and svg renderers. - http://www.graphviz.org/doc/info/shapes.html#html

## Language


## Usage

See default renderer:

`dot -Tpng:`


`dot -Tpng diagram.gv > test.png && open test.png`
