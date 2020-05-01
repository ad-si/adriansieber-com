+++
title = "How I Replaced My iPhone with a Kindle 3"
draft = true

[taxonomies]
tags = ["iPhone", "Kindle"]
+++

Minimalism => Being device independent and handling only PDFs

What do I want from my mobile devices?


## Writing Emails

- Great keyboard


## Reading Ebooks

```shell
pandoc \
  --variable 'geometry:papersize={90mm,90mm},top=2mm,textwidth=86mm,textheight=86mm'
  --output alice_in_wonderland.pdf \
  alice_in_wonderland.epub
```

```shell
pandoc \
  --include-in-header header.tex \
  --variable 'geometry:papersize={85mm,85mm},top=2mm,textwidth=81mm,textheight=81mm' \
  --variable 'fontfamily:droidserif' \
  --variable 'fontfamilyoptions:default' \
  --variable 'linestretch:1.1' \
  --output alice_in_wonderland.pdf \
  alice_in_wonderland.epub
```

`header.tex`:

```tex
\usepackage{xcolor}
\color{darkgray}
```

```shell
pandoc \
  --include-in-header header.tex \
  --variable 'geometry:papersize={85mm,85mm},top=2mm,textwidth=81mm,textheight=81mm' \
  --variable 'fontfamily:roboto' \
  --variable 'fontfamilyoptions:rm' \
  --variable 'linestretch:1.1' \
  --output alice_roboto-slab.pdf \
  alice_in_wonderland.epub
```


pandoc \
  --variable 'geometry:papersize={85mm,85mm},top=2mm,textwidth=81mm,textheight=81mm' \
  --variable 'fontfamily:concrete' \
  --output alice_computer-concrete.pdf \
  pg11-images.epub

pandoc \
  --variable 'geometry:papersize={85mm,85mm},top=2mm,textwidth=81mm,textheight=81mm' \
  --variable 'fontfamily:roboto' \
  --variable 'fontfamilyoptions:rm' \
  --variable 'linestretch:1.1' \
  --output alice_roboto-slab.pdf \
  pg11-images.epub

pandoc \
  --variable 'geometry:papersize={85mm,85mm},top=2mm,textwidth=81mm,textheight=81mm' \
  --variable 'fontfamily:palatino' \
  --output palatino.pdf \
  pg11-images.epub

pandoc \
  --variable 'geometry:papersize={85mm,85mm},top=2mm,textwidth=81mm,textheight=81mm' \
  --variable 'fontfamily:Baskervaldx' \
  --output test.pdf \
  pg11-images.epub


## Kindle Paperwhite 3

```shell
pandoc \
  --variable 'geometry:papersize={90mm,122mm},top=5mm,right=5mm,bottom=5mm,left=5mm' \
  --output readme.pdf \
  readme.md
```

```shell
pandoc \
  --pdf-engine=wkhtmltopdf \
  --pdf-engine-opt='--lowquality' \
  --output 'book.pdf' \
  book.html
```


Alternative to keep syntax highlighting if Pandoc with LaTeX does not work.
B7 is a good approximation of the Kindle screen size.

```shell
wkhtmltopdf \
  --page-size B7 \
  --dpi 250 \
  --margin-top 2mm \
  --margin-right 2mm \
  --margin-bottom 2mm \
  --margin-left 2mm \
  --no-background \
  book.html \
  book.pdf
```


```shell
// TODO: Use native dpi after https://github.com/jgm/pandoc/issues/4142 is fixed
pandoc \
  --pdf-engine wkhtmltopdf \
  --pdf-engine-opt '--dpi' \
  --pdf-engine-opt '300' \
  --css ~/dotfiles/configs/pandoc/ebook-reader.css \
  --variable papersize:B7 \
  --variable margin-top:2mm \
  --variable margin-right:2mm \
  --variable margin-bottom:2mm \
  --variable margin-left:2mm \
  --output '2017-12-11 PureScript by Example.pdf' \
  https://leanpub.com/purescript/read

  --output example.pdf
  https://example.org
```


- Cropping PDFs => [briss](http://briss.sourceforge.net/)
- Splitting PDFs => [mupdf](https://mupdf.com/) `mutool poster -x 2 file.pdf`


## Shortcuts

Negative:

- No URL shortcuts, only app shortcuts


## NFC Tags


## QR Code Reader

