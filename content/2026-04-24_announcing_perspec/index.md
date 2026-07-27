---
title: Announcing Perspec 1.0
author: Adrian Sieber
date: 2026-04-24
tags: [haskell, desktop, app, computer-vision]
draft: true
---

I'm very excited to announce the 1.0 release of [Perspec]!

Perspec is a desktop app to correct the perspective of images.
This is primarily useful for photos of documents and receipts,
but it can be used for any kind of image.

![Screenshot of Perspec](./screenshot.png)

This has finally become the app I envisioned
when I started working on the project 9 years ago.
I didn't think it would take me this long to get here,
but I'm very happy with the result and I hope you'll like it too!


## Initial Motivation

I guess you all know the scanner apps available for mobile phones like
[Adobe Scan](https://www.adobe.com/acrobat/mobile/scanner-app.html), [vFlat](https://www.vflat.com),
[SwiftScan](https://swiftscan.app/), … and numerous others.
Scanning functionality is also integrated into
[Dropbox](https://www.dropbox.com/features/productivity/document-scanner) and
natively into [iOS](https://support.apple.com/en-us/108963) itself by now.

However, I don't like working on my phone and I'd rather just take photos of the documents / receipts
and deal with cleaning them up and organizing them another day on my computer.
There I have a big screen, a keyboard, and a precise mouse,
which leads to faster and more accurate editing.

Also, the mobile apps make some annoying technical decisions
in the name of delivering the user something that's familar to them.

For example:
If you store a document as a grayscale PNG,
you can get small file sizes without introducing any compression artifacts.
However, all common apps will give you a grayscale JPEG image
with much bigger file sizes and worse image quality,
just because JPEG is what people are familiar with.

Or maybe I'm giving them too much credit
and they actually don't know that PNGs can
be smaller than JPEGs if the image contains a lot of continous areas with the same color.
Whereas for normal photos, JPEG is smaller than PNGs.
And no, converting it to PNG afterwards is not an option,
as the image then already contains all the JPEG compression artifacts.

For example, let's check the results when scanning following document:

<img
  alt="Photo of research paper lying on table"
  src="./paper_example.jpeg"
  style="max-width: 20rem;"
/>

The sizes are bigger and you can clearly see the compression artifacts
that lead to a worse result.

<table>
  <thead>
    <tr>
      <th>App</th>
      <th>Result</th>
      <th>Preview</th>
      <th>Notes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Perspec</td>
      <td>~110 kB, PNG</td>
      <td><img alt="Perspec result detail" src="result_perspec_detail.png"
        style="min-width: 15rem; image-rendering: pixelated;"></td>
      <td></td>
    </tr>
    <tr>
      <td>Scanner Pro</td>
      <td>~190 kB, JPEG</td>
      <td><img alt="Scanner Pro result detail" src="result_scanner_pro_detail.jpeg"
        style="min-width: 15rem; image-rendering: pixelated;"></td>
      <td>Extracted JPEG from exported PDF</td>
    </tr>
    <tr>
      <td>iOS</td>
      <td>~300 kB, JPEG</td>
      <td><img alt="iOS result detail" src="result_ios_detail.jpeg"
        style="min-width: 15rem; image-rendering: pixelated;"></td>
      <td>Extracted JPEG from exported PDF</td>
    </tr>
  </tbody>
</table>

Another thing that annoys me more than it should are the
ridiculous detection previews that seemingly all apps incorporate by now:

<table>
  <tr>
    <td><img alt="Screenshot Scanner Pro"
      src="screenshot_scanner_pro.png" style="max-height: 20rem;"></td>
    <td><img alt="Screenshot iOS"
      src="screenshot_ios.png" style="max-height: 20rem;"></td>
  </tr>
</table>

While taking a photo it will already show you an overlay of where it is detecting the document.
This, however, doesn't help you at all.
Just because it can detect the document correctly in the preview video feed,
doesn't mean it will detect it correctly in the final photo.
Due to the higher resolution, different lighting (exposure times, flash, …),
and different contrast,
the detection will often be quite different in the final photo.

So all the preview is telling you
is that there is indeed a document in front of your camera,
which you already know since you placed it there. 🤦‍♂️

Lastly and most importantly, I knew I could build
a better document detection algorithm for the kind of photos I was taking.
The detection of existing apps would often be slightly off,
even if you had a good picture
with good contrast between the document and the background.

Most apps use some kind of edge detection step in their pipeline.
Like Dropbox [explains here](https://dropbox.tech/machine-learning/fast-and-accurate-document-detection-for-scanning).
But I knew that documents and receipts often don't have straight edges
but rather wrinkled or curved ones.
When you try to match an even just slight curve with a straight line,
the endpoints will be quite off.
So instead, it should try to detect the corners and build up the document from there.
There is a detailed comparison of the computer vision techniques later in the post.


## The long road to 1.0

I was still a student when I started to work on Perspec
and had to scan a lot of stuff for my studies,
so I had a good motivation to build something like this.

My first iteration was a fully automatic CLI app called [Perspectra],
implemented with Python and [scikit-image].
You'd pass your image and it would try to detect and extract the document for you.
Simple as that.

Although I actually liked [scikit-image] --
feature rich, yet more straight forward than [OpenCV] --
I quickly realized that I absolutely do not like Python
and that I also needed a GUI to fix incorrectly detected document boundaries,
as the fully automatic CV pipeline would never get all documents 100% right.

TODO: you could also do it with Photoshop or GIMP but more overhead and not focused workflow

And how do you build a desktop app with a GUI?
Obviously with Haskell. 😝

So I started to work on the desktop app in parallel to trying to improve the CV pipeline.

As I didn't want to use Python any longer, my next instinct was to use ImageMagick,
as I had some experience with its features and capabilities.
The [existing Haskell bindings](https://hackage.haskell.org/packages/search?terms=magick)
were rather lacking, so I opted to simply call `magick` as an external process.
While this mostly worked, it was always a pain to get it to install
and link correctly across platforms,
and the performance was surprisingly bad for larger images.

Another obvious choice might be OpenCV, but I had some bad memories of using it at university
(maybe it was just the C++ context …), and the Haskell bindings looked rather painful.

So, my next experiment was using [Hip](https://github.com/lehins/hip).
With the help of the author [@lehins](/u/lehins) himself and [@HanStolpo](/u/hanstolpo)
we were able to make it work at ZuriHac! _(Thanks again!)_

However, it was still missing some features that I wanted, like binarization with Otsu's Method.
While it would certainly be possible to implement this in Hip,
I (for once) felt that Haskell's abstractions didn't really help with the task at hand
and only complicated things unnecessarily.
A for loop in C, by comparison, is conceptually very simple and already as fast as the Haskell code.
Luckily, C is a first-class citizen in Haskell
and it's very easy to bundle some C code and call it via Haskell's FFI.

Unfortunately, there didn't seem to be a straightforward C library
that I could hook up to Perspec,
so I started to work on [FlatCV] -
a pure C library for computer vision and image manipulation.

I might have overdone it with the yak shaving here,
but since the whole project is a labour of love anyways,
why not go all the way? 😅

I'm quite happy with the UX of using C for the image manipulation algorithms,
and I was able to quickly build a fully functioning version with the necessary Haskell bindings.
Just recently, I released [version 0.3.0](https://github.com/ad-si/FlatCV/releases/tag/v0.3.0),
and by now it has most of the basic operations you would expect from an image manipulation library.
I also ported some of the higher-level CV operations,
like [adaptive binarization](https://flatcv.ad-si.com/binarize.html#smart-black--white)
and [corner detection](https://flatcv.ad-si.com/corner-detection.html),
that I first implemented in [Perspectra].

There are still plenty of opportunities to improve the performance of FlatCV:
SIMD, GPU usage,
[streamed processing](https://github.com/libvips/libvips/wiki/Why-is-libvips-quick), etc.
However, as FlatCV isn't used in a real-time context (i.e., 60 fps),
the performance is already more than sufficient.

Hope you like it, and I'd be interested to know if you have any use cases for FlatCV!



## Next Steps

- support defining several regions
- output sizes (A4, letter, …)
- QR code detection
  - Inspired by https://blog.marcelrobitaille.me/receipt-ingestion/


---

[FlatCV]: https://github.com/ad-si/FlatCV
[OpenCV]: https://opencv.org
[Perspec]: https://github.com/ad-si/Perspec
[Perspectra]: https://github.com/ad-si/Perspectra
[scikit-image]: https://scikit-image.org/
