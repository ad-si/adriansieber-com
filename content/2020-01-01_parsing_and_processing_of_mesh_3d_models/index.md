+++
title = "Parsing and Processing of Mesh-based 3D Models"
draft = true

[taxonomies]
tags = ["3D", "mesh"]
+++

# {{ page.title }}

## Introduction

Users of Brickify get started by uploading
their 3D models into the webapp.
This process consists of six steps:

1. Loading the file
1. Parsing the file and converting it into a native data-structure
1. Optimizing the data-structure for increased performance
  and decreased storage requirements
1. Automatic alignment
  1. Rotating the model to align it to the grid imposed by the bricks
  1. Centering the model in the xy-plane
  1. Aligning the model-surface and the brick surfaces


Our goal was to archive a performant and visually pleasing experience
for the user, despite the limitations of browser-environments.

To accomplish these goals we leveraged low-level language
constructs and developed novel and elaborate algorithms.
These algorithms and techniques are described in more detail
in the following sections.


## Loading

### Streamed vs. Buffered Loading

The first step, in the sequence of steps from the original
file to our internal optimized data-structure, is the loading process.

A precondition to reading user specified files in webapps
is the so called File API, which was introduced in HTML5.
All mayor browsers have been supporting this feature since spring 2012.
[http://caniuse.com/#search=file%20api]

Traditionally, a working file-upload component consists
of an upload button which prompts the user to select files
and a script which handles those specified.

The minimal implementation looks like this:

```html
<!doctype html>
<title>Minimal file upload example</title>
<input type=file id=input>
<script>
  document
    .getElementById('input')
    .addEventListener('change', function () {
      var file = this.files[0],
        reader = new FileReader()

      reader.onload = function () {
        // File content is now available in reader.result
        // and can be processed
      }

      reader.readAsArrayBuffer(file)
    })
</script>
```

The user triggers the input button, which open a popup window.
In this window the user can select the files to upload
and start the loading-process.
The script then starts the upload and waits for the upload-process
to terminate so that it's able to use the data.

This standard implementation, however, has a mayor issue.

During the time the script waits for the file-loading process
to terminate, processor cycles are wasted by doing nothing.

Buffered Workflow

```
Loading     | ####
Parsing     |     #######
Optimizing  |            ###
Processing  |               ##########
Displaying  |                         ##
```


Streamed Workflow

```
Loading     | #     #    #
Parsing     |  ##    ##   ##
Optimizing  |    #     #    #
Processing  |     ###  ###   ###
Displaying  |        #    #     #
```





## Parsing

## Optimization of data-structures

## Automatic Rotation of Models

## Centering of Models

## Aligniment of Model-surface to Brick-surfaces
