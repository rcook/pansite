# Pansite: a simple web site management tool

[![Travis branch](https://img.shields.io/travis/rcook/pansite/master.svg)](https://travis-ci.org/rcook/pansite)
[![Hackage](https://img.shields.io/hackage/v/pansite.svg)](http://hackage.haskell.org/package/pansite)
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/rcook/pansite/master/LICENSE)

## Development

This project uses [Stack][stack], [Pandoc][pandoc], [Warp][warp] and [Shake][shake]

## Why are you doing this?

I want a [Hakyll][hakyll]-like static web site generator that can be easily used in conjunction with a dynamic site built with [Yesod][yesod]. I want it to use Pandoc so that I can build rich content, possibly with embedded mathematics. I also want it to be responsive, so that when I make changes to the underlying Markdown files, I can view the updated output in my browser quickly instead of having to wait thirty seconds for the Hakyll build to complete. There are probably tools that already do this (probably Hakyll itself can be made to do this), but I love reinventing the wheel. So, that's what I'm going to do.

The system is intended to be extensible in the future, so I will put some thought into how to extract its core functionality into a library.

## Work in progress

This project is a prototype and, therefore, _should not be used for any real work yet!_

## Current features

* Uses Pandoc to render Markdown into HTML (and, also, Microsoft Word documents)
* Supports static resources such as CSS
* Supports dynamic refresh of routes
* Shake-based build ensure that outputs are correctly maintained as long as dependencies are fully specified

## The vision

Currently Pansite is a trivial web app built on top of Warp. Routes are defined in a `.pansite.yaml` file using the following schema:

```yaml
# $(@D) is an automatic variable meaning "the output directory" (a la GNU Make)
# All other paths are resolved relative to this directory containing this file

routes:
- path: ""
  target: $(@D)/index.html
- path: page1
  target: $(@D)/page1.html
- path: page2
  target: $(@D)/page2.html
- path: css/buttondown.css
  target: buttondown.css

targets:
- path: $(@D)/index.html
  tool: pandoc
  tool-settings:
    number-sections: false
  inputs:
  - index.md
  dependencies:
  - .pansite.yaml
- path: $(@D)/page1.html
  tool: pandoc
  tool-settings:
    number-sections: false
  inputs:
  - page1.md
  dependencies:
  - .pansite.yaml
- path: $(@D)/page2.html
  tool: pandoc
  tool-settings:
    mathjax: true
  inputs:
  - page2.md
  dependencies:
  - .pansite.yaml

tool-settings:
  pandoc:
    number-sections: true
    template-path: template.html
    vars:
    - [css, css/buttondown.css]
```

Each `path` entry defines a route that the web app will respond to. The `target` key defines the cached content file to return in response to this route.

The cached content files are currently built using [Shake][shake] using rules generated from the `.pansite.yaml` file. Thus, the app itself defines how to build the cached content files using a simple declarative format. There is a silly test site defined under `_app`, specifically in [`_app/.pansite.yaml`][app-example] that demonstrates the idea. I do not want to allow the app's content itself to provide a Shake build script since I do not want to allow the user-provided content to run arbitrary commands on my server. Instead, the simple declarative rules in `.pansite.yaml` constrain what the build system can do while still keeping it useful.

Currently this prototype demonstrates the use of a single build tool, namely Pandoc. I intend to refactor the code to make it straightforward to specify additional build tools: some will be embedded directly, like Pandoc, others can use the Shake's [`cmd`][cmd-hackage] function to invoke external processes.

Build tools currently supported:

* Pandoc (`pandoc`): Process input files using Pandoc
* Copy (`copy`): Copy input file to output

## How to run it

Build it:

```bash
stack build
```

Run the example site:

```bash
cd _app/
stack exec -- pansite --port 3000
```

In your web browser, navigate to a route defined in `.pansite.yaml`, e.g. `http://localhost:3000/page2`.

## Command-line options

```terminal
$ stack exec -- pansite --help
Pansite development server 0.1.0.0.772a2d5 (locally modified)

Usage: pansite ([-p|--port PORT] [-c|--config CONFIG]
                   [-o|--output-dir OUTPUTDIR] | [-v|--version])
  Run Pansite development server

Available options:
  -h,--help                Show this help text
  -p,--port PORT           Port
  -c,--config CONFIG       Path to YAML application configuration file
  -o,--output-dir OUTPUTDIR
                           Output directory
  -v,--version             Show version
```

## Licence

Released under [MIT License][licence]

Copyright &copy; 2017&mdash;2018 Richard Cook

[app-example]: _app/.pansite.yaml
[cmd-hackage]: https://hackage.haskell.org/package/shake-0.15.11/docs/Development-Shake-Command.html
[gnu-make]: https://www.gnu.org/software/make/
[hakyll]: https://jaspervdj.be/hakyll/
[licence]: LICENSE
[pandoc]: http://pandoc.org/
[shake]: http://shakebuild.com/
[stack]: https://haskellstack.org/
[warp]: https://hackage.haskell.org/package/warp
[yesod]: http://www.yesodweb.com/
