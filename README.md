# Pansite: a simple web site management tool

## Development

This project uses [Stack][stack]

## Why are you doing this?

I want a [Hakyll][hakyll]-like static web site generator that can be easily used in conjunction with a dynamic site built with [Yesod][yesod]. I want it to use Pandoc so that I can build rich content, possibly with embedded mathematics. I also want it to be responsive, so that when I make changes to the underlying Markdown files, I can view the updated output in my browser quickly instead of having to wait thirty seconds for the Hakyll build to complete. There are probably tools that already do this (probably Hakyll itself can be made to do this), but I love reinventing the wheel. So, that's what I'm going to do.

## Work in progress

This project is a prototype and, therefore, _should not be used for any real work yet!_

## Current features

* Uses Pandoc to render Markdown into HTML
* Supports static resources such as CSS
* Supports dynamic refresh of routes
* Shake-based build ensure that outputs are correctly maintained as long as dependencies are fully specified

## The vision

Currently Pansite is a trivial web app built on top of [Warp][warp-hackage]. Routes are defined in a `app.yaml` file using the following schema:

```yaml
# $(@D) is an automatic variable meaning "the output directory" (a la GNU Make)
# All other paths are resolved relative to this directory containing this file

routes:
- path: content/ctp
  target: $(@D)/ctp.html
- path: content/pcph
  target: $(@D)/pcph.html
- path: css/buttondown.css
  target: buttondown.css

targets:
- path: $(@D)/ctp.html
  tool: pandoc
  tool-settings:
    number-sections: false
  inputs:
  - ctp.md
  dependencies:
  - app.yaml
- path: $(@D)/pcph.html
  tool: pandoc
  inputs:
  - pcph.md
  dependencies:
  - app.yaml

tool-settings:
  pandoc:
    number-sections: true
    template-path: template.html
    vars:
    - [css, css/buttondown.css]
```

Each `path` entry defines a route that the web app will respond to. The `target` key defines the cached content file to return in response to this route.

The cached content files are currently built using [Shake][shake] using rules generated from the `app.yaml` file. Thus, the app itself defines how to build the cached content files using a simple declarative format. There is a silly test site defined under `_app`, specifically in [`_app/app.yaml`][app-example] that demonstrates the idea. I do not want to allow the app's content itself to provide a Shake build script since I do not want to allow the user-provided content to run arbitrary commands on my server. Instead, the simple declarative rules in `app.yaml` constrain what the build system can do while still keeping it useful.

Currently this prototype demonstrates the use of a single build tool, namely [Pandoc][pandoc-hackage]. I intend to refactor the code to make it straightforward to specify additional build tools: some will be embedded directly, like Pandoc, others can use the Shake's [`cmd`][cmd-hackage] function to invoke external processes.

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
stack exec -- pansite-app --port 3000
```

In your web browser, navigate to a route defined in `app.yaml`, e.g. `http://localhost:3000/content/ctp`.

## Command-line options

```terminal
$ stack exec -- pansite-app --help
Pansite development server

Usage: pansite-app [-p|--port PORT] [-c|--config CONFIG]
                   [-o|--output-dir OUTPUTDIR]
  Run Pansite development server

Available options:
  -h,--help                Show this help text
  -p,--port PORT           port
  -c,--config CONFIG       path to YAML application configuration file
  -o,--output-dir OUTPUTDIR
                           output directory
```

## Licence

Released under [MIT License][licence]

Copyright &copy; 2017 Richard Cook

[app-example]: _app/app.yaml
[cmd-hackage]: https://hackage.haskell.org/package/shake-0.15.11/docs/Development-Shake-Command.html
[gnu-make]: https://www.gnu.org/software/make/
[hakyll]: https://jaspervdj.be/hakyll/
[licence]: LICENSE
[pandoc-hackage]: https://hackage.haskell.org/package/pandoc
[shake]: http://shakebuild.com/
[stack]: https://haskellstack.org/
[warp-hackage]: https://hackage.haskell.org/package/warp
[yesod]: http://www.yesodweb.com/
