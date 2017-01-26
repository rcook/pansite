# Pansite: a simple web site management tool

## Development

This project uses [Stack][stack]

## Work in progress

This project is a prototype and, therefore, _should not be used for any real work yet!_

## The vision

Currently Pansite is a trivial web app built on top of [Warp][warp-hackage]. Routes are defined in a `routes.yaml` file using the following schema:

```yaml
routes:
- path: content/page0
  target: page0.html
- path: content/page1
  target: page1.html

targets:
- path: page0.html
  build-tool: pandoc
  dependencies:
  - page0.md
- path: page1.html
  build-tool: pandoc
  dependencies:
  - page1.md
```

Each `path` entry defines a route that the web app will respond to. The `target` key defines the cached content file to return in response to this route.

The cached content files are currently built using [Shake][shake] using rules generated from the `routes.yaml` file. Thus, the app itself defines how to build the cached content files using a simple declarative format. There is a silly test site defined under `_app`, specifically in [`_app/routes.yaml`][routes-example] that demonstrates the idea. I do not want to allow the app's content itself to provide a Shake build script since I do not want to allow the user-provided content to run arbitrary commands on my server. Instead, the simple declarative rules in `routes.yaml` constrain what the build system can do while still keeping it useful.

Currently this prototype demonstrates the use of a single build tool, namely [Pandoc][pandoc-hackage]. I intend to refactor the code to make it straightforward to specify additional build tools: some will be embedded directly, like Pandoc, others can use the Shake's [`cmd`][cmd-hackage] function to invoke external processes.

## How to run it

Build it:

```bash
stack build
```

Run the example site:

```bash
stack exec -- pansite-app --port 3000
```

In your web browser, navigate to a route defined in `routes.yaml`, e.g. `http://localhost:3000/content/ctp`.

## Licence

Released under [MIT License][licence]

Copyright &copy; 2017 Richard Cook

[cmd-hackage]: https://hackage.haskell.org/package/shake-0.15.11/docs/Development-Shake-Command.html
[gnu-make]: https://www.gnu.org/software/make/
[licence]: LICENSE
[pandoc-hackage]: https://hackage.haskell.org/package/pandoc
[routes-example]: _app/routes.yaml
[shake]: http://shakebuild.com/
[stack]: https://haskellstack.org/
[warp-hackage]: https://hackage.haskell.org/package/warp
