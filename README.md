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

The cached content files are currently built by shelling out to the [make][gnu-make] command. Thus, the app defines how to build the cached content files using a `Makefile` placed alongside the `routes.yaml` file. There is a silly test site defined under `_site`, specifically in [`_site/routes.yaml`][routes-example] and [`_site/Makefile`][makefile-example] that demonstrates this idea.

The ultimate goal is to replace the GNU Make dependency and, instead, use [Shake][shake] as the build tool. The plan is to extend the `routes.yaml` schema with a simple list of build rules (in fact, the [sample][routes-example] includes some of these already) which will be used to create Shake build rules. I do not want to allow the app's content itself to provide a Shake build script since I do not want to allow the user-provided content to run arbitrary commands on my server. Instead, the simple declarative rules in `routes.yaml` will be used to constrain what the build system can do.

I expect that I will also directly link to the [Pandoc][pandoc-hackage] libraries instead of having Shake shell out to it (which is what the [`Makefile`][makefile-example] currently does).

## How to run it

Build it:

```bash
stack build
```

Run the example site:

```bash
stack exec -- pansite-app --port 3000
```

In your web browser, navigate to a route defined in `routes.yaml`, e.g. http://localhost:3000/content/ctp.

Note that you'll need to have the `pandoc` executable on your system search path for the time being otherwise Pansite will fail at runtime. Once we embed Pandoc into the `pansite-app` executable directly, this will no longer be a requirement.

## Licence

Released under [MIT License][licence]

Copyright &copy; 2017 Richard Cook

[gnu-make]: https://www.gnu.org/software/make/
[licence]: LICENSE
[makefile-example]: _site/Makefile
[pandoc-hackage]: https://hackage.haskell.org/package/pandoc
[routes-example]: _site/routes.yaml
[shake]: http://shakebuild.com/
[stack]: https://haskellstack.org/
[warp-hackage]: https://hackage.haskell.org/package/warp
