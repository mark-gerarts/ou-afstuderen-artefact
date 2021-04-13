# ou-afstuderen-artefact

## A note on Prelude

TopHat has its own Prelude module which is for a part a reexport of
[Relude](https://github.com/kowainik/relude). It is possible to depend on TopHat
as a library and still use the plain old Prelude using
[mixins](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-mixins),
but unfortunately [this breaks IDE
integration](https://github.com/haskell/haskell-language-server/issues/1415).
The solution is to rely on Cabal instead of Stack in `hie.yaml`, but this
requires manually installing TopHat, since it is not available on Hackage.

Instead I've chosen to simply not use mixins, and to just embrace the Prelude of
Tophat/Relude. Since Relude uses Text, I've also added `OverloadedStrings` as a
global flag.

## Dev setup

### Building & running

Run the following once:

```bash
$ stack setup
$ stack build
```

To develop with hot reload enabled we use `yesod-bin`:

```bash
$ stack build yesod-bin
$ stack exec -- yesod devel
```

Or alternatively with nix:

```bash
$ nix-shell --run serve-backend
```

### Hoogle

Run a local Hoogle server:

```
$ nix-shell --run local-hoogle
```

The local Hoogle server can be found at
[http://localhost:8080](http://localhost:8080). Grab yourself a cup of coffee if
it's the first time you run this. Also, the command needs to be run again in
case of changes in code/dependencies.

### Frontend

Compile the CSS just once - we don't intent to make much changes for now.

```
$ npm run css-build-dev --prefix=frontend
$ # or
$ nix-shell --run css-build-dev
```

Run the frontend with hot reload:

```
$ nix-shell --run serve-ui
```

For general dev flow it is easier to run both Halogen with hot reload and
yesod-devel for the server. One way is to open up 2 terminal windows, or
alternatively:

```
$ nix-shell --run serve-both
```

The API will be available at  http://localhost:3000 and the frontend at
http://localhost:3001 (which will open automatically).

### VSCode

Run vscode as follows:

```
$ nix-shell --run 'code .'
```

Then set up a [multi-root
workspace](https://code.visualstudio.com/docs/editor/multi-root-workspaces) for
both the root project directory and the `frontend` directory. This is needed to
get the PureScript IDE extension to work correctly.

## Building a release

Work in progress. Currently you can do `nix-shell --run build-prod && nix-shell
--run 'stack run'`, which will launch the application at http://localhost:3000
with a production frontend build.
