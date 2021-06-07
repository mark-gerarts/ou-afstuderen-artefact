# Development setup

## One-time commands

Run the following once to fetch and build all Haskell dependencies. All commands
are intended to be run in the project root.

```console
$ stack setup
$ stack build
```

Next, fetch all frontend dependencies:

```console
$ npm ci --prefix=frontend
```

Compile the CSS:

```console
$ npm run css-build-dev --prefix=frontend
```

## Nix

Most commands have alternatives for [Nix](https://nixos.org/)-users. For
example:

```console
$ nix-shell --run css-build-dev
```

Or enter the shell and run your commands from there:

```console
$ nix-shell
$ css-build-dev
```

To get a complete overview, take a look at the `shell.nix` file.

## Development flow

### Backend

To run the backend with hot reload enabled we use `yesod-bin`:

```console
$ stack build yesod-bin # Only needed once
$ stack exec -- yesod devel
```

Or alternatively with nix:

```console
$ nix-shell --run serve-backend
```

This will spin up a server at [localhost:3000](http://localhost:3000) and
automatically rebuild the Haskell part of the application whenever a file is
changed under the `src/` directory. Manual building is still needed when making
changes in the `app/` folder.

Note: `yesod devel` uses `app/devel.hs` as its entrypoint, not `app/Main.hs`.

Sometimes `yesod devel` borks, and does not build upon first starting the dev
server. If this happens, simply change and save a project file to force a
rebuild.

### Frontend

With the backend server up and running, open up a separate terminal and run the
following to get the frontend development environment up:

```console
$ spago build
$ npm run serve
```

Alternatively:

```console
$ nix-shell --run serve-ui
```

This uses [Parcel](https://parceljs.org/) to provide live reloading of the
frontend. It will automatically open a browser at
[localhost:3001](http://localhost:3001).

When the frontend development environment is started this way, it will assume
the backend is available at port 3000 - which it should be when using `yesod devel`.

Spinning up both the backend and frontend is combined in a single Nix alias:

```console
$ nix-shell --run serve-both
```

### VSCode

Set up a [multi-root
workspace](https://code.visualstudio.com/docs/editor/multi-root-workspaces) for
both the root project directory and the `frontend` directory. This is needed to
get the PureScript IDE extension to work correctly.

For Nix users, keep in mind to start VSCode from the nix-shell environment:

```console
$ nix-shell --run 'code .'
```

### Hoogle

It's possible to generate a local Hoogle server for the project:

```console
$ stack test --fast --haddock-deps
$ hoogle generate --local=./
$ hoogle server --local --port=8080
```

With nix:

```console
$ nix-shell --run local-hoogle
```

The local Hoogle server can be found at
[http://localhost:8080](http://localhost:8080). Grab yourself a cup of coffee if
it's the first time you run this. Also, the command needs to be run again in
case of changes in code/dependencies.

## Building a release

Building a release (i.e. a non-dev environment) involves:

- Building the CSS
- Compiling the PureScript application
- Building the Haskell executable

```console
$ npm run build-prod --prefix=frontend
$ stack build
```

With this in place you can use `stack run` to start the application at
[http://localhost:3000](http://localhost:3000). To use a different port, set the
`PORT` environment variable:

```console
$ PORT=80 stack run
```

## A note on Prelude

TopHat has its own Prelude module which is for a part a reexport of
[Relude](https://github.com/kowainik/relude). It is possible to depend on TopHat
as a library and still use the plain old Prelude using
[mixins](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-mixins),
but unfortunately [this breaks IDE
integration](https://github.com/haskell/haskell-language-server/issues/1415).
The solution is to rely on Cabal instead of Stack in `hie.yaml`, but this
requires manually installing TopHat, since it is not available on Hackage.

Instead we've chosen to simply not use mixins, and to just embrace the Prelude
of Tophat/Relude. Since Relude uses Text, we've also added `OverloadedStrings` as
a global flag.
