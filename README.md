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
$ nix-shell --run yesod-devel
```
