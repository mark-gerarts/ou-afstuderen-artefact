# ou-afstuderen-artefact

## Dev setup

Run the following once:

```
$ stack setup
$ stack build
```

To develop with hot reload enabled we use yesod-bin:

```
$ stack build yesod-bin # If you want hot reload, run this once
$ stack exec -- yesod devel
```

Or alternatively with nix:

```
$ nix-shell --run yesod-devel
```
