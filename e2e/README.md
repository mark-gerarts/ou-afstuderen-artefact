# End-to-end tests

The test suite uses
[Playwright](https://github.com/microsoft/playwright) to provide browser
interaction, combined with [jest](https://jestjs.io/) as test runner.

## Installation & usage

The test suite is dockerized, so we could (hypothetically for now) include it in
CI/CD. In order to run it:


Build the container:

```
$ docker build . -f Dockerfile.e2e -t e2e-tests
```

Note: this takes a while to run for the first time, because:

- We start from the stack-build image, which is large (~4GB)
- We install playwright, which comes with Chromium included
- We have to install and compile all Haskell dependencies

Run the test suite:

```
$ docker run --rm e2e-tests
```

With nix shell, both commands are combined in `nix-shell --run e2e`.

## The non-docker way

Since the dockerized version basically duplicates our local development setup,
you could just install the test setup locally, which would be faster:

```
$ npm install --prefix=e2e
$ npm run test --prefix=e2e
```
