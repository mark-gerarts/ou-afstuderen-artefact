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
$ npm ci --prefix=e2e
$ npm run test --prefix=e2e
```

## Developing tests

What works easiest for me is:

First, start a local server for the page you're testing:

```
$ stack runghc e2e/task/StringUpdate.hs
```

This allows you to check what is going on, and play around with the selectors
etc that you're using in the tests.

Then run the test suite either directly or using docker, as explained above.

To create a new test suite:

- Create a new test suite: `e2e/src/your-new-test.e2e.js`
- Create a new task to test: `e2e/task/YourNewTest.hs`
- Add a new entry to spin up a server in `e2e/jest-playwright.config.js`
  - Make sure you use a unique port number
  - Use this port in your test suite

The following links are useful when writing tests:

- Interacting with the browser: [Playwright docs](https://playwright.dev/docs/intro)
- All available matchers (`expect(..)`): [Jest docs](https://jestjs.io/docs/expect)
- Additional common matchers: [expect-playwright](https://github.com/playwright-community/expect-playwright#expect-playwright)
