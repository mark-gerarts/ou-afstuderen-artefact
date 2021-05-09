# End-to-end tests

The test suite uses
[Playwright](https://github.com/microsoft/playwright) to provide browser
interaction, combined with [jest](https://jestjs.io/) as test runner.

## Installation & usage

The test suite is dockerized. In order to run it:


Build the container:

```
$ docker build . -t e2e-tests
```

Make sure the application is running (I guess it would be better to add it to
the container as well, but this is fine for now), so in a separate terminal:

```
$ cd ../ && stack run
```

Run the test suite. We add the `--net=host` so we can access the running
application on our host machine:

```
$ docker run --net=host --rm e2e-tests
```

Note that this way we run the application as-is. In a later stage, we will have
a few applications in the `app/` directory, which we can use to run specific
test cases.
