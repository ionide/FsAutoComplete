This directory contains mainly integration tests for the project.

To run the tests use `make integration-test` or `./fake
IntegrationTest` from the `FsAutoComplete` directory. This will run
`git status` as its last action. The tests are considered to have
passed if there are no changes to the various `*.json` files. If a
feature has been changed, then there may be acceptable changes --
think carefully and then commit them.

For absolute paths, there is some ad-hoc regex trickery to strip off
the beginning of the path.

## Dev guide

### How to attach to running fsautocomplete under test.

fsautocomplete has a `--wait-debugger` argument for wait at start, so is possibile to attach to process

In test suite, setting env var `FSAC_TESTSUITE_WAITDEBUGGER` to `1` **before** running the test enable that flag
