Testo Contribution Guidelines
==

Testo is developed in the open by the OCaml community for the OCaml community.

This document is a collection of guides to facilitate contributions to the project.

Meta Goals
--

We want contributions to be easy for outsiders who are not familiar with
the internals of Testo. We also want contributions to be easy to
review and to approve by the project admins.

One-off contributions
--

We encourage first-time contributions
but before making complicated changes, it's best to [open an
issue](https://github.com/semgrep/testo/issues) in which you present
your plans. For simple bugfixes and enhancements, you don't have to file
an issue first but it's generally a good idea.

Build the project with `make`. Test it with `make test`. Beware that
the tests can be a little confusing because they use Testo on its
own output.

When making a pull request (PR), a check list will remind you of a few
essential things that need to be taken care of before the PR can be
reviewed and merged into the main branch. We have CI checks in place. Make
sure that they pass.

If you would like to make an official release, notify a project
admin. They will follow the instructions below.

Release instructions for admins
--

The release process involves assigning a
[version ID](https://semver.org/), tagging a git commit with this
version ID, building an archive, and publishing the opam packages that
use this archive.
[dune-release](https://github.com/ocamllabs/dune-release) makes this
process easy and safe. Refer to its documentation for more information.

Note that:
* We run the release steps directly on the main branch. We could
  resort to creating a branch if pushing to the main branch was
  restricted or if there was significant material to review.
* The point of no return is `dune-release publish`. If there's a
  failure after that, the release ID should be incremented and all the
  steps should be followed again.

1. Run `make opam-files` to make sure the opam files are up-to-date.
2. Review and update the changelog `CHANGES.md`.
3. Create a section with the desired version e.g. `2.3.0
   (2022-03-10)`.
4. Commit the changes.
5. Install [dune-release](https://github.com/ocamllabs/dune-release)
   if not already installed:
   `opam install dune-release`
6. Run `make opam-release` or run the individual steps below by hand:
   * Run `dune-release tag`. It will pick up the version from the
     changelog and ask for confirmation.
   * Run `dune-release distrib` to create a tarball.
   * Run `dune-release publish --draft` to upload the tarball to GitHub and
     create GitHub release including the changes extracted from the
     changelog.
   * Create opam packages with `dune-release opam pkg`.
   * Submit the opam packages to opam-repository using
     `dune-release opam submit`.
7. Fix the opam-repository pull request as needed. For example, this
   may require setting a new version constraint on the `atd` package
   in the opam files, if it wasn't possible to do so in
   `dune-project`.
8. Check whether opam-repository's CI test succeed and fix problems
   accordingly until the pull request is merged.

Shortcut for all the `dune-release` steps:
```
$ make opam-release
```

### Documentation setup

We maintain a generated API reference for the development version of Testo.
The HTML files must be updated by running `make doc`. GitHub takes
care of serving the pages.

The documentation is (or will be, eventually) divided into the 4 kinds
so as to separate concerns and make it easier to write
as [advocated by Daniele Procida](https://diataxis.fr/):

- reference
- tutorials
- how-tos
- (deep understanding)

The sources for the documentation live in `docsrc`. After running
`make doc`, the website can be previewed directly by opening
`docs/index.ml` in a web browser.
