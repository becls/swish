# Contributing to Swish

Swish is a work in progress. We invite contributions from anyone who is
interested in putting forth the necessary effort. Before investing significant
effort preparing a contribution, consider running the idea by one of the
maintainers for additional guidance and advice. One or more of the maintainers
will review pull requests for compatibility with the principles and guidance
given below (adapted from [Chez Scheme](https://github.com/cisco/ChezScheme/blob/master/CONTRIBUTING.md)).

Our core principles are simple: we try to make Swish reliable and efficient.

Reliability means behaving as designed and documented. A Swish program may crash
due to bugs in the program, but it should not crash due to bugs in Swish.

Efficiency means performing at a high level, without excessive use of CPU time
and memory. Performance should scale well as problem size grows and should be
balanced across features, not good in one area and bad in another.

We attempt to achieve the core principles through careful control over growth,
testing, and documentation.

* When asked to add a new feature, we first look for a way to achieve the same
effect with existing functionality or smaller extensions that are more generally
applicable.

* Swish includes a suite of manual tests and automated tests that are run on each
pull request and each push to the dev branch. When a contribution changes code,
be sure to add or update the automated tests.

* A feature is incomplete until documented. Writing documentation often
exposes unnecessary complexity in the design and bugs in the implementation,
particularly in corner cases. New features should be documented in the Swish
[design document](https://becls.github.io/swish/swish.pdf).


Consistent with these principles, we naturally want Swish to
evolve in various useful ways to, among other things:

* increase reliability and efficiency
* increase utility
* improve user friendliness
* run on new platforms

Please keep in mind the following guidance when preparing contributions:

* Include appropriate tests and documentation with all code changes.

* Follow the coding structure (including indentation) of the existing
  code base. This implies that contributors should study the existing code
  before contributing.

* Spend the time required to make the code as clean, clear, and
  efficient as possible.  All other things equal, shorter code is
  preferable to longer code. 

* Describe changes in the [ChangeLog](ChangeLog.md) and in the git commit
  messages and GitHub pull request logs.  The revision-control
  system might change over time, but the ChangeLog should endure.

* Some contributions may be more appropriately published as projects
  of their own.  If you are contributing a significant extension built
  using Swish, consider whether your contribution is such an
  independent project.

* Intellectual Property Policy
  * All contributions to this project shall be made under the
    [MIT License](LICENSE).
  * By submitting a contribution, a Contributor certifies that
    the Contributor is the sole creator of the contribution and/or
    has the right under all applicable intellectual property laws
    to provide the contribution to the Project under the terms of
    the MIT License.
