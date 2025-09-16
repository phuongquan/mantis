# Contributing to mantis

## Reporting issues

Please report any bugs or suggestions by opening a [github issue](https://github.com/phuongquan/mantis/issues).

If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).
See the tidyverse guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.


## Development guidelines

If you'd like to contribute changes please raise an issue first, to make sure someone from the team agrees that it’s needed.

### Pull request process

* Fork the package and clone onto your computer.

* Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
    
* Create a Git branch for your changes, with a brief but relevant name.

* Make your changes, add/update any appropriate unit tests, update any documentation, and make sure R CMD check passes.

* For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header).

* Commit and push your changes, then create a PR. The title of your PR should briefly describe the change. The body of your PR should contain `Fixes #issue-number`.

### Code style

New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
We use [Air](https://posit-dev.github.io/air/) to apply this style, but please don't restyle code that has nothing to do with your PR.  

We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.


## Code of Conduct

Please note that this package is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). 
By contributing to this project, you agree to abide by its terms.
