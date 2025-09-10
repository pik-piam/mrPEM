# Prepare data to be used by the Political Economy Model (PEM)

R package **mrPEM**, version **0.1.0**

[![CRAN status](https://www.r-pkg.org/badges/version/mrPEM)](https://cran.r-project.org/package=mrPEM) [![R build status](https://github.com/pik-piam/mrPEM/workflows/check/badge.svg)](https://github.com/pik-piam/mrPEM/actions) [![codecov](https://codecov.io/gh/pik-piam/mrPEM/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrPEM) 

## Purpose and Functionality

Prepare data to be used by the Political Economy Model (PEM).


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrPEM")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Renato Rodrigues <renato.rodrigues@pik-potsdam.de>.

## Citation

To cite package **mrPEM** in publications use:

Rodrigues R, Kriegler E (2025). "mrPEM: Prepare data to be used by the Political Economy Model (PEM)." Version: 0.1.0, <https://github.com/pik-piam/mrPEM>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrPEM: Prepare data to be used by the Political Economy Model (PEM)},
  author = {Renato Rodrigues and Elmar Kriegler},
  date = {2025-09-10},
  year = {2025},
  url = {https://github.com/pik-piam/mrPEM},
  note = {Version: 0.1.0},
}
```
