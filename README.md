
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nash

<!-- badges: start -->

[![R-CMD-check](https://github.com/ThomasDelSantoONeill/nash/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ThomasDelSantoONeill/nash/actions/workflows/R-CMD-check.yaml)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/github/languages/code-size/ThomasDelSantoONeill/nash.svg)](https://github.com/ThomasDelSantoONeill/nash)
<!-- badges: end -->

The goal of `nash` is to compute Nash equilibrium
(NE)<sup>[\[1\]](#1)</sup> harvesting rates for ecological models of the
form:

![\frac{d\mathbf{B}}{dt}=\mathbf{f}(\mathbf{B})\circ\mathbf{B}-\mathbf{F}\circ\mathbf{B},](https://latex.codecogs.com/png.latex?%5Cfrac%7Bd%5Cmathbf%7BB%7D%7D%7Bdt%7D%3D%5Cmathbf%7Bf%7D%28%5Cmathbf%7BB%7D%29%5Ccirc%5Cmathbf%7BB%7D-%5Cmathbf%7BF%7D%5Ccirc%5Cmathbf%7BB%7D%2C "\frac{d\mathbf{B}}{dt}=\mathbf{f}(\mathbf{B})\circ\mathbf{B}-\mathbf{F}\circ\mathbf{B},")

with
![\mathbf{B}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BB%7D "\mathbf{B}")
representing the non-negative biomass state vector (dimensions
![\text{MASS}](https://latex.codecogs.com/png.latex?%5Ctext%7BMASS%7D "\text{MASS}")),
![\mathbf{f}(\mathbf{B})](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bf%7D%28%5Cmathbf%7BB%7D%29 "\mathbf{f}(\mathbf{B})")
specifying the population growth (or decay) rate in the absence of
exploitation (dimensions
![1/\text{TIME}](https://latex.codecogs.com/png.latex?1%2F%5Ctext%7BTIME%7D "1/\text{TIME}"))
and ![\circ](https://latex.codecogs.com/png.latex?%5Ccirc "\circ")
denoting the entry-wise or
[Hadamard](https://en.wikipedia.org/wiki/Hadamard_product_(matrices))
product.

- (![\mathbf{F}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BF%7D "\mathbf{F}");
  dimensions
  ![1/\text{TIME}](https://latex.codecogs.com/png.latex?1%2F%5Ctext%7BTIME%7D "1/\text{TIME}"))
- Add Gustav’s comments and aknowledge him.
- Add nash logo

To run `nash`, the user is required to define an `R` function that
contains the above model alongside an integration routine to solve it;
that, for given
![\mathbf{F}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BF%7D "\mathbf{F}"),
the user-specified model outputs the yields at the stable equilibrium
![\mathbf{Y}=\mathbf{F}\circ\mathbf{B}^\*=\mathbf{F}\circ\mathbf{B}^\*(\mathbf{F})](https://latex.codecogs.com/png.latex?%5Cmathbf%7BY%7D%3D%5Cmathbf%7BF%7D%5Ccirc%5Cmathbf%7BB%7D%5E%2A%3D%5Cmathbf%7BF%7D%5Ccirc%5Cmathbf%7BB%7D%5E%2A%28%5Cmathbf%7BF%7D%29 "\mathbf{Y}=\mathbf{F}\circ\mathbf{B}^*=\mathbf{F}\circ\mathbf{B}^*(\mathbf{F})").
The `nash` function will then approximate the model near equilibrium
dynamics by a multispecies Lotka-Volterra (LV) model, for which the NE
can be computed analytically and so a first estimation of optimal
![\mathbf{F}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BF%7D "\mathbf{F}")
obtained. Subsequently, an updated LV approximation is calculated near
the equilibrium given by this new
![\mathbf{F}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BF%7D "\mathbf{F}").
`nash` will then re-compute the NE starting a new iteration until a
(user-adjustable) convergence threshold for
![\mathbf{F}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BF%7D "\mathbf{F}")
is reached.

## Installation

You can install the development version of `nash` either through the
`devtools`<sup>[\[2\]](#2)</sup> or `remotes`<sup>[\[3\]](#3)</sup>
packages:

``` r
# install.packages("devtools")
devtools::install_github("ThomasDelSantoONeill/nash")
```

## Assistance

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/ThomasDelSantoONeill/nash/issues). For
questions and other discussions/enhancements please email me at
[t.j.delsantooneill@qmul.ac.uk](t.j.delsantooneill@qmul.ac.uk).

## Minimal Example

## References

<a id="1">\[1\]</a> Nash, J. (1951). Non-cooperative games. <i>Annals of
Mathematics</i>, 54(2), 286–295 (<https://doi.org/10.2307/1969529>).

<a id="2">\[2\]</a> Wickham, H., Hester, J. and Chang, W. (2020).
<i>devtools: Tools to Make Developing R Packages Easier</i>. R package
version 2.3.1. (<https://github.com/r-lib/devtools>).

<a id="3">\[3\]</a> Hester, J., Csárdi, G., Wickham, H., Chang, W.,
Morgan, M. and Tenenbaum, D. (2020). <i>remotes: R Package Installation
from Remote Repositories, Including “GitHub”</i>. R package version
2.2.0. (<https://github.com/r-lib/remotes>).
