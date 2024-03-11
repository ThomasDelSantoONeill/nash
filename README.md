
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/nashlogo.svg" style='width: 100%; object-fit: contain'/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ThomasDelSantoONeill/nash/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ThomasDelSantoONeill/nash/actions/workflows/R-CMD-check.yaml)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/github/languages/code-size/ThomasDelSantoONeill/nash.svg)](https://github.com/ThomasDelSantoONeill/nash)
<!-- badges: end -->

# Quick Start

The goal of `nash` is to compute Nash ([1951](#ref-Nash1951))
Equilibrium (NE) harvesting rates for naturally occurring species that
**biologically interact** through *e.g.* predation and/or competition.
[NE](https://en.wikipedia.org/wiki/Nash_equilibrium) harvesting rates
mean *to exploit each species in a wild mixture at such rates that no
deviation from this rate can increase the long-term yield from that
species* ([Farcas and Rossberg 2016](#ref-Farcas2016)).

The algorithms implemented in `nash` assume that **users have developed
an ecosystem model** with $S$ harvested compartments, typically
represented by population biomass variables. In the **simplest case**,
population dynamics are given by a system of autonomous ordinary
differential equations of the general form:

$$\frac{d\mathbf{B}}{dt}=\mathbf{f}(\mathbf{B})\circ\mathbf{B}-\mathbf{F}\circ\mathbf{B},$$

with $\mathbf{B}$ representing the non-negative biomass or state vector
(dimensions $\text{MASS}$), $\mathbf{f}(\mathbf{B})$ specifying the
population growth (or decay) rate in the absence of exploitation
(dimensions $1/\text{TIME}$) and the last term describing removal
(*e.g.* harvesting and/or culling) at a rate $\mathbf{F}$ (dimensions
$1/\text{TIME}$). In addition, $\circ$ denotes the entry-wise or
[Hadamard](https://en.wikipedia.org/wiki/Hadamard_product_(matrices))
product.

- Add Gustav’s comments and aknowledge him.

To run `nash`, the user is required to define an `R` function that
contains the above model alongside an integration routine to solve it;
that, for given $\mathbf{F}$, the user-specified model outputs the
yields at the stable equilibrium
$\mathbf{Y}=\mathbf{F}\circ\mathbf{B}^*=\mathbf{F}\circ\mathbf{B}^*(\mathbf{F})$.
The `nash` function will then approximate the model near equilibrium
dynamics by a multispecies Lotka-Volterra (LV) model, for which the NE
can be computed analytically and so a first estimation of optimal
$\mathbf{F}$ obtained. Subsequently, an updated LV approximation is
calculated near the equilibrium given by this new $\mathbf{F}$. `nash`
will then re-compute the NE starting a new iteration until a
(user-adjustable) convergence threshold for $\mathbf{F}$ is reached.

## Installation

You can install the development version of `nash` either through the
`devtools` ([Wickham et al. 2022](#ref-devtools2022)) or `remotes`
([Csárdi et al. 2023](#ref-remotes2023)) packages:

``` r
# install.packages("devtools")
devtools::install_github("ThomasDelSantoONeill/nash")
```

## Minimal Example

## Assistance

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/ThomasDelSantoONeill/nash/issues). For
questions and other discussions/enhancements please email me at
[t.j.delsantooneill@qmul.ac.uk](t.j.delsantooneill@qmul.ac.uk).

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-remotes2023" class="csl-entry">

Csárdi, Gábor, Jim Hester, Hadley Wickham, Winston Chang, Martin Morgan,
and Dan Tenenbaum. 2023. *Remotes: R Package Installation from Remote
Repositories, Including “GitHub”*.
<https://CRAN.R-project.org/package=remotes>.

</div>

<div id="ref-Farcas2016" class="csl-entry">

Farcas, Adrian, and Axel G. Rossberg. 2016. “Maximum Sustainable Yield
from Interacting Fish Stocks in an Uncertain World: Two Policy Choices
and Underlying Trade-Offs.” *ICES Journal of Marine Science* 73 (10):
2499–2508. <https://doi.org/10.1093/icesjms/fsw113>.

</div>

<div id="ref-Nash1951" class="csl-entry">

Nash, John. 1951. “Non-Cooperative Games.” *Annals of Mathematics* 54
(2): 286–95. <https://doi.org/10.2307/1969529>.

</div>

<div id="ref-devtools2022" class="csl-entry">

Wickham, Hadley, Jim Hester, Winston Chang, and Jennifer Bryan. 2022.
*Devtools: Tools to Make Developing r Packages Easier*.
<https://CRAN.R-project.org/package=devtools>.

</div>

</div>
