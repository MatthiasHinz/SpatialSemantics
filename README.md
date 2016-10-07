# SpatialSemantics

'SpatialSemantics' is a prototypical R package for capturing semantics and provenance of spatio-temporal data analysis.

The package records provenance through task-callbacks while a user executes commands on the R-console. Part of this provenance information is a version history maintained for each variable that answers important provenance-related questions, namely where, when and how an R object has been created or modified. Based on this provenance information, a spatio-temporal data derivation graph is created that allows retracing the execution regarding all involved objects, operations, calls and their parameters.

The package incorporates semantic annotations in compliance with Scheider et al. [1]. These annotations express the meaning and purpose data and functions and serve to detect semantic inconsistencies during the execution. Also, they enhance data derivation graphs with domain-specific information regarding Spatial Statistics. The propagation of semantic metadata is automated as far as possible, but primarily the package relies on user-defined semantics. Therefore it provides functions for the annotation of R objects. The package provided means to annotate objects that are function-outputs by user-defined postprocessor-functions. Function calls are checked regarding meaningfulness by user-defined validator-functions.

It is possible to visualize and export spatio-temporal data derivation graphs using the Rgraphviz-package. Besides the native GraphViz-format 'dot/gv', many other export formats are supported, for instance, pdf, svg, png and jpg.

The package was initiated in fulfillment of the Master's thesis 'Communicating meaning and purpose of spatio-temporal data analysis' by Matthias Hinz. More information follows soon.

[1] Scheider, Simon, et al. "Modeling spatiotemporal information generation." International Journal of Geographical Information Science (2016): 1-29.

## Package installation

```
#for installing dependencies not on CRAN
install.packages("devtools")
devtools::install_github('duncantl/CodeDepends')
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

#optional command, these dependencies should install automatically from CRAN:
install.packages(c("stringr","codetools")

devtools::install_github("MatthiasHinz/SpatialSemantics")
```
## Figures

A data derivation graph of a spatio-temporal aggregation carried out in R:

![Spatio-temporal data derivation graph](https://github.com/MatthiasHinz/SpatialSemantics/raw/master/resources/Florida.png)
