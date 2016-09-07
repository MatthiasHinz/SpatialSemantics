# SpatialSemantics

An R package to capture semantics and provenance of spatio-temporal data analysis.

SpatialSemantics records provenance through task-callbacks as commands are executed on the R-console. Part of this provenance information is a version history maintained for each variable that serves to answer important provenance-related questions, namely where, when and how an object has been created or modified. Based on the collected information a spatio-temporal data derivation graph is constructed that describes all objects, operations, calls and their parameters involved in the execution. 

The graph is enriched with semantic annotations in compliance with Scheider et al. [1]. These annotations serve to clarify the semantic meaning of a piece of data and to detect semantic inconsistencies during the workflow execution. The graph can be vizualized and exported using the Rgraphviz-package. Besides the native GraphViz-format 'dot/gv', many other export formats are supported, for instance pdf, svg,png and jpg. 
    
  [1] Scheider, Simon, et al. "Modeling spatiotemporal information generation." International Journal of Geographical Information Science (2016): 1-29.

## Package installation:

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
