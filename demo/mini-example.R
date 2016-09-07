library(SpatialSemantics)
library(Rgraphviz)

#prepare semantic-enabled functions:
captureSemantics(rnorm, procedureName="rname") <- TRUE
sum = function(x){base::sum(x)}
captureSemantics(sum, semantics = "any set -> any", procedureName="rname") <- TRUE

# 'Analysis' begins here
enableProvenance()
t=1
attr(t, which="semantics") <- "test"
t=t+2
t=round(abs(rnorm(t)))
t=rnorm(sum(t), semantics="any -> any set")
disableProvenance()
# 'Analysis' ends here

#Show all recorded commands:
provenance_history()

## Print some semantics and provenance of t:
getVersions(t)
getSemanticPedigree(t)

#create, plot and export derivation graph
gRlayout = getScriptGraph()
plot(gRlayout, main="Derivation Graph")

#setwd("output")
toFile(gRlayout , layoutType="dot", filename="miniexample.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="miniexample.svg", fileType="svg")
system(command = "dot -Tpdf miniexample.dot -o miniexpample.pdf")
#setwd("../")

#reset provenance for further recording
reset_provenance()
