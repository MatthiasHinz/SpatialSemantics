library(SpatialSemantics)
library(Rgraphviz)
library(sp)

enableProvenance()
data("meuse")
coordinates(meuse) <- c("x","y")
meuse$lzinc = log(meuse$zinc)
disableProvenance()
toFile(getScriptGraph() , layoutType="dot", filename="myDerivationGraph.dot", fileType="dot")
system(command = "dot -Tpdf myDerivationGraph.dot -o myDerivationGraph.pdf")

getVersions(meuse)
getObjectSemantics(meuse)
getSemanticPedigree(meuse)
reset_provenance()

