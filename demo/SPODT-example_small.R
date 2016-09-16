library(SPODT)
library(tree)
library(SpatialSemantics)
library(Rgraphviz)


#captureSemantics(spTransform) <- FALSE
#captureSemantics(`coordinates<-`) <- FALSE
#captureSemantics(`proj4string<-`) <- FALSE
#captureSemantics(CRS) <- FALSE

#captureSemantics(test.spodt) <- TRUE
#captureSemantics(spodtSpatialLines) <- TRUE
#captureSemantics(spodt) <- TRUE

#captureSemantics(tree) <- FALSE
#captureSemantics(partition.tree) <- FALSE



####################################################################
########            Section 4: Data examples                    ####
########  4.1 Clustering malaria episodes, (Bandiagara, Mali)   ####
####################################################################

data("dataMALARIA")

#projection
coordinates(dataMALARIA) <- c("x", "y")
proj4string(dataMALARIA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
dataMALARIA <- spTransform(dataMALARIA, CRS("+proj=merc +datum=WGS84 +ellps=WGS84"))


### BEGIN OF ANALSIS
#--------------------------------------
enableProvenance()

########################################################################################
### Spatial oblique decision tree: using SpODT algorithme for spatial classification ###

spodt.results <- spodt(z ~ 1, data = dataMALARIA, graft = 0.13, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01)

#classification tree
spodt.tree(spodt.results)

#creation of spatial lines between classes and plot
SSL.result <- spodtSpatialLines(spodt.results, dataMALARIA)
plot(SSL.result)
#adding each location
points(dataMALARIA, cex = log(dataMALARIA@data$z*10))

disableProvenance()

### END OF ANALSIS
#--------------------------------------

gRlayout = getScriptGraph()
plot(gRlayout, main="Derivation Graph")

#setwd("output") #optional in order to put output in a separate folder
toFile(gRlayout , layoutType="dot", filename="SPODT2.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="SPODT2.svg", fileType="svg")
system(command = "dot -Tpdf SPODT2.dot -o SPODT2.pdf")
#setwd("../")

#reset provenance for further recording
reset_provenance()


####
###
#TODO: Annotate SMarkedEvent on data malaria
#write validator that checks for Slattice
#let spodtSpatialines annotate data as SinvField
#test validator against meuse

demo(meuse,echo = FALSE, ask = FALSE)
functionalType(meuse) <- "SField"

spodt.results.meuse <- spodt(zinc ~ 1, data = meuse, graft = 0.13, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01) #should throw error

validator = function(args, output, semantics, assumtions){
  pedigreeData = getSemanticPedigree(args[["data"]])
  valid = "SMarkedEvent" %in% pedigreeData$procedureName
  if(!valid){
    warning("Spatial partitioning with the given input data is not meaningful!")
  }
  return(valid)
}


postprocessor = function(args, output, semantics){
  functionalType(output) <- "SInvField"
  return(valid)
}
