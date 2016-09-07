library(SPODT)
library(tree)
library(SpatialSemantics)
library(Rgraphviz)


captureSemantics(spTransform) <- FALSE
captureSemantics(`coordinates<-`) <- FALSE
captureSemantics(`proj4string<-`) <- FALSE
captureSemantics(CRS) <- FALSE

captureSemantics(test.spodt) <- TRUE
captureSemantics(spodtSpatialLines) <- TRUE
captureSemantics(spodt) <- TRUE

captureSemantics(tree) <- FALSE
captureSemantics(partition.tree) <- FALSE

### BEGIN OF ANALSIS
#--------------------------------------
enableProvenance()

####################################################################
########            Section 4: Data examples                    ####
########  4.1 Clustering malaria episodes, (Bandiagara, Mali)   ####
####################################################################

data("dataMALARIA")

#projection
coordinates(dataMALARIA) <- c("x", "y")
proj4string(dataMALARIA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
dataMALARIA <- spTransform(dataMALARIA, CRS("+proj=merc +datum=WGS84 +ellps=WGS84"))

########################################################################################
### Spatial oblique decision tree: using SpODT algorithme for spatial classification ###

spodt.results <- spodt(z ~ 1, data = dataMALARIA, graft = 0.13, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01)

#partition obtained
spodt.results@partition

#R2 for the partition
spodt.results@R2

#classification tree
spodt.tree(spodt.results)

#creation of spatial lines between classes and plot
SSL.result <- spodtSpatialLines(spodt.results, dataMALARIA)
plot(SSL.result)
#adding each location
points(dataMALARIA, cex = log(dataMALARIA@data$z*10))


#Spatial oblique decision tree with no graft option
spodt.results0 <- spodt(z ~ 1, data = dataMALARIA, graft = 0, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01)
SSL.result0 <- spodtSpatialLines(spodt.results0, dataMALARIA)
plot(SSL.result0)
points(dataMALARIA, cex = log(dataMALARIA@data$z*10))

#Test for the SpODT classification
test.spodt(z ~ 1, data = dataMALARIA, spodt.results@R2, "rpois", c(length(dataMALARIA@data$loc), mean(dataMALARIA@data$z)), 99, weight = TRUE, graft = 0.13, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01)


########################################################
###  Using CART algorithm for spatial classification ###

cart.results <- tree(z ~ x + y, data=dataMALARIA)

# classification tree
plot(cart.results)
text(cart.results)

# classification map
partition.tree(cart.results)
points(dataMALARIA, cex = log(dataMALARIA@data$z*10))

disableProvenance()

### END OF ANALSIS
#--------------------------------------

gRlayout = getScriptGraph()
plot(gRlayout, main="Derivation Graph")

#setwd("output") #optional in order to put output in a separate folder
toFile(gRlayout , layoutType="dot", filename="SPODT1.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="SPODT1.svg", fileType="svg")
system(command = "dot -Tpdf SPODT1.dot -o SPODT1.pdf")
#setwd("../")

#reset provenance for further recording
reset_provenance()
