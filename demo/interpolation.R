library(mss)
library(sp)
library(gstat)
library(Rgraphviz)
library(SpatialSemantics)

# Load resources

# define helper functions
#---------------------------------------------------
init_model = function(pointData) {
  range = sqrt(sum(apply(bbox(pointData@observations), 1, diff)^2)) / 6
  sill = var(pointData[[1]])
  vgm(2 * sill / 3, "Sph", range, sill / 3) # initial variogram model
}
captureSemantics(init_model) <-TRUE

modelSemivariogram = function(pointData) {
  n = names(pointData@observations)
  if (length(n) > 1)
    warning("taking first attribute variable")
  f = as.formula(paste(n[1], "~1")) # which variable to model? take first.
  init = init_model(pointData)
  fit.variogram(variogram(f, pointData@observations), init)
}
captureSemantics(modelSemivariogram) <-TRUE

getInterpolator = function(params, pointData) {
  if (!is(params, "variogramModel"))
    warning("getInterpolator: params should be of class variogramModel")
  #if (!is(pointData, "SField"))
  #	warning("getInterpolator: pointData should be of class SField")
  out=function(locOfInterest) {
    n = names(pointData@observations)[1] # which variable to model? take first.
    f = as.formula(paste(n, "~ 1"))
    out=interpolate(f, pointData, locOfInterest, model = params)
    functionalType(out@observations) <- "SField"
    return(out)
  }
  captureSemantics(out, semantics = "S -> (S, Q)") <-TRUE
  attr(out, "semantics") <- "(S -> (S,Q))"
  # is, strictly not S -> Q but S -> (S,Q)
  return(out)
}
captureSemantics(getInterpolator, procedureName="getInterpolator") <-TRUE

captureSemantics(geometry, procedureName="fst") <- TRUE

SFieldData <- SField

captureSemantics(SFieldData,  procedureName = "SFieldData",
  postprocessor = function(args, output, call_semantics) {
    attr(output@observations, "semanticPedigree") <- getSemanticPedigree(args$observations)
    attr(output@observations, "semantics") <- attr(args$observations,"semantics")
    return(output)
  }
) <- TRUE


# Initialize provenance tracking
enableProvenance()
# Run analysis
#-----------------------------------------------

# load meuse data from package sp in current session:
demo(meuse, ask=FALSE, echo=FALSE)
functionalType(meuse) <- "SField"
functionalType(meuse.grid) <- "SField"
meuse$lzinc = log(meuse$zinc)

zincPointData = SFieldData(meuse["lzinc"], meuse.area)
#class(zincPointData) # of class SField
#plot(zincPointData)

interpolator = getInterpolator(modelSemivariogram(zincPointData), zincPointData)
#class(interpolator) # untyped, but is S -> Q

locInterest = SFieldData(geometry(meuse.grid), geometry(meuse.grid), cellsArePoints = TRUE)
intZincPointData = interpolator(locInterest, semantics = "S set -> S x Q set")
#class(intZincPointData)
spplot(intZincPointData@observations["var1.pred"])

# End analysis / disable provenance recording
#-----------------------------------------------

disableProvenance()

# Create / visualize derivation graph
gRlayout = getScriptGraph()
plot(gRlayout, main="Derivation Graph")

## Export derivation graph to external files with Rgraphviz
#setwd("output")
toFile(gRlayout , layoutType="dot", filename="interpolation.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="interpolation.svg", fileType="svg")
system(command = "dot -Tpdf interpolation.dot -o interpolation.pdf")
#setwd("../")
#for exploration and analytics, if the graph has an error...
#psem_internals()$compareVE(psem_internals()$scriptGraph)
#sapply(ls(), function(vars){getSemanticPedigree(get(vars, envir = globalenv()))})

#reset proveance record for further recording
reset_provenance()
