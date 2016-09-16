library(SpatialSemantics);library(Rgraphviz);library(sp)

enableProvenance()
data("meuse")
coordinates(meuse) <- c("x","y")
meuse$lzinc = log(meuse$zinc)
disableProvenance()
toFile(getScriptGraph() , layoutType="dot", filename="myDerivationGraph.dot", fileType="dot")
system(command = "dot -Tpdf myDerivationGraph.dot -o myDerivationGraph.pdf")


getVersions(meuse)[c(-1,-6)]
provenance_history()

getObjectSemantics(meuse)
getSemanticPedigree(meuse)


getVersions(meuse)


attr(meuse,"semantics") <- "'a set"
getObjectSemantics(meuse)


reset_provenance()
library(SpatialSemantics);library(Rgraphviz);library(sp)
enableProvenance() #recording starts here
data("meuse")
coordinates(meuse) <- c("x","y")
functionalType(meuse) <- "SField"
meuse$lzinc = log(meuse$zinc)
disableProvenance() #recording ends here
toFile(getScriptGraph() , layoutType="dot", filename="myDerivationGraph.dot", fileType="dot")
getSemanticPedigree(meuse)[c("procedureName","procedure","result_attribute")]
getSemanticPedigree(meuse)[c("result_semantics","parent_semantics")]
getSemanticPedigree(meuse)[c("rec_num","command")]




#-----------------------
reset_provenance()
rm(list=ls())

library(SpatialSemantics)
log = function(x){
  return(base::log(x))
}
captureSemantics(log) <- TRUE
getDefaultCallSemantics(log)
captureSemantics(log) <- FALSE
captureSemantics(log, semantics = c("Q -> Q", "Q set -> Q set")) <- TRUE
getDefaultCallSemantics(log)

test = 1
log = function(x){
  return(base::log(x))
}


captureSemantics(log) <- FALSE

#---------------------------------------
postprocessor = function(args, output, semantics){
  addSemanticPedigree(obj = output, name = "log", procedure = "Q -> Q", result_semantics = "Q set")
}

validator = function(args, output, defaultSemantics, semantics){
  in_sem = getObjectSemantics(args$x) # get input semantics
  valid = in_sem %in% c("Q", "Q set") # state requirement
  if(!valid){
    warning(paste0("Invalid input of type ",in_sem,"! Expected Q or Q set"))
    return(FALSE)
  }
  return(TRUE)
}

captureSemantics(log, validator = validator, postprocessor= postprocessor) <- TRUE

result=log(1)
getSemanticPedigree(result)[1:4]

input = 1
attr(input, "semantics") <- "D"
result=log(input)
