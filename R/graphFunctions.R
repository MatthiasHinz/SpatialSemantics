#require(stringr)
#require("Rgraphviz")
#require(CodeDepends)
#require(codetools)

algebr = new.env()


####
# Provenance functions
####

#
# Initiates a new derivation graph object for provenance recording
#
# @return derivation graph object in internal data.frame format
#
algebr$newDerivationGraph <-function(){
  g = list(V = c(), E = list(), eAttrs=list(), nAttrs=list(), attrs=list(), fCalls=list(), exps=list())
  g$attrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fillcolor="white", style="filled"),
                  edge=list(style="solid", arrowhead="normal"))
  return(g)
}

#
#' Retrieve derivation graph for export / vizalization
#'
#' This function returns a derivation graph from the previously recorded provenance, if any
#'
#' @param g Derivation graph in internal data.frame format as created by the internal function 'algebr$newDerivationGraph'
#'
#' @return Returns derivation graph of the script in Ragraph format (Rgraphviz) or NULL, if there is no provanance available
#'
#' @seealso \code{\link{enableProvenance}}, \code{\link{disableProvenance}\code, {\link{reset_Provenance}}
#' @export
#'
#' @examples
getScriptGraph <- function(g=algebr$scriptGraph){
  if(is.null(g$V) || length(g$V) == 0){
    warning("There is no derivation graph available because no provenance was recorded. Returning NULL")
    return(NULL)
  }

  gR <- graph::graphNEL(nodes = g$V,
                 edgeL = g$E,
                 edgemode = "directed")
  graph::graph.par(list(fontsize=11))
  gR <- Rgraphviz::layoutGraph(gR) #graphNEL format (graph package, just edges and nodes)
  #Ragraph format (GraphViz package, includes layout information)
  gRlayout <- Rgraphviz::agopen(gR, name="f", attrs=g$attrs, nodeAttrs=g$nAttrs, edgeAttrs=g$eAttrs)
}

algebr$provenanceCallback <- function(algebr_env = algebr) {
  #TODO: review the counting of rec_num, for capturing semantics and parsing in-outputs
  isFirstCall = TRUE
  function(expr, value, ok, visible, data=algebr_env) {
    if(isFirstCall){
      #don't track first task executed (= call to enableProvenance() )
      isFirstCall <<- FALSE
      return(TRUE)
    }
    algebr=data

    #------------------------------------------------------------------------------------
    # Collect provenance information from workspace changes that may be used for parsing
    #------------------------------------------------------------------------------------
    algebr$history_list = append(algebr$history_list, expr)
    #cat(as.character(as.expression(expr)))
    algebr$new_ls = ls(envir = globalenv())

    #notify and track new variables
   #  new_vars = algebr$new_ls[!algebr$new_ls %in% algebr$last_ls]
   # # if(length(new_vars)>0)
   # #   cat(paste("The following variables have been initialized: ", paste(new_vars, collapse = " "),"\n"))
   #
   #  ls(envir = globalenv())[ls() %in% algebr$ls_last]
   #
   #  info = CodeDepends::scriptInfo(CodeDepends::readScript(txt=as.character(as.expression(expr))))
   #  side_effects = new_vars[!new_vars %in% info[[1]]@outputs]

    ##save last captured call sementics to temporary call stack

    algebr$tempCallStack = NULL
    if(dim(algebr$callStack)[1]>0){
      algebr$tempCallStack <- subset(algebr$callStack, rec_num == algebr$rec_num) ##TODO: Review counting re_num (as mentioned above)
     }

    info = CodeDepends::scriptInfo(CodeDepends::readScript(txt=as.character(as.expression(expr))))
    side_effects = c()
    lapply(algebr$new_ls, function(var){
      vare = algebr$enquote(var)
      obj=eval(parse(text=vare), envir = globalenv())
      if(!isTRUE(attr(obj,"isTracked")) && !(var %in% info[[1]]@outputs)){
        side_effects <<- append(side_effects, var)
      }else
        return(invisible())
    });side_effects

    #------------------------------------------------------------------------------------
    # Parse available provenance information to enhance the derivation graph
    #------------------------------------------------------------------------------------

    # actually parsing the last executed expression to a graph:
    algebr$scriptGraph=algebr$parseCommand(expr,algebr$scriptGraph, first_call = TRUE)



    cmd_id = algebr$scriptGraph$first_call
    sapply(side_effects, function(variable){
      algebr$scriptGraph <<- algebr$addNodeObject(var = variable, g = algebr$scriptGraph, isOutput = TRUE)
      algebr$scriptGraph <<- algebr$addEdgeOutput(output =  variable,cmd = cmd_id,g = algebr$scriptGraph, hidden = TRUE)

      obj=eval(parse(text=algebr$enquote(variable)), envir = globalenv())
      if(!isTRUE(attr(obj, "isTracked"))){
        attr(obj, "isTracked") <-TRUE
        assign(variable, obj,envir = globalenv())
      }
    })

     #if(length(side_effects)>0)
     # warning(paste("These variables have been initialized from side-efects of the previous task: ", paste(side_effects, collapse = " ")))


    #estimates semantics of functions and expressions that are not explicitely stated
   algebr$scriptGraph=algebr$estimateMissingSemantics(algebr$scriptGraph)

    #look
    #-----------------------------------------------------------------------------------

    # Be aware that the last_ls variable is overwritten IN THE END of the callback, but may be used during parsing from various methods
    # So please don't move it!
    algebr$last_ls = algebr$new_ls
    algebr$rec_num = algebr$rec_num+1 #record number

    ## for debugging
    # tst = function(){
    #   sapply(ls(envir = globalenv()),function(var){
    #     obj=eval(parse(text=algebr$enquote(var)), envir = globalenv())
    #     return(isTRUE(attr(obj, "isTracked")))
    #   })}
    #
    # print(tst())
    return(TRUE)
  }
}


#' Enable / disable provenance tracking
#'
#' @return
#' @export
#'
#' @seealso \code{\link{disableProvenance}\code, {\link{reset_Provenance}}
#' @examples
enableProvenance <- function(){
  if(is.null(algebr$rec_num))
    algebr$rec_num = 1
  if(is.null(algebr$version_history))
    algebr$version_history = list()
  if(is.null(algebr$history_list))
    algebr$history_list = list()
  if(is.null(algebr$scriptGraph))
    algebr$scriptGraph = algebr$newDerivationGraph()
  if(is.null(algebr$callStack))
    algebr$callStack = data.frame()

  if(!isTRUE(algebr$isEnabled)){
    algebr$callback <- addTaskCallback(algebr$provenanceCallback())
    algebr$isEnabled = TRUE
  }else{
    warning("Provenance tracking is already enabled!")
    return(invisible())
  }

   algebr$last_ls = ls(envir = globalenv())

   for(var in algebr$last_ls){
     obj=eval(parse(text=algebr$enquote(var)), envir = globalenv())
     if(!isTRUE(attr(obj, "isTracked"))){
       attr(obj, "isTracked") <-TRUE
       assign(var, obj,envir = globalenv())
     }
   }
  invisible()
}



#' Enable / disable provenance tracking
#'
#' @return
#' @export
#'
#' @seealso \code{\link{enableProvenance}}, {\link{reset_Provenance}}
#' @examples
disableProvenance <-function(){

  if(isTRUE(algebr$isEnabled)){
    algebr$isEnabled= FALSE
    removeTaskCallback(algebr$callback)
  }else{
    warning("Provenance tracking is already disabled!")
  }
  invisible()
}


#' Deletes all internally recorded provenance and sets the tracker back to default state
#'
#' @return
#' @export
#'
#' @seealso \code{\link{enableProvenance}}, \code{\link{disableProvenance}\code
#' @examples
reset_provenance <-function(){
  if(isTRUE(algebr$isEnabled)){
    disableProvenance()
  }
  algebr$rec_num = 1
  algebr$version_history = list()
  algebr$history_list = list()
  algebr$scriptGraph = algebr$newDerivationGraph()
  algebr$callStack = data.frame()
}



#' Retrieve the list of all recorded commands
#'
#' @return
#' @export
#'
#' @examples
provenance_history <- function(){
  return(algebr$history_list)
}



###
# Functions for creating a derivation graphs
####

algebr$addNodeLabel <- function(node_id, g, label){
  g$nAttrs$label[[node_id]]=label
  return(g)
}


algebr$getNodeLabel <- function(node_id, g){
  g$nAttrs$label[[node_id]]
}


algebr$addNode = function(node_id, g, label=NULL, color=NULL, shape=NULL){
  node_id = algebr$unquote(as.character(as.expression(node_id)))
  if (all(g$V != node_id)){
    g$V = append(g$V, node_id)
    g$E[[node_id]]=list(edges=c(), weights=c())
  }

  if(!is.null(label))
      g=algebr$addNodeLabel(node_id, g, label)

  if(!is.null(color))
    g$nAttrs$fillcolor[[node_id]]="orange"

  g$last_vt=node_id
  return(g)
}

algebr$instance <- function(var, pos=0, forInput=FALSE){
  if(!is.character(var))
    var=as.character(substitute(var))

  if(forInput){
    versions = getVersions(var)
    inst=versions[(dim(versions))[1]+pos,]
    if(algebr$rec_num==inst$rec_num && dim(versions)[1]>1)
      return(versions[(dim(versions))[1]+pos-1,])
    else
      return(inst)
  }
   versions = getVersions(var)
   return(versions[(dim(versions)[1]+pos),])
}

algebr$addNewVersionRecord <- function(var){
 # print(paste("new version record",var))
  var=as.character(as.expression(var))
  #print(var)

  var_e = parse(text=algebr$enquote(var))[[1]]
  if(is.name(var_e)){
    obj=eval(var_e, envir = globalenv())
    if(!isTRUE(attr(obj, "isTracked"))){
      attr(obj, "isTracked") <-TRUE
      assign(var, obj,envir = globalenv())
     # print(paste("Start tracking variable",var))
    }
  }

 # print(paste("Version update of", var))
  if(is.null(algebr$version_history[[var]]))
    algebr$version_history[[var]]=data.frame()
  #make sure that versions updated only ONCE per execution (even if this method might be called multiple times)
  else if(algebr$instance(var)$rec_num>=algebr$rec_num){
    return()
  }
  IID=algebr$unquote(var)
  num=dim(algebr$version_history[[var]])[1]
  if(num>0){
    IID=paste0(IID,"~",num+1)
  }

  var0=var
  if(stringr::str_detect(var,pattern = "<-")){
    var0=paste0("`",var,"`")
  }
  command = paste(deparse(provenance_history()[[algebr$rec_num]]), collapse = "\n")
  instance=data.frame(rec_num = algebr$rec_num, IID=IID, class=class(eval(parse(text=var0),envir = globalenv())), semantics = getObjectSemantics(var0), command = command, timestamp=timestamp(quiet = TRUE),stringsAsFactors = FALSE)
 # instance=data.frame(rec_num = algebr$rec_num, IID=IID, class=class(eval(parse(text=paste0("`",var,"`")),envir = globalenv())), semantics = getObjectSemantics(var), timestamp=timestamp(quiet = TRUE),stringsAsFactors = FALSE)
  algebr$version_history[[var]]=rbind(algebr$version_history[[var]], instance)
}


#' Version history of variable bindings
#'
#' This function returns a versioning history showing all modifications (initialization, updates and replacements) of a particular variable in the global environment
#' The version based on recording provenance, i.e. it only captures modifcations that where don after calling enableProvenance() and before calling disableProvenance()
#'
#' @param var name of the variable, either string or object
#'
#' @return
#' @export
#'
#' @examples
getVersions <- function(var){
  if(!is.character(var))
    var=as.character(substitute(var))
  #if(stringr::str_detect(var,pattern = "<-")){
  #  var=paste0("`",var,"`")
  #}
  #print(var)
  return(algebr$version_history[[var]])
}


algebr$addNodeObject <- function(var, g, isInput=FALSE, isOutput=FALSE, isSubset=FALSE) {

  var = algebr$unquote(as.character(as.expression(var)))#ensure that variable name is a string
  #print(paste("addN:",var))
  isVersionUpdated=FALSE
  label=var
  node_name=var
  if((isSubset || exists(var,envir = globalenv())) && is.null(algebr$version_history[[var]])){
    algebr$addNewVersionRecord(var)
    isVersionUpdated=TRUE
  }

  if(isInput && isOutput)
    #normally never happens...
    warning(paste0("Algebr: The variable \"",var,"\" addEclassified as Input AND output. The resulting derivation graph may be flawed."))

  if(isInput){
    #verify that object existed before the last task addEdgeexecuted
    #if not, it shall be treaded as a litteral, e.g. value of class 'symbol' or 'expression'
    #IMPORTANT: this test is heuristic, because the workspace cannot (yet?) really be examined before execution
    # -- so there might be cases of missclassification as literal

    #if-clause assumes that the previously executed task did not add variables to any parent environment
    if(!var %in% algebr$last_ls && !isSubset && !exists(var, envir = parent.env(globalenv()))){
        return(algebr$addNodeLiteral(label = var, g))
    }
    ver_num = dim(subset(getVersions(var), rec_num<algebr$rec_num))[1]
    #versioning support of variables
   # print(paste(ver_num, "version_num1",var, algebr$rec_num))
    if(ver_num>1){
      node_name = paste0(var,"~",ver_num)
      label=node_name
    }
    class=getVersions(var)[ver_num, "semantics"]
    #print(paste("CLASS,",class))
    if(length(class)==0)
      class=getVersions(var)[ver_num+1, "semantics"]
    label=paste0(label, " \\n[",class,"]")
  }else if(isOutput){
    #for outputs, its sufficient to check if objects exists in current workspace
    if(!var %in% algebr$new_ls && !isSubset && !exists(var, envir = parent.env(globalenv()))){
      return(algebr$addNodeLiteral(label = var, g))
    }
    #create entry in version history
    if((isSubset || exists(var,envir = globalenv())) && !isVersionUpdated){
      algebr$addNewVersionRecord(var)
    }

    #versioning support of variables

    ver_num = dim(getVersions(var))[1]
    #print(paste(ver_num, "version_num2",var, algebr$rec_num))
    if(ver_num>1){
      node_name = paste0(var,"~",ver_num)
      label=node_name
    }
    label=paste0(label, " \\n[",getVersions(var)[ver_num, "semantics"],"]")
  }

  g=algebr$addNode(node_name,g, label = label)
  return(g)
}

algebr$addEdgeOutput <- function(output, cmd, g, hidden=FALSE) {
  if(all(g$E[[cmd]]$edges != output)){
    g$E[[cmd]]$edges = append(g$E[[cmd]]$edges, output)
    g$E[[cmd]]$weights = append(g$E[[cmd]]$weights, 1)
    g$eAttrs$color[[paste0(cmd, "~", output)]] = "red"
    if(hidden)
      g$eAttrs$style[[paste0(cmd, "~", output)]] = "dashed"
  }

  if(cmd %in% names(g$fCalls)){
    if(is.null(g$fCalls[[cmd]]$outputs)){
      g$fCalls[[cmd]]$outputs = list(output)
    }else{
      g$fCalls[[cmd]]$outputs = append(g$fCalls[[cmd]]$outputs, output)
    }
  }
  #sligthly redundand code...
  if(cmd %in% names(g$exps)){
    if(is.null(g$fexps[[cmd]]$outputs)){
      g$exps[[cmd]]$outputs = list(output)
    }else{
      g$exps[[cmd]]$outputs = append(g$exps[[cmd]]$outputs, output)
    }
  }
  return(g)
}


algebr$addEdgeInput <- function(input, cmd, g, label=NULL, hidden=FALSE){
  if(all(g$E[[input]]$edges != cmd)){
    g$E[[input]]$edges = append(g$E[[input]]$edges, cmd)
    g$E[[input]]$weights = append(g$E[[input]]$weights, 1)
    if(!is.null(label)){
      g$eAttrs$label[[paste0(input,"~",cmd)]] =label
    }
    g$eAttrs$arrowhead[[paste0(input, "~", cmd)]] = "onormal"

    if(hidden)
      g$eAttrs$style[[paste0(input, "~", cmd)]] = "dashed"
  }else
    warning(paste("AlgebR: Dublicate edge from", input,"to",cmd,"! Second edge (and possibly the label) will not display in derivation graph."))

  if(cmd %in% names(g$fCalls)){
    if(is.null(g$fCalls[[cmd]]$inputs)){
      g$fCalls[[cmd]]$inputs = list(input)
    }else{
      g$fCalls[[cmd]]$inputs = append(g$fCalls[[cmd]]$inputs, input)
    }
  }
  #sligthly redundand code...

  if(cmd %in% names(g$exps)){
    if(is.null(g$fexps[[cmd]]$inputs)){
      g$exps[[cmd]]$inputs = list(input)
    }else{
      g$exps[[cmd]]$inputs = append(g$exps[[cmd]]$inputs, input)
    }
  }


  g$last_vt = input
  return(g)
}


algebr$addEdgeDerivation <- function(parent, child, g, label=NULL, hidden=FALSE){
  if(all(g$E[[parent]]$edges != child)){
    g$E[[parent]]$edges = append(g$E[[parent]]$edges, child)
    g$E[[parent]]$weights = append(g$E[[parent]]$weights, 1)
    if(!is.null(label)){
      g$eAttrs$label[[paste0(parent,"~",child)]] =label
    }
    g$eAttrs$arrowhead[[paste0(parent, "~", child)]] = "normal"

    if(hidden)
      g$eAttrs$style[[paste0(parent, "~", child)]] = "dashed"
  }else
    warning(paste("AlgebR: Dublicate edge from", parent,"to",child,"! Second edge (and possibly the label) will not display in derivation graph."))
  g$last_vt = parent
  return(g)
}


algebr$addEdgeFunctionCall <- function(fun_id, call_id, g, hidden=FALSE){
  if(all(g$E[[fun_id]]$edges != call_id)){
    g$E[[fun_id]]$edges = append(g$E[[fun_id]]$edges, call_id)
    g$E[[fun_id]]$weights = append(g$E[[fun_id]]$weights, 1)
    g$eAttrs$color[[paste0(fun_id, "~", call_id)]] = "blue"
    if(hidden){
      g$eAttrs$style[[paste0(fun_id, "~", call_id)]] = "dashed"
    }
  }
  return(g)
}



algebr$addNodeLiteral = function(label, g){
  vt_id=paste0("lt_",algebr$makeid()) #generates a random id for the node to be unique
  g=algebr$addNode(node = vt_id, g = g,label = label)


  return(g)
  #g$V = append(g$V, cmd_id)
}


algebr$addNodeExpression = function(label, g){
  cmd_id=paste0("expr_",algebr$makeid())
  g=algebr$addNode(node_id = cmd_id, label = label,g = g, color = "orange")
  return(g)
}


algebr$addNodeOperation = function(label, g){
  cmd_id=paste0("fcall_",algebr$makeid())
  g=algebr$addNode(node_id = cmd_id, label = label,g = g, color = "orange")
  return(g)
}


algebr$parseCommand = function(cmd, g=list(V = c(), E = list(), attrs=list(), eAttrs=list(), nAttrs=list(), chunks=list(), last_vt=NULL), first_call=FALSE, isInput=FALSE, isOutput=FALSE){
 # print(paste0("cmd:", deparse(cmd)))
  if(first_call)
    g$first_call = NULL

  #print(paste(as.character(as.expression(cmd)), class(cmd)))
  cmd_id = NULL
  #CASE 1: cmd is some kind of variable
  cmdInfo = CodeDepends::getInputs(algebr$removeTheAt(cmd))
  if(is.name(cmd)){
    g=algebr$addNodeObject(cmd, g, isInput= isInput, isOutput=isOutput)
    cmd_id=g$last_vt
    #CASE 2: cmd is some kind of literal
  }else if(any(sapply(list(is.numeric, is.symbol, is.character, is.logical), function(x){x(cmd)}))){
    #print("---------------------")
    #print(paste0("found literal: ",cmd))
    # print("---------------------")
    g=algebr$addNodeLiteral(as.character(as.expression(cmd)), g)
    cmd_id=g$last_vt
    #CASE 2: cmd is an assignment (TODO: handle operators like ->, <<- ...)
  }else if(any(class(cmd) ==c("=", "<-", "<<-"))){
    cmd = rewriteReplacementFunction(cmd) #turn calls to replacement functions into a parser-friendly style
    # handle left-hand side of assignmet
    #print(paste("something wrong? - sould be an assignment",as.character(as.expression(cmd))))
    g = algebr$parseCommand(cmd[[2]], g, isOutput=TRUE)
    output_id=g$last_vt
    #---- Parses right-hand side of the assignment:
    g = algebr$parseCommand(cmd[[3]], g, isInput=TRUE)
    #--------------------------------------------------------------
    cmd_id = g$last_vt
    g=algebr$addEdgeOutput(output = output_id, cmd = cmd_id, g)
    #case 3: cmd is some kind of call, (possibly right hand of an assignment)
  }else if(class(cmd)=="call"){
    #Case 3.1: cmd is a function definition (whole definition can hardly be displayed or has to be parsed with special care)
    if(cmd[[1]]=='function'){
      cmd_id=paste0("def_",algebr$makeid())
      # cmd_o=paste(as.character(as.expression(cmd), collapse="\n", sep=""))
      g$chunks[[cmd_id]]=list(code=cmd, id=cmd_id)
      g$nAttrs$fillcolor[[cmd_id]]="orange"
      g$V = append(g$V, cmd_id)
      g$E[[cmd_id]]=list(edges=c(), weights=c())
      #handle function definitions
    }else if(algebr$containsOnlyPrimitives(cmd = cmdInfo)){
      #print(paste0("only primitives:", deparse(cmd)))
      exp=as.character(as.expression(cmd))

      g=algebr$addNodeExpression(label = exp,g = g)
      cmd_id = g$last_vt
      g$exps[[cmd_id]] = list(exp=exp, semantics=NA) #semantics must be estimated after evaluation of the whole task

      outputs = append(cmdInfo@outputs, cmdInfo@updates)
      inputs = cmdInfo@inputs
      if(length(inputs)>0)
        sapply(inputs, function(input){
          #TODO: This solution needs to be reviewd (probably conflicting versioning)
          g <<- algebr$addNodeObject(input, g, isInput = TRUE)
          instance = algebr$instance(input, forInput = TRUE)
          IID = instance$IID
          g <<- algebr$addEdgeInput(input = IID, cmd = cmd_id, g = g)
        })

      if(length(outputs)>0)
        sapply(outputs, function(output){
          g <<- algebr$addNodeObject(output, g, isOutput = TRUE)
          instance = algebr$instance(output)
          IID = instance$IID
          g <<- algebr$addEdgeOutput(output = IID, cmd = cmd_id, g = g)
        })



    }else if(cmd[[1]] =='[[' || cmd[[1]]=='['|| cmd[[1]]=='$'|| cmd[[1]]=='@'){
      reference = cmd #TODO: add (this) object reference to profenance of this node

      findParent <- function(cmd){
        if(length(cmd)==1)
          return(cmd)
        else
          return(findParent(cmd[[2]]))
      }

      parent_name = findParent(cmd)
      #print(paste("Parent: ",parent))
      g=algebr$addNodeObject(var = parent_name, g = g, isInput = isInput,isOutput = isOutput)
      parent_id = g$last_vt

      #g=algebr$parseCommand(as.name(selout$selection),g, isInput = FALSE, isOutput = FALSE) ##TODO not so clear how to handel isInput/isOutput flags here (may lead to misclassification in addNodeObject currently)
      g=algebr$addNodeObject(var = cmd, g = g, isInput = isInput,isOutput = isOutput, isSubset=TRUE)
      query = g$last_vt
      cmd_id=g$last_vt
      # print(paste(parent, selout$selection," <- create selection"))
      if(isInput){
        g=algebr$addEdgeInput(parent_id, query, g)
      }else if(isOutput){
        g=algebr$addEdgeOutput(parent_id, query, g)
        parent_old=algebr$instance(as.character(parent_name), forInput = TRUE)$IID
        parent_new=algebr$instance(as.character(parent_name))$IID

        g=algebr$addEdgeDerivation(parent = parent_old,child = parent_new,g = g, hidden = TRUE)
      }
      #-------------------
    }else if(any(cmd[[1]]==c("log","sin","cos"))&& (is.character(cmd[[2]]) || is.numeric(cmd[[2]]))){
      ##this actually seems never to be aplied
      g = algebr$parseCommand(as.character(as.expression(cmd)));
      query=g$last_vt
      g$nAttrs$fillcolor[[query]]="orange"
      cmd_id=g$last_vt
      #------------------------------------
      g = algebr$parseCommand(cmd[[2]], g, isInput=TRUE)
      #-------------------------------------
      g=algebr$addEdgeInput(g$last_vt, query, g)
      #-------------------

      #Case 3.2: cmd is a function call or some operation (e.g. mathematical, logical)
    }else
      # print(paste("found call: ",as.character(as.expression(cmd))))
      ##TODO: preserve mathematical expressions in one node,
      #i.e. diferentiate between mathematical/logical operations and function calls


      if(eval(call("is.function", cmd[[1]]))){
        # TODO: review the way, semantics are evaluated from a call stack
        # Normally the nodes and semantics should match well, but for the rare case that one function is executed multiple times in one task
        # Semantics missmatched if the stack is not processed in the right order.
        # Ideally, expressions should perhaps be parsed the same way as the parser works (leftmost-innermost ?) and the stack should be evaluated from the first to the last call
        #function_obj = algebr$funFromString(as.character(as.expression(cmd[[1]])))
        function_obj = algebr$funFromString(as.character(cmd[[1]]))

        label=algebr$unquote(as.character(as.expression(cmd[[1]])))
        call_function = label

        semantics = NA
        #add semantics to label, if available
        if(captureSemantics(function_obj)){
          fid=attr(function_obj, "fid")
          sel=which(algebr$tempCallStack$fid == fid)
          if(length(sel)>0){
            sel=sel[1] #select first element
            semantics=algebr$tempCallStack[sel,]$semantics
            label=paste0(label,"\\n[",semantics,"]")
            sel=-1*sel
            algebr$tempCallStack = algebr$tempCallStack[sel,] #remove evaluated call
          }
        }

        g=algebr$addNodeOperation(label, g)



        cmd_id = g$last_vt
        if(is.null(g$fCalls_count))
          g$fCalls_count=1


        g$fCalls[[cmd_id]]=list(fname=call_function, command=cmd, count=g$fCalls_count, semantics = semantics)  #called function (without any annotation)
        g$fCalls_count=g$fCalls_count+1
        if(captureSemantics(function_obj)){
          g$fCalls[[cmd_id]]=append(g$fCalls[[cmd_id]], list(wrapper_fid = attr(function_obj, "fid")))
        }

        ##TODO:review this; HEURISTIC: for graphics function create dependency on previously called plot-funcion
        if(call_function %in% c("text", "points", "lines", "title", "par", "abline","arrow","axis","Axis","box", "grid","legend","lines", "pch","rug")){
          for(i in ((length(g$fCalls)-1):1)){
              if(stringr::str_detect(g$fCalls[[i]]$fname, "plot")){
                cmd_id_plot = names(g$fCalls[i])
                g=algebr$addEdgeInput(input = cmd_id_plot, cmd = cmd_id, g=g,hidden = TRUE,label = "[heuristic]")
              }
          }
        }

        # print(paste("label: ",label))

        fdef=eval(cmd[[1]])
        if(!is.primitive(fdef))
          cmd=match.call(fdef, cmd)

        #print(cmd[2:length(cmd)])
        #print(paste0("label: ", label))
        #g$nAttrs$label[[cmd_id]]=label
        #g$nAttrs$fillcolor[[cmd_id]]="orange"
        #g$nAttrs$style[[cmd_id]]="filled"
        #g$V = append(g$V, cmd_id)
        #g$E[[cmd_id]]=list(edges=c(), weights=c())


        if(length(cmd)>1)
          for(i in 2:length(cmd)){
            # print(paste("cmd:",cmd, length(cmd)))
            arg=cmd[[i]]
            #------------------------------------
            g = algebr$parseCommand(arg, g, isInput=TRUE, isOutput=FALSE)
            # connect operand/arguments as input to the operator/function:
            label=names(cmd[i])
            g=algebr$addEdgeInput(g$last_vt, cmd_id, g, label)
          }


        if(!is.primitive(get(algebr$unquote(as.character(as.expression(cmd[[1]])))))){

          function_obj = algebr$funFromString(cmd[[1]])


          if(isTRUE(attr(function_obj,"SemanticWrapper"))){
            function_obj = attr(function_obj,"wFun")
          }

        #  globals = eval(call("findGlobals", function_obj, merge=FALSE))
          globals = codetools::findGlobals(fun = function_obj,merge = FALSE)
          ##TODO: expore function references to other packages
          ls_func = globals$functions[globals$functions %in% ls(envir = globalenv())]

          hiddenCallBlackList <- c("captureSemantics<-","captureSemantics","functionalType<-","addSemanticPedigree")
          if(length(ls_func)>0)
            sapply(ls_func, function(x){
              if(x %in% hiddenCallBlackList) #ignore functions from the blacklist,i,e, tracker functions
                return()

              x<-algebr$unquote(x)
              g <<- algebr$addNodeObject(x,g = g,isInput = TRUE)
              x_IID = algebr$instance(as.character(x))$IID
              g <<- algebr$addEdgeFunctionCall(fun_id = x_IID,call_id = cmd_id,g = g, hidden = TRUE)
            })
          ls_vars = globals$variables[globals$variables %in% ls(envir = globalenv())]
          if(length(ls_vars)>0)
            sapply(ls_vars, function(x){
              if(x %in% c("algebr")) ##try not to track the tracker...
                return()
              g <<- algebr$addNodeObject(x,g = g,isInput = TRUE) #add node as input if not yet registered
              x_IID = algebr$instance(as.character(x))$IID
              #TODO: detect (with scriptInfo of CodeDepends ?) if global variable is updated
              g <<- algebr$addEdgeInput(input = x_IID, cmd = cmd_id, g=g, hidden=TRUE)
            })}
      }
    #for all calls:
    for(V in g$V){
      cmd_ids = as.character(cmd_id)
      call_label=g$fCalls[[cmd_ids]]$fname
      if(!is.null(call_label) && V==call_label){
        V_IID = algebr$instance(V)$IID #link latest instance of this function
        g=algebr$addEdgeFunctionCall(V_IID, cmd_ids, g)
      }
    }

    if(is.null(g$first_call))
      g$first_call=cmd_id
  }


  g$last_vt=cmd_id
  return(g)
}


algebr$containsOnlyPrimitives = function(cmd){
  if(class(cmd)== "ScriptNodeInfo")
    cmdInfo=cmd
  else
    cmdInfo = CodeDepends::getInputs(algebr$removeTheAt(cmd))

  funs=names(cmdInfo@functions)
  sel = funs %in% c("[","[[","$","@") #exclude expressions that only contain subset-operators
  funs=funs[!sel]
  if(length(funs)==0)
    return(FALSE)
  isPrimitive=sapply(funs, function(fun_string){
    function_obj = algebr$funFromString(fun_string)
    return(is.primitive(function_obj))
  })
  return(all(isPrimitive))
}

#' Rewrite replacement function
#'
#' Turns calls of replacement functions into a logically equivalent, parser-friendly form
#'
#' See http://adv-r.had.co.nz/Functions.html
#' @param expr
#'
#' @return
#' @export
#'
#' @examples
#' > rewriteReplacementFunction(quote(attr(t, "semantics") <- "test"))
#'  # t <- `attr<-`(t, "semantics", "test")
#'
#' > rewriteReplacementFunction(quote(functionalType(meuse) <- "SField"))
#'  #meuse <- `functionalType<-`(meuse, "SField")
#'
#'
rewriteReplacementFunction = function(expr){
  if(is.call(expr[[2]])){
    fname= expr[[2]][[1]]

    #exception for these operators:
    if(any(as.character(fname) %in% c('[[','[','$','@'))){
      #print(expr)
      return(expr)
    }

    fname=as.name(paste0(fname,"<-"))
    # find out if a replacement function was used:
    if(exists(as.character(fname)) && eval(call("is.function", fname))){
      op = expr[[1]]
      value = expr[[3]]
      obj = expr[[2]][[2]]
      args= as.list(expr[[2]][-1]);args
      cl=append(fname, args);cl
      cl=append(cl,value);cl
      cl=as.call(cl);cl
      cl=list(op, obj, cl);cl
      cl=as.call(cl);cl
      return(cl)
    }
  }
  return(expr)
}

###
# Semantics related functions
####

#' Estimate semantics of a given object
#'
#' @param var object or name of the object
#' @param env The environment in which the object shall be evaluated
#' @param isLiteral TRUE/FALSE allowed. Force var to be interpreted as a literal, even if it is for instance a name of an object in the workspace.
#'
#' @return Returns the semantic reference type of the object.
#'  If the reference type is uncertain because a required annotation is missing, a reference type is assumed and prefixed with a questionmark (?), also a warning is given out.
#'  For objects, where no semantic mapping is defined, simply the class will be returned
#' @export
#'
#' @examples
getObjectSemantics <- function(var, env=globalenv(), isLiteral=FALSE){

  if((!is.character(var) && !is.symbol(var) && !is.name(var)) || isLiteral) {
    obj=var
    var=as.character(as.expression(substitute(var)))
  }else{
    var=as.character(var)
    obj = tryCatch(eval(parse(text=var),envir = env),error = function(e) var)
  }

  ##for expressions such as meuse[1] (subset), it will be -ASSUMED- that the semantics are the same as with paren data set. i.e. meuse
# print(paste0("parse variable ", paste(var, collapse = ""), " of class ", class(var)))
 var_e = parse(text=algebr$enquote(var))
  var_e = var_e[[1]]
  if(!is.symbol(var_e) && var_e[[1]]=="["){
    var_e=var_e[[2]]
    var=deparse(var_e)
    obj = tryCatch(eval(var_e,envir = env),error = function(e) var)
  }


  if(!is.null(attr(obj, "semantics")))
    return(attr(obj, "semantics"))
  # test if object is annotated with semantics, if not, predict semantics from class and structure (note that the latter is only a generic assumption, i.e. based on heuristics)
  # be carful about the order of if-statements, because some classes extend others but imply different semantics
  if (any(sapply(list(is.numeric, is.character, is.factor, is.symbol, is.name, is.expression), function(fun) {
    return(fun(obj))
  }))) {
    if(length(obj)<=1)
      return("Q")
    else return("Q set")
  }

  if (is.logical(obj)) {
    if(length(obj)<=1)
      return("bool")
    else return("bool set")
  }

  if (is(obj, "CRS")) { ## for the actual mss package
    return("'a")
  }

  if (is(obj, "SField")) { ## for the actual mss package
    SFieldData_observations = slot(obj, "observations")
    sObs = getObjectSemantics(SFieldData_observations, env = environment())
    sObs = stringr::str_replace(sObs, " set","")
    return(paste(sObs, "x SExtend set"))
  }

  if (is(obj, "SpatialLinesDataFrame")) {
    semantics= "(?)S x Q set"
    warning(paste("No semantic annotation available for object",var,". Assumend semantics will be",semantics))
    return(semantics)
  }

  if (is(obj, "SpatialLines")) {
    return("S set")
  }


  if (is(obj, "SpatialPixelsDataFrame")) {
    semantics= "(?)S x Q set"
    warning(paste("No semantic annotation available for object",var,". Assumend semantics will be",semantics))
    return(semantics)
  }

  if (is(obj, "SpatialPointsDataFrame")) {
    semantics= "(?)S x Q set"
    warning(paste("No semantic annotation available for object",var,". Assumend semantics will be ",semantics))
    return(semantics)
  }

  if (is(obj, "SpatialPixels")) {
    return("S set")
  }

  if (is(obj, "SpatialPoints")) {
    #how many points
    return("S set")
  }

  if (is(obj, "SpatialMultiPointsDataFrame")) {
    #how many points
    semantics= "(?)S x Q set"
    warning(paste("No semantic annotation available for object",var,". Assumend semantics will be ",semantics))
    return(semantics)
  }

  if (is(obj, "SpatialMultiPoints")) {
    #how many points
    return("S set")
  }

  if (is(obj, "SpatialGridDataFrame")) {
    semantics = "(?)S x Q set"
    warning(paste("No semantic annotation available for object",var,". Assumend semantics will be ",semantics))
    return(semantics)
  }

  if (is(obj, "SpatialGrid")) {
    return("R set")
  }

  if (is(obj, "SpatialPolygonsDataFrame")) {
    #TODO: how many Polygons?
    semantics= "(?)R x Q set"
    warning(paste("No semantic annotation available for object",var,". Assumend semantics will be",semantics))
    return(semantics)
  }

  if (is(obj, "SpatialPolygons")) {
    #TODO: how many Polygons?
    return("R set")
  }

  if (is(obj, "Spatial")) {
    return("S set")
  }

 if(any(sapply(list(is.data.frame, is.list, is.array, is.matrix), function(fun) {
    return(fun(obj))
  }))) {
      return("Q set")
  }

  return(class(obj))
}


#' Pre-defined semantics of a function call
#'
#' @param x An object of type function
#'
#' @return
#' - returns a list of pre-defined semantics which are alowed for this function
#' - returns "dynamic" if the function is semantics-enabled but does not have pre-defined semantics
#' - returns NULL if the function is not semantics-enabled and does not implement pre-defined semantics
#' @export
#' @seealso \code{\link{getObjectSemantics}}, \code{\link{captureSemantics}}
#'
#' @examples
getCallSemantics = function(x){
  if(!is.function(x)){stop("The given object x is not a function. Call semantics are an attribute only for semantics-enabled funtions.")}

  if(!captureSemantics(x)){
    warning("This function is not semantics-enabled. Please use 'captureSemantics' in order to create a semantic wrapper.")
    return(NULL)
  }
  return(attr(x,"callSemantics"))
}


algebr$estimateCallSemantics <- function(args, output) {
  s_output = getObjectSemantics(output)
  s_inputs = sapply(args, function(arg){getObjectSemantics(arg, env = environment())})
  call_semantics = paste(paste(s_inputs, collapse = " -> "), s_output, sep=" -> ")
  return(call_semantics)
}

# setting call semantics is not yet supported, because the captureSemantics object cannot be accessed from the function body (I don't know how)
# setting semantics permanently has to be done by creating a new semantic wrapper
#algebr$`callSemantics<-` = function(x, semantics){
#  if(!captureSemantics(x)){
#    stop("This function is not semantics-enabled. Please use 'captureSemantics' in order to create a semantic wrapper.")
#  }
#  return(attr(x,"callSemantics") <- semantics)
#}

#annotates a given variable with the callsemantics of procedure x, standard postprocessor for function wrappers
algebr$genericProcedureAnnotator <- function(procedureName){
  function(args, output, callSemantics){
    output=addSemanticPedigree(obj = output, name = procedureName, procedure = callSemantics)
    return(output)
  }
}

#jars of clay - frail
#' Capture Semantics
#'
#' @param fun
#'
#' @return
#' @export
#'
#' @examples
captureSemantics <- function(fun){
  return(isTRUE(attr(fun,"SemanticWrapper")))
}



#' Capture Semantics
#'
#' @param fun
#' @param semantics
#' @param procedureName
#' @param validator
#' @param postprocessor
#' @param value
#'
#' @return
#' @export
#'
#' @examples
`captureSemantics<-` <- function(fun, semantics = NA, procedureName = "unknown", validator=NULL, postprocessor=algebr$genericProcedureAnnotator(procedureName), value){
  if(is.null(semantics)){
    semantics=NA
  }

  bool=value #shall function be wrapped or not
  #returieving the function names seems not to be possible from here
  #fname=as.character(substitute(fun,env = globalenv()))
  fid=paste0("w_",algebr$makeid())
  wFun = fun #wrapped function
  if(!bool){
    if(isTRUE(attr(fun,"SemanticWrapper"))){
      fun= attr(fun,"wFun")
    }
    return(fun)
  }

  if(is.primitive(fun)){
    stop("Primitive functions are not supported!")
  }

  if(isTRUE(attr(fun,"SemanticWrapper"))){
    fun = attr(fun,"wFun")
    fun = `captureSemantics<-`(fun, value = value, semantics = semantics)
    warning("Function was already wrapped. The old wrapper is replaced.")
    return(fun)
  }

  wrapper=function(){
    #TODO: Improve wrapping behaviour, e.g. by capturing and passing the original variable names with non-standard evaluation


    ls_fun=ls(envir = environment())
    args = sapply(ls_fun, function(var){
      #print(var)
      out=list(get(var))
      names(out[1]) <- var
      return(out)
    })
    #remove semantics-argument not to be passed on
    if(!"semantics" %in% names(formals(wFun))){
      args=args[names(args)!="semantics"]
    }
    isConsistent = TRUE

    output = do.call(wFun, args, envir = environment())


    call_semantics = algebr$estimateCallSemantics(args, output)


    if(!is.null(postprocessor)){
      output = postprocessor(args, output, call_semantics)
    }
    if(!is.null(validator)){
      isValid = validator(args, output, semantics, call_semantics)
      if(!isValid){
        warning("Semantic inconsistensy found during post-validation of a semanic wrapper!")
        isConsistent=FALSE
      }
    }

    if(length(semantics==1) && is.na(semantics)){ ## estimate semantics from in/output
      semantics = call_semantics
    }else{
      s=stringr::str_to_upper(stringr::str_trim(semantics))
      sc=stringr::str_to_upper(stringr::str_trim(call_semantics))
      if(!sc %in% s){
        warning(paste("Inconsistent function semantics, given is ",call_semantics,"but expected was one of the following: ",paste(semantics,collapse=", ")))
        isConsistent = FALSE
      }
      if(!isConsistent)
        call_semantics = paste0(call_semantics, ": INCONSISTENT!")
    }

    ## The call is only recordet if it occurs from the global environment (to avoid confusion if they are called internaly or recursively(?))
    if(isTRUE(algebr$isEnabled) && identical(parent.frame(),globalenv())){
      cat(paste0("Call: ",call_semantics,"\n"))
      callSemantics=data.frame(rec_num=algebr$rec_num, semantics=call_semantics,fid=fid, time = timestamp(quiet = TRUE), stringsAsFactors = FALSE)
      algebr$callStack=rbind(algebr$callStack,callSemantics)
    }
    ## check if output was annotated
    rec_nums = attr(output, "semanticPedigree")$rec_num
    if(isTRUE(algebr$isEnabled) && (is.null(rec_nums) || !algebr$rec_num %in% rec_nums)){
      genericProcessor = algebr$genericProcedureAnnotator(procedureName)
      output=genericProcessor(args, output, call_semantics)
      cat("Information: using 'genericProcedureAnnotator'-function to annotate output. Consider customizing the postprocessor-function in order to apply user-defined semantics.")
      #warning("Function output was not annotated with pedigree. Please consider using 'genericProcedureAnnotator'-function or write a costum postprocessor function that annotates the output object!")
    }

    return(output)
  }

  formals_w = formals(wFun)
  formals_w$semantics = semantics
  #print(paste(deparse(formals_w), "formals"))
  formals(wrapper) <- formals_w
  attr(wrapper,"SemanticWrapper") <- TRUE
  if(length(semantics) == 1 && is.na(semantics)){
    attr(wrapper,"callSemantics") <- "dynamic"
  }else{
    attr(wrapper,"callSemantics") <- semantics
  }

  attr(wrapper,"wFun") <- wFun
  attr(wrapper,"fid") <- fid
  fun=wrapper
  return(fun)
}

#' Add semantic pedigree
#'
#' @param obj
#' @param attr
#' @param name
#' @param procedure
#' @param result_semantics
#' @param parent_semantics
#'
#' @return
#' @export
#'
#' @examples
addSemanticPedigree <- function(obj, attr="ALL", name = NA, procedure, result_semantics=NULL, parent_semantics=NULL){
  #print(paste("adding semantics for", substitute(obj), attr, name, procedure, result_semantics, parent_semantics))
  ## attr might be of length > 1. in this case create one record for each attr
  if(length(attr)>1){
    for(attr_n in attr){
      obj <- addSemanticPedigree(obj, attr_n, name, procedure, result_semantics, parent_semantics)
    }
    return(obj)
  }


    varname = as.character(substitute(obj))
  if(is.null(attr(obj, "semanticPedigree"))){
    attr(obj, "semanticPedigree") <- data.frame()
  }
  if(attr=="ALL" && is.null(parent_semantics)){
    if(!is.null(result_semantics)){
      attr(obj, "semantics") <-result_semantics
    }
  }else{
    if(!is.null(result_semantics) && attr!="ALL"){
      attr(obj[[attr]], "semantics") <-result_semantics
    }

    if(!is.null(parent_semantics)){
      attr(obj, "semantics") <-parent_semantics
    }
  }

  if(attr=="ALL" && is.null(parent_semantics)){
    result_semantics = getObjectSemantics(obj)
    parent_semantics = NA
  }else if(attr=="ALL" && !is.null(result_semantics)){
    parent_semantics =  getObjectSemantics(obj)
  }else{
    #print(paste("estimating semantics of ---",varname, attr))
    result_semantics = getObjectSemantics(obj[[attr]])
    parent_semantics =  getObjectSemantics(obj)
  }

  command=NA
  rec_num=NA

  if(algebr$isEnabled){
    rec_num=algebr$rec_num
    if(algebr$rec_num <= length(provenance_history()))
      command=paste(deparse(expr=provenance_history()[[algebr$rec_num]]),collapse="\n")
       #if the call is currently executed, the command is still unknown but can be interfered later from the record number
  }
    record = data.frame(procedureName=name,  procedure=procedure,  result_attribute=attr, result_semantics=result_semantics, parent_semantics = parent_semantics,  rec_num=rec_num, command=command, stringsAsFactors = FALSE)
    attr(obj,"semanticPedigree") <- rbind(attr(obj,"semanticPedigree"), record)
    if(attr=="ALL" && !is.null(names(obj))){
      for(name in names(obj)){
        try({
            # in some cases, the "names" don't refer to attributes but to ids or something else
            # so they are not always reliable for accessing atrributes
           if(is.null(attr(obj[[name]],"semanticPedigree"))){
             attr(obj[[name]],"semanticPedigree") <- data.frame()
           }
           attr(obj[[name]],"semanticPedigree") <- rbind(attr(obj[[name]],"semanticPedigree"), record)
        }, silent= TRUE)
        }
    }else if(attr!="ALL" && attr %in% names(obj)){
      if(is.null(attr(obj[[attr]],"semanticPedigree"))){
        attr(obj[[attr]],"semanticPedigree") <- data.frame()
      }
      attr(obj[[attr]],"semanticPedigree") <- rbind(attr(obj[[attr]],"semanticPedigree"), record)
    }

  return(obj)
}

algebr$findMissingPedigreeCommands <- function(ped){
  for(i in 1:dim(ped)[1]){
    record = ped[i,]
    if(is.na(record$command) && !is.na(record$rec_num)){
      if(record$rec_num <= length(provenance_history())){
          record$command=paste(deparse(expr=provenance_history()[[record$rec_num]]),collapse="\n")
      }
    }
    ped[i,] = record
  }
  return(ped)
}

#' Get semantic pedigree
#'
#' @param obj
#' @param attr
#'
#' @return
#' @export
#'
#' @examples
getSemanticPedigree <- function(obj, attr="ALL"){
  varname = as.character(substitute(obj))

  sn = slotNames(obj)
  if(!is.null(sn)){
    hasPedigree = sapply(sn, function(slotName){
      pedigree=attr(slot(obj, slotName),"semanticPedigree")
      return(!is.null(pedigree))
    } ,USE.NAMES = FALSE)

    if(any(hasPedigree)){
      message=paste("Information: Semantic pedigree is available for the following slot(s):", paste(sn[hasPedigree], collapse = ", "),"\n")
      cat(message)
    }
  }

  if(is.null(attr(obj,"semanticPedigree")))
    return(NULL)

  out=NULL
  if(attr=="ALL"){
    out = attr(obj,"semanticPedigree")
  }else if(attr %in% names(obj)){
    out = attr(obj[[attr]],"semanticPedigree")
  }else{
    out = attr(obj,"semanticPedigree")
    sel1 = out$attr == "ALL"
    sel2= out$att == attr
    sel = sel1 | sel2 #select all records refering to either the specified attribute or "ALL" attributes
    out = out[sel,]
  }

  if(!is.null(out))
    out = algebr$findMissingPedigreeCommands(out)



  return(out)
}

###
# Utility functions
####

algebr$unquote = function(str){
  if(stringr::str_detect(str,"^`.*`$"))
     return(stringr::str_sub(str, 2,-2))
  else
    return(str)
}


algebr$enquote = function(str){
  if(!stringr::str_detect(str,"^`.*`$") && stringr::str_detect(str,".*<-")){
    str=paste0("`",str,"`")
    return(str)
  }else{
    return(str)
  }
}


algebr$getChunks = function(g, cmd){
  return(g$chunks[[as.character(cmd)]]$code)
}



# visualy compare if each vertex of a graph has a matching list of edges (required for GraphViz)
algebr$compareVE = function(g){
  out=list(nodes=sort(g$V), edges_names= sort(names(g$E)))
  count=out$nodes[summary(as.factor(out$nodes))>1]
    if(length(count)>0)
      cat("ERROR: Certain nodes occur more than once", count,"\n")
  sel=which(!out$nodes %in% out$edges_names)
 # print(out$nodes %in% out$edges_names)
  if(length(sel)>0)
    cat(paste("Error: These nodes are not matched by the lists of edges: ",paste(out$nodes[sel], collapse = ", ")),"\n")
  sel=which(!out$edges_names %in% out$nodes)
  #print(out$edges_names %in% out$nodes)
  if(length(sel)>0)
    cat(paste("Error: These nodes do not occur in the lists of nodes: ",paste(out$edge_names[sel], collapse=", ")),"\n")
  cat(paste())
  cat(paste("Lists of edges: ",length(out$edges_names),"\n"))
  cat(paste("Number of vertexes: ",length(out$nodes),"\n"))


  return(out)
}

#creates a random id of 6 digits using letters and numbers
algebr$makeid=function(){
  range = c(LETTERS, 0:9,letters)
  rnds=runif(n = 6,min = 1,max = length(range))
  paste(sapply(rnds, function(rnd){
    range[rnd]
  }),collapse = "")
}

algebr$funFromString = function(string_var, env = globalenv()){
 # print(paste("funFromString:",string_var))
  fun_obj = try(get(string_var,envir = env), silent = TRUE)
  if(is.function(fun_obj))
    return(fun_obj)
  #in some cases, the following method works better when the other fails (for instance, when the function contained in a subset, i.e. parent$function())
  if(stringr::str_detect(string_var,pattern = "<-")){
    string_var=paste0("`",string_var,"`")
  }
  fun_obj=eval(parse(text=as.character(as.expression(string_var))), envir = env)
  return(fun_obj)
}

##this function is created as a workaround for Issue https://github.com/duncantl/CodeDepends/issues/4
## in CodeDepends. Expressions manipulated with this function are not correct, but it will prevents getInputs from returning wrong inputs
algebr$removeTheAt = function(expr){
  if(length(expr)==1){
    return(expr)
  }
  eList = as.list(expr)
  if(eList[[1]]==as.symbol("@")){
    return(eList[[2]])
  }

  outList=sapply(eList, algebr$removeTheAt)

  outExp = as.call(outList)
  return(outExp)
}


#' Add a functional type to semantic pedigree
#'
#' @param obj
#' @param attr
#' @param value
#'
#' @return
#' @export
#'
#' @examples
`functionalType<-` <- function(obj,attr="ALL",value){
  if(value == "SField"){
      obj=addSemanticPedigree(obj,attr = attr, name = "SField", procedure = "S -> Q",result_semantics = "Q set", parent_semantics = "S x Q set")
      return(obj)
  }

  if(value == "Field"){
    obj=addSemanticPedigree(obj,attr = attr, name = "Field", procedure = "S x T-> Q",result_semantics = "Q set", parent_semantics = "S x T x Q set")
    return(obj)
   }
  stop("Functional type name is unknown. Please consider adding the semantics manually using addSemanticPedigree")
}

#captureSemantics(`functionalType<-`, postprocessor=NULL) <- TRUE

algebr$isAlreadyAnnotated = function(obj){
  #return(FALSE)
  #look if object is already annoted
  rec_nums = attr(obj, "semanticPedigree")$rec_num
  isAnnotated= isTRUE(algebr$isEnabled) && !is.null(rec_nums) && algebr$rec_num %in% rec_nums
 return(isAnnotated)
}

algebr$findExpressionSemantics = function(exp, exp_id, cmd, g){

    in_vec = algebr$findDependencySemantics(exp, exp$inputs, cmd, g)
    in_vec = paste(in_vec, collapse = " -> ")
    out_sem = algebr$findDependencySemantics(exp, exp$outputs, cmd, g)
    out_sem_str = out_sem
    if(length(out_sem>1)){
      out_sem_str = paste("(",paste(out_sem, collapse = ", "),")")
    }
    exp$semantics=paste0(in_vec, " -> ", out_sem_str)
    label = g$nAttrs$label[[exp_id]]
    label = paste0(label,"\n[",exp$semantics,"]")
    g$nAttrs$label[[exp_id]] <- label

   out_iids = NULL
  if(stringr::str_detect(exp_id,"^expr_")){
    g$exps[[exp_id]] <- exp
    out_iids = unlist(g$exps[[exp_id]]$outputs)
  }else if(stringr::str_detect(exp_id,"^fcall_")){
    g$fCalls[[exp_id]] <- exp
    out_iids = unlist(g$fCalls[[exp_id]]$outputs)
  }else{
    stop(paste0("Could not determine whether input is a simple expression or a function call: ", exp_id))
  }

  print(out_sem)
  print(out_iids)
  if(is.null(out_iids))
    return(g)
  mapply(function(out_iid, out_sem){

      var = algebr$varFromIID(out_iid)



      attr=algebr$findAttr(var)

      tryCatch({
        #look if call is function call
        fname = cmd[[1]]
        if (is.function(eval(fname, envir = globalenv()))) {
          name = deparse(fname)
        } else
          name = "expression"

      }, error = function(e) {
        name = "expression"
        warning(e)
      })

      if (is.na(attr)) {
        obj = eval(parse(text = algebr$enquote(var)), envir = globalenv())
        if(algebr$isAlreadyAnnotated(obj))
          return(g)
        obj = addSemanticPedigree(obj = obj, name = name, procedure = exp$semantics, result_semantics = out_sem)
        assign(var, obj, envir = globalenv())
        #print(paste0("Assigninged semantic pedigree to ",var))
      } else{
        #find parent var
        parent_var = parse(text = algebr$enquote(var))[[1]][[2]]
        obj = eval(parent_var, envir = globalenv())
        if(algebr$isAlreadyAnnotated(obj))
          return(g)
        obj = addSemanticPedigree(obj = obj, attr = attr, name = name, procedure = exp$semantics, result_semantics = out_sem)
        assign(deparse(parent_var), obj, envir = globalenv())
        #print(paste0("Assigninged semantic pedigree to ",parent_var))
      }
  }, out_iid=out_iids, out_sem=out_sem)
  return(g)

}


algebr$findDependencySemantics <- function(exp, dep_nodes, cmd, g) {
  if(!is.null(dep_nodes)&& length(dep_nodes)>0){
    sem_vec = sapply(dep_nodes, function(dep_id){
      record=algebr$findInstanceRecord(dep_id)
      if(!is.null(record)){
        return(record$semantics) #should always return a value normally
      } else if(dep_id %in% names(g$fCalls)){
        return("?")
      }else if(stringr::str_detect(dep_id,"^lt_")){
        value=g$nAttrs$label[[dep_id]] ##may not be the most stable solution... TODO: save literal values somewhere else
        return(getObjectSemantics(value,isLiteral = TRUE))
      }else{
        return("?")
      }
    })
    return(sem_vec)
  }else{
    #print(paste("No dep nodes for ", as.character(as.expression(substitute(dep_nodes)))))
    in_sem="?"
    tryCatch({value_xyz = eval(cmd)
    in_sem=getObjectSemantics(value_xyz)},
    error= function(e){
      warning(paste("semantics of expression ", exp, "could not be evaluated"))
      in_sem="?"})
    return(in_sem)
  }
}


algebr$estimateMissingSemantics = function(g){
  #estimate missing semantics of expressions

  #finds semantics of inputs/outputs (dependend nodes, "dep_nodes") for an expression

 mapply(function(exp, exp_id){
    if(is.na(exp$semantics)){
     cmd=parse(text=exp$exp)[[1]]
     g <<- algebr$findExpressionSemantics(exp, exp_id, cmd,g =  g)
     return()
    }
     return()
  }, exp=g$exps, exp_id=names(g$exps))

  #estimate missing semantics of function calls
mapply(function(fCall, call_id){
    if(is.na(fCall$semantics)){

      g <<- algebr$findExpressionSemantics(fCall, call_id, fCall$command,g= g)
      #if(stringr::str_detect(node_id, "^lt_")){
        # assume it is a literal, try to parse label to value #TODO make this more "formal"
      return()
    }
    return()
  }, fCall=g$fCalls, call_id=names(g$fCalls))

  #print(g$exps)
  #print(g$fCalls)
  return(g)
}

#test=algebr$estimateMissingSemantics(algebr$scriptGraph)
#test$exps$expr_lkjnmQ
#test$fCalls$fcall_crrNKR
algebr$varFromIID <-function(iid){
  var=stringr::str_replace(iid, "~\\d*$","")
  if(!is.null(getVersions(var)))
    return(var)
  else
    return(NULL)
}

algebr$findInstanceRecord = function(node_id){
   var=algebr$varFromIID(node_id)
   if(is.null(var))
     return(NULL)
   hist = getVersions(var)
   if(!is.null(hist)){
     sel=which(hist$IID == node_id)

     if(length(sel)==1)
       return(hist[sel,])
     else {
       return(NULL)}
   }else{
     return(NULL)
   }
}

#for testing
# exp=quote(meuse[[c(1,1)]]);exp
# algebr$findAttr(exp)
# exp=quote(meuse[[1]]);exp
# algebr$findAttr(exp)
# exp=quote(meuse["zinc"]);exp
# algebr$findAttr(exp)
# exp=quote(meuse[1,1]);exp
# algebr$findAttr(exp)
# exp=quote(meuse$zinc);exp
# algebr$findAttr(exp)
# exp=quote(meuse[c("zinc", "copper")]);exp
# algebr$findAttr(exp)
# exp=quote(meuse[c(1, 2)]);exp
# algebr$findAttr(exp)
# exp=quote(intZincPointData@observations[1]);exp
# algebr$findAttr(exp)


algebr$findAttr = function(exp){
  if(is.character(exp))
    exp=parse(text=exp)[[1]]
  expl=as.list(exp);exp
  if (length(expl) >= 3) {
    #inputs may be e.g. meuse$zinc, meuse[["zinc"]], meuse[[1]], meuse[[c(1,1)]]
    if (expl[[1]] == "[[") {
      par = expl[[3]]
      if (is.character(par)) {
        attrN = par
      } else if (is.numeric(par)) {
        str = paste0("names(", deparse(expl[[2]]), "[", par, "])")
        attrN = eval(parse(text = str), envir = globalenv())
      } else if (algebr$containsOnlyPrimitives(par)) {
        pval = eval(par, envir = globalenv())
        pval
        if (is.numeric(pval) && length(pval) > 0) {
          str = paste0("names(", deparse(expl[[2]]), "[", pval[1], "])")
          attrN = eval(parse(text = str), envir = globalenv())

        } else{
          attrN = NA
        }
      } else{
        attrN = NA
      }
    } else if (expl[[1]] == "$") {
      if (is.name(expl[[3]])){
        attrN = as.character(expl[[3]])
      } else{
        attrN = NA
      }
    } else if (expl[[1]] == "[") {
      if(length(expl)==4){
        par= expl[[4]]
      }else
        par= expl[[3]]

      if(is.numeric(par)){
        str = paste0("names(", deparse(expl[[2]]), "[", par, "])")
        attrN = eval(parse(text = str), envir = globalenv())
      }else if(is.character(par)){
        attrN = par
      }else
        if(algebr$containsOnlyPrimitives(par)){
          pval = eval(par, envir = globalenv())
          if(is.character(pval)){
            #character selector of multiple columns
            attrN = pval
          }else if(is.numeric(pval) || is.logical(pval)){
            str = paste0("names(", deparse(expl[[2]]), "[", deparse(par), "])")
            attrN = eval(parse(text = str), envir = globalenv())
          }
        }
    } else{
      attrN = NA
    }
    return(attrN)
  }
 return(NA)
}



#' Retrieves packages internals
#'
#' @return An environment in which all internal variables and functions of the SpatialSemantics-package are saved
#' @export
spsem_internals <- function(){
  return(algebr)
}
