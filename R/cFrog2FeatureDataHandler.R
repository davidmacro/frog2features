 #' cFrogdataHandler 
 #   
 #' @require data.table
 #' @require digest
 #' @require igraph
 cFrog2FeatureDataHandler <- R6Class("frog2featureDH", 
 
    public = list( 
         
        # ------------------------------------------------------------------------------------------- # 
        # // Part 1: Public Global Variables                                                          #
        # ------------------------------------------------------------------------------------------- # 
        
        Description         = NULL,  
        Logbook             = NULL,    
        PathDefinitions     = NULL,  
        Labels              = NULL, 
        Features            = NULL,
         
        Settings            = list(
            KeepLog         = FALSE,
            Filters.Global  = NULL,
            dictionary.path = NULL
        ), 
         
        RequiredDataTransformations = list(
            
        ), 
        
        Data = list(  
            # The raw dataset should contain     the unedited raw dataset as provided
            # at the start of the analysis. 
            Raw                      =   NULL,
            Raw.List                 =   NULL,
            Raw.Dictionaries         =   NULL,
            Merged                   =   NULL,
            Replay                   =   NULL,
            Replay.List              =   NULL,
            Replay.Dictionaries      =   NULL
        ),    
         
        Plot = function(graph = "full", paths ="all", pretty = F, vertex.size = 38, vertex.size2 = 10, edge.arrow.size = 0.2, adjust.position = list(), plot.position = F, ...){
            
                if(graph == "full"){
                    
                    if(paths == "all"){
                        
                        lay <- layout_as_tree(self$RequiredDataTransformations$full.graph)
                        lay.dt <- {lay %>% data.table}[,x := V1][,y := V2][, V1 := NULL][, V2 := NULL]
                        lay.dt <- cbind(lay.dt, name = V(self$RequiredDataTransformations$full.graph)$name)
                  
                        if(pretty){ 
                            
                            lay.dt <- cbind(lay.dt, name         = V(self$RequiredDataTransformations$full.graph)$name)
                            lay.dt <- cbind(lay.dt, vertex.label = V(self$RequiredDataTransformations$full.graph)$vertex.label)
                            lay.dt <- cbind(lay.dt, type         = V(self$RequiredDataTransformations$full.graph)$type)
                            lay.dt <- cbind(lay.dt, type         = V(self$RequiredDataTransformations$full.graph)$token)
                            
                            lay.dt[type == "output",             y := min(y)]
                            lay.dt[type == "create.dictionary",  y := min(y)]
                            lay.dt[type == "save.dictionary",    y := min(y)]
                            lay.dt[type == "construct.features", y := min(y)]
      
                        }
                        
                        for(node in names(adjust.position)){
                            if("x" %in% (adjust.position[[node]] %>% names)){
                                lay.dt[name == node, x := adjust.position[[node]]$x]    
                            }
                            
                            if("y" %in% (adjust.position[[node]] %>% names)){
                                lay.dt[name == node, y := adjust.position[[node]]$y]            
                            }
                        }
                        
                        
                        if(plot.position){ 
                            vertex.label <- lay.dt[, name %+% "\r\n" %+% "(x=" %+% x %+% ", y=" %+% y %+% ")"]
                        } else {
                            vertex.label <- V(self$RequiredDataTransformations$full.graph)$vertex.label
                        }
                        
                        lay <- lay.dt[, .(x,y)] %>% as.matrix
                        
                        plotRequiredDataTransformations(
                           layout          = lay,
                           dt.graph        = self$RequiredDataTransformations$full.graph,
                           vertex.label    = vertex.label,
                           vertex.size     = vertex.size, 
                           vertex.size2    = vertex.size2, 
                           edge.arrow.size = edge.arrow.size, ...
                        )
                        
                        
                         
                    } else {
                        
                        if(length(paths) == 1){
                            
                            path <- frogdh$RequiredDataTransformations$full.paths[[paths]]
                        
                            lay <- layout_as_tree(path)
         
                            plotRequiredDataTransformations(path, ...)         
                                
                        } else {
                            
                        } 
                    }
                     
                } 
            
                
                if(graph == "replay"){
                    
                    if(paths == "all"){
                        
                        lay <- layout_as_tree(self$RequiredDataTransformations$replay.graph)
                        lay.dt <- {lay %>% data.table}[,x := V1][,y := V2][, V1 := NULL][, V2 := NULL]
                        lay.dt <- cbind(lay.dt, name = V(self$RequiredDataTransformations$replay.graph)$name)
                  
                        if(pretty){ 
                            
                            lay.dt <- cbind(lay.dt, name         = V(self$RequiredDataTransformations$replay.graph)$name)
                            lay.dt <- cbind(lay.dt, vertex.label = V(self$RequiredDataTransformations$replay.graph)$vertex.label)
                            lay.dt <- cbind(lay.dt, type         = V(self$RequiredDataTransformations$replay.graph)$type)
                            lay.dt <- cbind(lay.dt, type         = V(self$RequiredDataTransformations$replay.graph)$token)
                            
                            lay.dt[type == "output",             y := min(y)]
                            lay.dt[type == "create.dictionary",  y := min(y)]
                            lay.dt[type == "save.dictionary",    y := min(y)]
                            lay.dt[type == "construct.features", y := min(y)]
      
                        }
                        
                        for(node in names(adjust.position)){
                            if("x" %in% (adjust.position[[node]] %>% names)){
                                lay.dt[name == node, x := adjust.position[[node]]$x]    
                            }
                            
                            if("y" %in% (adjust.position[[node]] %>% names)){
                                lay.dt[name == node, y := adjust.position[[node]]$y]            
                            }
                        }
                        
                        
                        if(plot.position){ 
                            vertex.label <- lay.dt[, name %+% "\r\n" %+% "(x=" %+% x %+% ", y=" %+% y %+% ")"]
                        } else {
                            vertex.label <- V(self$RequiredDataTransformations$replay.graph)$vertex.label
                        }
                        
                        lay <- lay.dt[, .(x,y)] %>% as.matrix
                        
                        plotRequiredDataTransformations(
                           layout          = lay,
                           dt.graph        = self$RequiredDataTransformations$replay.graph,
                           vertex.label    = vertex.label,
                           vertex.size     = vertex.size, 
                           vertex.size2    = vertex.size2, 
                           edge.arrow.size = edge.arrow.size, ...
                        )
                        
                        
                         
                    } else {
                        
                        if(length(paths) == 1){
                            
                            path <- frogdh$RequiredDataTransformations$replay.graph[[paths]]
                        
                            lay <- layout_as_tree(path)
         
                            plotRequiredDataTransformations(path, ...)         
                                
                        } else {
                            
                        } 
                    }
                     
                } 
             
        },
        
        
        # ------------------------------------------------------------------------------------------- # 
        # // Part 2: Initialization                                                                   #
        # ------------------------------------------------------------------------------------------- # 
         
        #' Title
        #'
        #' @param dtfrog 
        #' @param objFeatures 
        #' @param ... 
        #'
        #' @return
        #' @export
        #'
        #' @examples
        initialize = function(Dataset              = NULL, 
                              Settings             = NULL,
                              PathDefinitions      = NULL, 
                              Dictionaries         = NULL, ...){
 
            cat("# ----------------------------------------------------------------\r\n")
            cat("# Frog2Feature output:                                               \r\n") 
            cat("# ----------------------------------------------------------------\r\n\r\n")
            cat("# Initialize Frog Data Handler object: \r\n\r\n")
            cat("# ----------------------------------------------------------------\r\n\r\n")
 
            # ------------------------------------------------------------------------------------------- #
            # (i.0) Determine whether to use a logbook                                                    #
            # ------------------------------------------------------------------------------------------- #
            
            # Initialize a log if required
            cat("     - Initialize logbook object \r\n")
                
            # Determine whether to use a log book:
            if(self$Settings$KeepLog){ self$Logbook <- cLogBook$new(name = "frog data handler")}     
             
 
            # ------------------------------------------------------------------------------------------- #
            # (i.1) Check input arguments                                                                 #
            # ------------------------------------------------------------------------------------------- #
 
            if(any(Dataset %>% is.null, Settings %>% is.null, PathDefinitions %>% is.null)){
                cat("Warning: Initializing empty or partial Frog2Features-object \r\n\r\n")
                cat("Please note that to continue, you must additionally provide: \r\n\r\n")
            }
  
            if(Dataset %>% is.null){ 
                cat("\t\t a valid 'Dataset' object from a FROG-pipeline; use: setData(...)\r\n ") 
            }  
            
            if(Settings %>% is.null){
                cat("\t\t a valid 'Settings'object; use: setSettings(...)\r\n ") 
            } else {
                self$Settings %<>% modifyList(Settings)
            }
            
            if(PathDefinitions %>% is.null){
                cat("\t\t a valid 'PathDefinitions' list of path definitions stored in feature objects. Use: setPathDefinitions\r\n") 
            }
            
            if(Dictionaries %>% is.null){
                cat("\t\t No dictionaries a valid 'PathDefinitions' list of feature-definition objects.use: setPathDefinitions\r\n")  
            }
            
            # ------------------------------------------------------------------------------------------- #
            # (i.2) Assign arguments to local variables                                                   #
            # ------------------------------------------------------------------------------------------- #
            
            if(Dataset %>% is.null %>% not){ 
                cat("\t\t - Set data \r\n") 
                self$setData(x = Dataset)
            }    
                
            if(Settings %>% is.null %>% not){ 
                cat("\t\t - Override set with settings provided by user \r\n") 
                self$setSettings(x = Settings)
            }        
            
            if(PathDefinitions %>% is.null %>% not){ 
                cat("\t\t - Set Path definitions \r\n") 
                self$setPathDefinitions(PathDefinitions)
            }    
            
            if(Dictionaries %>% is.null %>% not){ 
                cat("\t\t - Set Predifined Dictionaries \r\n") 
                self$setDictionaries(x = Dictionaries)
            }    
             
            # ------------------------------------------------------------------------------------------- #
            # (i.3) Assign arguments to local variables                                                   #
            # ------------------------------------------------------------------------------------------- #             
             
                
            #self$run()
                
            cat("\r\nNow run, in successive order: \r\n\r\n")
                
            cat("\t\t\t ...$setRawData(x = ...)\r\n")
            
            cat("\t\t\t ...$setSettings()\r\n")
                    
                
            invisible(self)
        }, 
        
        # ------------------------------------------------------------------------------------------- # 
        # // Part 3: Public Setters                                                                   #
        # ------------------------------------------------------------------------------------------- # 
        setRawData = function(x){
         
            stopifnot(c("data.frame", "data.table") %in% class(x) %>% any)
			 
            self$Data$Raw <- x %>% copy %>% FixStringsFactors %>% data.table
            setorderv(self$Data$Raw, c("docid","sent","position"))
            
            invisible(self)  
        },
            
        setReplayData = function(x){
		
			stopifnot(c("data.frame", "data.table") %in% class(x) %>% any)
		
            self$Data$Replay <-x %>% copy %>%  FixStringsFactors %>% data.table
            setorderv(self$Data$Raw, c("docid","sent","position"))
            
            invisible(self)  
        },           
         
        setSettings = function(...){ 
            self$Settings %<>% modifyList(list(...))   
            invisible(self) 
        },
        
        setPathDefinitions = function(...){
            
			dotted <- list(...)
 
			if(dotted %>% names %>% length %>% equals(0)){
				names(dotted) <- "f" %+% 1:length(names(dotted))
			}
		   
            self$PathDefinitions <- dotted
            
            invisible(self) 
        },
        
        setDictionaries = function(x){
            
            if(self$setDictionaries %>% private$checkDictionaries){
                self$setDictionaries <- x
            }  
            
            invisible(self) 
        },
         
		ClearFeatures = function(){

			

		},
        
        Merge = function(..., type = "raw"){
         
            MergeList <- list()
            
            for(feature in names(self$Data$Raw.Features)){
                
                MergeList[[feature]] <- merge(x     = self$getLables()[, docid],
                                              y     = self$Data$Raw.Features[[feature]],
                                              by    = "docid",
                                              all.x = T,
                                              all.y = F) 
            
            }
              
        },
        
        
        FixMissings = function(x){
          
            for (j in names(DT)){
                set(DT,which(is.na(DT[[j]])),j,0)
            }
             
        },
        
        
        #' Title
        #' 
        #' @param document.labels 
        #' @param document.labels.columns 
        #' @param do.return 
        #' @param do.save.internally 
        #'
        #' @return
        #' @export
        #'
        #' @examples
        setLabels = function(id.col                  = NULL,
                             labels                  = NULL, 
                             label.columns           = NULL,
                             do.return               = FALSE, 
                             do.save.internally      = FALSE,
                             missings                = c("all")){
              
        
                # Check 1: Check whether the F2F-datahandler object is ready to assign labels to anything 
                #          (for this, we need the raw dataset to be present)
                if(self$Data$Raw %>% is.null | self$Data$Raw %>% is.data.table %>% not){  
                    stop("Error: the instance should have a Frog output-object assigend to $Data$Raw")  
                    if(! c("docid", "sent", "position") %in% (self$Data$Raw %>% colnames)){ 
                         stop("Error: the instance should have a valid Frog output-object assigend to ...$Data$Raw") 
                    } 
                }
        
                # Check 2: Check whether the arguments are properly formatted; we either need label.columns 
                #          to be defined (method 1) or both id.col and labels to be present (method 2). 
                #          throw an error is a user tries to do both...
                    
                # Check the arguments of the method: 
                
                arg.ch1 <- (label.columns %>% is.null %>% not)
                arg.ch2 <- (id.col %>% is.null %>% not) & (labels %>% is.null %>% not)
                
                if((arg.ch1 %xor% arg.ch2) %>% not){
                    stop("Error: either specify 'id.col' and 'labels', or specify 'label.columns'. Not neither, not both, but xor.")
                }
                     
                # -------------------------------------------------------------------------------------------------------------------------------
                # Method 1: by using the (already) specified self$Data$Raw dataset, and definining the column names that comprise the label(s)
                #   requirements: 
                #      - dtfrog is already part of the instance
                #      - label.columns has been provided as a character vector
                # -------------------------------------------------------------------------------------------------------------------------------
                if(arg.ch1){ 
                    if(self$Settings$KeepLog){
                        self$Logbook$append(
                            action       = "Assign document labels (extract)", 
                            description  = "Extract columns " %+% paste0(label.columns, collapse = ",") %+% " from the raw frog dataset.",
                            functioncall = sys.call()
                        )  
                    } 
                    self$Labels <- self$Data$Raw[, c("docid",label.columns), with=F] %>% FixStringsFactors %>% unique 
                    setorderv(x = self$Labels,cols = "docid") 
                }
  
                # -------------------------------------------------------------------------------------------------------------------------------
                # Method 2: by providing a new data.frame with 
                #      - dtfrog is either given as an argument (in which case setData is also run (!)), or already part of the instance
                #      - document.labels have ben provided as a data.table
                # -------------------------------------------------------------------------------------------------------------------------------            
            
                # Method 2: 
                if(arg.ch2){  
                
                    if(self$Settings$KeepLog){    
                        self$Logbook$append(
                           action       = "Assign document labels (merge)", 
                           description  = "Merge labels from external datasource; merge on column " %+% id.col %+% ".", 
                           functioncall = sys.call()
                        )   
                    }
                        
                    self$Labels <- merge(x     = self$Data$Raw[, c(id.col), with = FALSE] %>% FixStringsFactors %>% unique, 
                                         y     = labels, 
                                         by    = id.col,
                                         all.x = TRUE,
                                         all.y = TRUE)
                   
                    setorderv(x = self$Labels, cols = "docid")
                   
                } 
            invisible(self) 
         }, 
         
          
        # ------------------------------------------------------------------------------------------- # 
        # // Part 4: Public Getters                                                                   #
        # ------------------------------------------------------------------------------------------- # 
        getSettings   = function(){ return(self$Settings) },  
        getRawData    = function(){ return(self$Data$Raw) }, 
        getReplayData = function(){ return(self$Data$Replay) },       
                
        getData       = function(x){ 
        
            if(x %in% c("raw","Raw")){return(self$Data$Raw)}
            
            if(x %in% c("global","Global", "master","Master")){return(self$Data$Global)}
    
            return(self$Data$Raw[[x]]) 
            
        },
        
        getDictionary = function(path, type = "raw"){
            
            stop("Type should be 'raw' or 'replay'.") %if% (type %notin% c("raw", "replay"))
            
            if(type == "raw"){ 
                id <- self$Data$Raw.Dictionaries[[path]]$dictionaryID 
                return(self$Data$Raw.List[[id]]) 
            }
            
            if(type == "replay"){
                id <- self$Data$Replay.Dictionaries[[path]]$dictionaryID 
                return(self$Data$Replay.List[[id]])
            }
             
        },
         
        getDataForFeature = function(path, type = "raw", fix.missing.values = FALSE){
            
            stop("Type should be 'raw' or 'replay'.") %if% (type %notin% c("raw", "replay"))
            
            if(fix.missing.values == FALSE){ 
                if(type == "raw"){ 
                   id <- self$Data$Raw.Dictionaries[[path]]$dataID 
                   return(self$Data$Raw.List[[id]]) 
                }
            
                if(type == "replay"){
                   id <- self$Data$Replay.Dictionaries[[path]]$dataID 
                   return(self$Data$Replay.List[[id]])
                }
            } else { 
			
                if(self$Labels %>% is.null %>% not){
                    
                    if(type == "raw"){ 
                        
                        id <- self$Data$Raw.Dictionaries[[path]]$dataID 
                        
                        data <- merge(
                            x = self$Labels[, list(docid)],
                            y = self$Data$Raw.List[[id]],
                            by = c("docid"),
                            all.x = T,
                            all.y = F
                        )
                        
                        {colnames(data) %>% lapply(FUN = function(x){
                            {setDT(data)[get(x) %>% is.na, (x) := fix.missing.values]}
                        })}
                         
                        return(self$Data$Raw.List[[id]]) 
                    }
                
                    if(type == "replay"){
                       id <- self$Data$Replay.Dictionaries[[path]]$dataID 
                       return(self$Data$Replay.List[[id]])
                    }
                } else {
                    cat("Error. Please run setlabels first.")
                }
                 
            } 
             
        },
        
		getPaths = function(type = "raw", verbose = F){
		 
			stopifnot(type %in% c("raw","replay"))
		 
			if(type == "raw"){
				if(self$RequiredDataTransformations$full.paths %>% is.null){
					cat("No paths defined yet!") %if% verbose
					return(FALSE)
				} else {
					cat("The following paths are defined: \r\n") %if% verbose
					cat(self$RequiredDataTransformations$full.paths %>% names) %if% verbose
					return(self$RequiredDataTransformations$full.paths %>% names)
				} 
			}  
			
			if(type == "replay"){
				if(self$RequiredDataTransformations$replay.paths %>% is.null){
					cat("No paths defined yet!") %if% verbose
					return(FALSE)
				} else {
					cat("The following paths are defined: \r\n") %if% verbose
					cat(self$RequiredDataTransformations$replay.paths %>% names) %if% verbose
					return(self$RequiredDataTransformations$replay.paths %>% names)
				}  
			}
			
		}, 
		
        getFeature = function(x, type = "raw"){
            if(type == "raw"){
                if(x %in% (self$Data$Raw.Features %>% names)){
                    return(self$Data$Raw.Features[[x]])
                }      
            }
        },
         
        getLabels               = function(){ return(self$Labels) }, 
        getFeatures             = function(){ return(self$Features) }, 
        getProperties           = function(){ return(self$Properties) }, 
        getDictionaries         = function(){ return(self$Dictionaries) }, 
        
        getRequiredDatasets     = function(){ return(self$RequiredDatasets) },        
        getRequiredDictionaries = function(){ return(self$RequiredDictionaries) },
 
        Make  = function(verbose=T, raw = T, replay = F, create.features = T){
 
             # Convert the feature-definition list to objects that define the data-transformations that need to be executed. 
             if({self$RequiredDataTransformations %>% length} == 0){
                 stop("No data-transformations defined. Please run $getRequiredDataTransformations() first.")
             }
             
             # Initialize the list to store the output datasets
             if(replay %>% not){
                 
                  stop("No data present; please run $setRawData([dataset]) first.") %if% (self$Data$Raw %>% is.null)
             
                  self$Data$Raw.List      <- list(rep(NA,self$RequiredDataTransformations$full.graph %>% vertex_attr(name = "data.id.current") %>% max))
                  self$Data$Raw.List[[1]] <- self$Data$Raw %>% copy
                  
                  # Error handling:
                  if(self$RequiredDataTransformations$full.graph %>% is.igraph %>% not){
                     stop("Error: the object in $RequiredDataTransformations$full.graph is not a valid igraph object.")     
                  } 
    
                  if(sapply(self$RequiredDataTransformations$full.paths, is.igraph) %>% all %>% not){
                     stop("Error: provided values for dt.paths are not (all) valid igraph objects.") 
                  }             
                   
                  # Assume that none of the vertices has been visited before: 
                  self$RequiredDataTransformations$full.graph %<>% set_vertex_attr("visited", value = 0)
                  
                  # Walk through each path
                  for(path.name in self$RequiredDataTransformations$full.paths %>% names){
                     
                     cat("\r\nTraversing path: ", path.name, ":\r\n") %if% (verbose)
                     cat("------------------------------------------------------------------ \r\n")
                     
                     # Walk through each node of each path
                     for(vertex.name in {self$RequiredDataTransformations$full.paths[[path.name]] %>% V}$name){
                   
                        cat("\t processing: ", vertex.name) %if% (verbose)
                        
                        # Derive the ruel from the vertex attribute:
                        tmp.rule <- getRuleFromVertexAttributes(self$RequiredDataTransformations$full.graph, vertex.name) 
                  
                        tmp.rule %<>% unserialize_rule()
                        
                        # If the vertex has been visited before, do not perform any action
                        if(tmp.rule$visited == 1){ 
                                
                            cat("\t (*)", "\r\n") %if% (verbose)
                                
                        } else {
                                
                            # If the node has not yet been visited, perform the required action
                            cat("\t fun: " %+% tmp.rule$type) %if% (verbose)    
                                
                            d.previous <- tmp.rule$data.id.previous
                            d.current  <- tmp.rule$data.id.current
                            
                            cat(" (" %+% d.previous  %+% " -> "  %+% d.current  %+% ")") %if% (verbose)
                            cat("\r\n") %if% (verbose)
                             
                            if(tmp.rule$type == "construct.features"){
                               
                                if(create.features){ 
                                    
                                    dictionaryID <- self$Data$Raw.Dictionaries[[path.name]]$dictionaryID
                                    dataID       <- self$Data$Raw.Dictionaries[[path.name]]$dataID
                                    
                                    cat("\t\t\t\t\t Create features based on: \r\n ")
                                    cat("\t\t\t\t\t\t dictionaryID = " %+% dictionaryID  %+%  "\r\n")
                                    cat("\t\t\t\t\t\t dataID       = " %+% dataID %+%  "\r\n")
                                    
                                    cat("Data structure: \r\n\t\t")
                                    
                                    data.input <<- self$Data$Raw.List[[dataID]] 
                                    dictionary <<- self$Data$Raw.List[[dictionaryID]]
                                    tmp.rule   <<- tmp.rule
                                    path.name  <<- path.name
                                    
                                    self$Data$Raw.Features[[path.name]]  <- getCallFromRule(
                                        rule       = tmp.rule, 
                                        data.input = self$Data$Raw.List[[dataID]], 
                                        dictionary = self$Data$Raw.List[[dictionaryID]]
                                    ) 
                                } 
                                
                            } else { 
                                
                                if(tmp.rule$type == "output"){
                                     cat("------------------------------------------------------------------ \r\n")
                                     cat("                                                         Done. \r\n\r\n")
                                } else {
                                    self$Data$Raw.List[[d.current]]  <- getCallFromRule(
                                        rule       = tmp.rule , 
                                        data.input = self$Data$Raw.List[[d.previous]]
                                    )
                                }
                            }
                              
                            tmp.rule$visited <- 1
                                
                            tmp.rule %<>% serialize_rule
                            
							# Update the attributes of the vertex in the full graph
                            self$RequiredDataTransformations$full.graph %<>% UpdateVertexAttributes(
                                vertex.name = vertex.name, 
                                new.rule    = tmp.rule, 
                                verbose     = F
                            ) 
							
							# Update the attributes of the vertex in extracted path
							self$RequiredDataTransformations$full.paths[[path.name]] %<>% UpdateVertexAttributes(
                                vertex.name = vertex.name, 
                                new.rule    = tmp.rule, 
                                verbose     = F
                            )  
                        }
                     }
                  }
             } else {
                
                  stop("No data present; please run $setReplayData([dataset]) first.") %if% (self$Data$Replay %>% is.null)

                  
                  self$Data$Replay.List      <- list(rep(NA,self$RequiredDataTransformations$replay.graph %>% vertex_attr(name = "data.id.current") %>% max))
                  
                  
                  self$Data$Replay.List[[1]] <- self$Data$Replay %>% copy
                  
                  # Error handling:
                  if(self$RequiredDataTransformations$replay.graph %>% is.igraph %>% not){
                    stop("Error: the object in $RequiredDataTransformations$full.graph is not a valid igraph object.")     
                  }
    
                  if(sapply(self$RequiredDataTransformations$replay.paths, is.igraph) %>% all %>% not){
                     stop("Error: provided values for dt.paths are not (all) valid igraph objects.") 
                  }             
                   
                  # Assume that none of the vertices has been visited before: 
                  self$RequiredDataTransformations$replay.graph %<>% set_vertex_attr("visited", value = 0)
                  
                  # Walk through each path
                  for(path.name in self$RequiredDataTransformations$replay.paths %>% names){
                     
                     cat("Traversing path: ", path.name, ":\r\n") %if% (verbose)
                     
                     # Walk through each node of each path
                     for(vertex.name in {self$RequiredDataTransformations$replay.paths[[path.name]] %>% V}$name){
  
                   
                        cat("\t processing: ", vertex.name) %if% (verbose)
                        
                        # Derive the rule from the vertex attribute:
                        tmp.rule <- getRuleFromVertexAttributes(self$RequiredDataTransformations$replay.graph, vertex.name)
                 
                        tmp.rule %<>% unserialize_rule()
                        
                        # If the vertex has been visited before, do not perform any action
                        if(tmp.rule$visited == 1){ 
                                
                            cat("\t (*)", "\r\n") %if% (verbose)
                                
                        } else {
                                
                            # If the node has not yet been visited, perform the required action
                            cat("\t fun: " %+% tmp.rule$type) %if% (verbose)    
                                
                            d.previous <- tmp.rule$data.id.previous
                            d.current  <- tmp.rule$data.id.current
                            
                            cat(" (" %+% d.previous  %+% " -> "  %+% d.current  %+% ")") %if% (verbose)
                            cat("\r\n") %if% (verbose)
                             
                            if(tmp.rule$type == "construct.features"){ 
                              
                                # Perform function call derived from rule
                                if(create.features){ 
                                        
                                    dictionaryID <- self$Data$Replay.Dictionaries[[path.name]]$dictionaryID
                                    dataID       <- self$Data$Replay.Dictionaries[[path.name]]$dataID
                                        
                                    cat("\t\t\t\t\t Create features based on: \r\n ")
                                    cat("\t\t\t\t\t\t dictionaryID = " %+% dictionaryID  %+%  "\r\n")
                                    cat("\t\t\t\t\t\t dataID       = " %+% dataID %+%  "\r\n")
                                        
                                    cat("Data structure: \r\n\t\t")
                                    
                                    self$Data$Replay.Features[[path.name]]  <- getCallFromRule(
                                        rule       = tmp.rule, 
                                        data.input = self$Data$Replay.List[[dataID]], 
                                        dictionary = self$Data$Raw.List[[dictionaryID]]
                                    )    
                                }
                            } else { 
                                
                                if(tmp.rule$type == "output"){
								
                                    cat("------------------------------------------------------------------ \r\n")
                                    cat("                                                         Done. \r\n\r\n")
									
                                } else {
								
                                    self$Data$Replay.List[[d.current]]  <- getCallFromRule(
                                        rule       = tmp.rule , 
                                        data.input = self$Data$Replay.List[[d.previous]]
                                    ) 
                                } 
                            }
                            
                            tmp.rule$visited <- 1
                                
                            tmp.rule %<>% serialize_rule
                            
							# Update the attributes of the vertex in the full graph
                            self$RequiredDataTransformations$replay.graph %<>% UpdateVertexAttributes(
                                vertex.name = vertex.name, 
                                new.rule    = tmp.rule, 
                                verbose     = F
                            ) 
							
							# Update the attributes of the vertex in extracted path
							self$RequiredDataTransformations$replay.paths[[path.name]] %<>% UpdateVertexAttributes(
                                vertex.name = vertex.name, 
                                new.rule    = tmp.rule, 
                                verbose     = F
                            )  
                        }
                     }
                  }
             } 
            invisible(self)
        }, 
          
        FixMissing = function(){ 
            frogdh$Data$Raw.Features[[1]] 
        },
        
		getRequiredDataTransformations =  function(PathDefinitions = NULL){
			
			if(PathDefinitions %>% is.null & self$PathDefinitions %>% is.null){
				stop("\t\tNo features defined; cannot calculate required datatransformations.")
			}
			
			if(PathDefinitions %>% is.null & self$PathDefinitions %>% is.null %>% not){
				
				PathDefinitions <- self$PathDefinitions
				
				if({self$RequiredDataTransformations %>% length} > 0){
					cat("\t\tWarning: data transformations already provided; overriding previously specified settings.\r\n")
				} 
				
			}
			
			self$RequiredDataTransformations <- getRequiredDataTransformations(x = PathDefinitions)
			
			self$Data$Raw.Dictionaries  <- getRequiredDictionaryList(
				paths = self$RequiredDataTransformations$full.paths,
				graph = self$RequiredDataTransformations$full.graph
			)
			
			self$Data$Replay.Dictionaries <- getRequiredDictionaryList(
				paths = self$RequiredDataTransformations$replay.paths,
				graph = self$RequiredDataTransformations$replay.graph
			)
			
			self$Data$Raw.Features    <- list()
			self$Data$Replay.Features <- list()
			
		},
		 
				 
              
        help = function(method = NULL){
        
            cat("\r\n") 
            cat("# --------------------------------------------------------------------------------------------- \r\n") 
            cat("# Instructions for the frog datahandler object \r\n")
            cat("# --------------------------------------------------------------------------------------------- \r\n") 
            cat("# See also: \r\n\r\n")
            cat("#     >  help(\"cFrogDatahandler\")\r\n")
            cat("#     >  help(\"cFrog2Features\")\r\n")
            cat("# --------------------------------------------------------------------------------------------- \r\n") 
            cat("\r\n") 
 
            cat("# ----------------------------------------------------------------------------------------------- \r\n") 
            cat("#  (1) Build the dh for the first time \r\n")
            cat("# ----------------------------------------------------------------------------------------------- \r\n") 
            cat("#  This is the typical workflow if frog datahandling procedure has never \r\n")
            cat("#  been run on this dataset; the required steps are:   \r\n")
            cat("#   i.   ...$setData(x = ...) with a valid frog data.table object; \r\n") 
            cat("#   ii.  ...$setSettings(Settings = ...) with a valid fh.options object;\r\n") 
            cat("#   iii. ...$setFeatures(objFeatures = ...) with a valid objFeatures list. \r\n") 
            cat("# ---------------------------------------------------------------------------------------------- \r\n") 
            cat("#  iv. ...$run() run the required actions \r\n") ;
            cat("# ---------------------------------------------------------------------------------------------- \r\n\r\n") 
            
            cat("#----------------------------------------------------------------------------------------------- \r\n") 
            cat("# (2) Load data and apply procedure from previous run \r\n")
            cat("#----------------------------------------------------------------------------------------------- \r\n") 
            cat("# This is the typical workflow to apply an existing procedure on new data:  \r\n")
            cat("#  i.   ...$setData(x = ...)   with a valid frog data.table object \r\n") 
            cat("#  ii.  ...$loadMethod(file = ...)      with a valid frog method acquired form a previous run \r\n")
            cat("#  iii. ...$run()                       run the required actions \r\n") 
            cat("#--------------------------------------------------------------------------------------------- \r\n\r\n") 
            
            
            cat("# --------------------------------------------------------------------------------------------- \r\n") 
            cat("#  (3) Load State from previous run \r\n")
            cat("# --------------------------------------------------------------------------------------------- \r\n") 
            cat("#  This is the typical workflow to restore a previous instance of the object \r\n") 
            cat("#   ii.  ...$loadMethod(file = ...)      with a valid frog method acquired form a previous run \r\n")
            cat("#   iii. ...$run()                       run the required actions \r\n") 
            cat("# --------------------------------------------------------------------------------------------- \r\n\r\n") 
            
            
            cat("# -------------------------------------------------------------------------------------------- \r\n") 
            cat("#  Optional actions - datahandling:  \r\n")
            cat("# -------------------------------------------------------------------------------------------- \r\n") 
            cat("#  ...$saveMethod(file = ...) save the method used in this analysis; \r\n")
            cat("#                             useful to rerun analysis on new data.  \r\n") 
            cat("#  ...$saveState(file = ...)  save the current state of the object, including dictionaries, \r\n")
            cat("#                             data transformations, etc.; \r\nuseful if data transformation \r\n")
            cat("#                              steps are time consuming. \r\n") 
            cat("# --------------------------------------------------------------------------------------------- \r\n\r\n") 
 
            invisible(self) 
            
        },
         
        saveMethod = function(file, include_transformed_data = FALSE){
        
            # Method is a combination of: 
            # ------------------------------------------------------------------------------
            #         (1)        the options provided (in fh.options)
            #         (2)        the feature construction objects provided (in ofjFeatures)
            #         (3)        the list/data.frame with data transformations to be applied
            #         (4)        the list/data.frame with data transformations to be applied
            #         (5)        the list/data.frame with data transformations to be applied        
            
            method_to_save = list( 
                Settings     = self$Settings,
                objFeatures  = self$objFeatures, 
                Dictionaries = self$Dictionaries
            )
        
            invisible(self) 
            
        },
        
        loadMethod = function(file){
 
        },
         
        get_document_properties = function(dtfrog                       = NULL, 
                                           document.properties            = NULL, 
                                           document.properties.columns  = NULL,
                                           do.return                    = FALSE, 
                                           do.save.internally           = FALSE){
          
            self$Logbook$append(action       = "Get document properties", 
                                description  = "If documents have properties (other than dependent-variable-information), 
                                                create a data.table at the document level with this information.",
                                functioncall = sys.call())  
            
            
            dtfrog                       <- (dtfrog) %or_if_null% (self$dtfrog)                  
            document.properties          <- (document.properties) %or_if_null% (self$document.properties) %or_if_null% NA      
            document.properties.columns  <- (document.properties.columns) %or_if_null% (self$fh.options$document.properties) 
            
            cat("Document label columns: ", document.properties.columns, "\r\n")
              
            if(is.na(document.properties)){  
                document.properties  <- dtfrog[, c("docid",document.properties.columns), with=T] %>% FixStringsFactors %>% unique     
            }  
            
            dt.document.properties <- dtfrog[, c("docid",document.properties.columns), with=F] %>% FixStringsFactors %>% unique
            
            if(do.save.internally){  
                self$document.properties <- dt.document.properties 
                invisible(self) 
            }
            
            if(do.return){  
                return( dt.document.properties)    
            }    
        },
        
 
        query = function(exp='', dtfrog = NULL){
            
            if(is.null(dtfrog)){
                dtfrog <- self$dtfrog    
            }
            
            return(dtfrog[eval(parse(text=exp)),] )
            
        } 
    )    
)
    
    
    
     
        
