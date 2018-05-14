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
		FeatureDefinitions  = NULL,  
        Labels 				= NULL, 
		Features			= NULL,
         
        Settings           = list(
            KeepLog        = FALSE,
			Filters.Global = NULL			
        ), 
         
		RequiredDataTransformations = list(
		    
		), 
		
		Data = list(  
			# The raw dataset should contain 	the unedited raw dataset as provided
			# at the start of the analysis. 
            Raw                      =   NULL,
            Raw.Checksum             =   NULL, 
		    List = NULL 
            
        ),	
		
 
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
                              FeatureDefinitions   = NULL, 
							  Dictionaries         = NULL, ...){
 
 			cat("# ----------------------------------------------------------------\r\n")
			cat("# Frog2Feature output:											   \r\n") 
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
 
			if(any(Dataset %>% is.null, Settings %>% is.null, FeatureDefinitions %>% is.null)){
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
			
			if(FeatureDefinitions %>% is.null){
				cat("\t\t a valid 'FeatureDefinitions' list of feature-definition objects.use: setFeatureDefinitions\r\n") 
			}
			
			if(Dictionaries %>% is.null){
				cat("\t\t No dictionaries a valid 'FeatureDefinitions' list of feature-definition objects.use: setFeatureDefinitions\r\n")  
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
			
			if(FeatureDefinitions %>% is.null %>% not){ 
				cat("\t\t - Set FeatureDefinitions \r\n") 
				self$setFeatureDefinitions(x = Features)
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
				  
			cat("\t\t\t ...$createRequiredDictionaries()\r\n")
				  
			cat("\t\t\t ...$extractFeatures()\r\n")
				
           
        }, 
        
		# ------------------------------------------------------------------------------------------- # 
		# // Part 3: Public Setters                                                                   #
		# ------------------------------------------------------------------------------------------- # 
        setRawData = function(x){
		 
            # Todo: check on valid frog data input object
            self$Data$Raw <- x %>% FixStringsFactors %>% data.table
            setorderv(self$Data$Raw, c("docid","sent","position"))
            
        },
			
                  
        setSettings = function(...){
			
            self$Settings %<>% modifyList(list(...)) 

		},
		
		setFeatureDefinitions = function(...){
            self$FeatureDefinitions <- list(...) 
		},
		
		setDictionaries = function(x){
			if(self$setDictionaries %>% private$checkDictionaries){
				self$setDictionaries <- x
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
				
         }, 
		 
		  
		# ------------------------------------------------------------------------------------------- # 
		# // Part 4: Public Getters                                                                   #
		# ------------------------------------------------------------------------------------------- # 
		getSettings   = function(){ return(self$Settings) },  
		getRawData 	  = function(){ return(self$Data$Raw) }, 
		              
		getData       = function(x){ 
		
			if(x %in% c("raw","Raw")){
				return(self$Data$Raw)
			}
			
			if(x %in% c("global","Global", "master","Master")){
				return(self$Data$Global)
			}
			
			# Otherwise:
			return(self$Data$Global[[x]]) 
		},
		
        getLabels               = function(){ return(self$Labels) }, 
        getFeatures             = function(){ return(self$Features) }, 
        getProperties           = function(){ return(self$Properties) }, 
        getDictionaries         = function(){ return(self$Dictionaries) }, 
		
		getRequiredDatasets     = function(){ return(self$RequiredDatasets) },		
		getRequiredDictionaries = function(){ return(self$RequiredDictionaries) },
		 
		# ------------------------------------------------------------------------------------------- # 
		# // Part 5:  Methods to calculate required next actions                                      #
		# ------------------------------------------------------------------------------------------- # 
 
		
		Make  = function(){
     
		    #input, dt.graph, dt.paths, fun = NULL, plot = FALSE, verbose=T
		     
		     stop("No data present; please run $setRawData([dataset]) first.")          %if% (self$Data$Raw %>% is.null)
		     stop("No features defined; please run $setFeatureDefinitions(...) first.") %if% (self$FeatureDefinitions %>% length == 0)
		    
		     # Convert the feature-definition list to objects that define the data-transformations that need to be executed. 
		     self$getRequiredDataTransformations() 
		    
		     # Initialize the list to store the output datasets
		     self$Data$List <- list(rep(NA,self$RequiredDataTransformations$full.graph %>% vertex_attr(name = "data.id.current") %>% max))
		     self$Data$List[[1]] <- self$Data$Raw %>% copy
		      
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
                 
                cat("Traversing path: ", path.name, ":\r\n") %if% (verbose)
                 
                # Walk through each node of each path
                for(vertex.name in {self$RequiredDataTransformations$full.paths[[path.name]] %>% V}$name){
               
                    cat("\t processing: ", vertex.name) %if% (verbose)
                    
                    # Derive the ruel from the vertex attribute:
                    tmp.rule <- getRuleFromVertexAttributes(self$RequiredDataTransformations$full.graph, vertex.name)
             
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
                         
                        # Perform function call derived from rule
                        self$Data$List[[d.current]]  <- getCallFromRule(
                            rule       = tmp.rule, 
                            data.input = frogdh$Data$List[[d.previous]]
                        ) 
                        
                        tmp.rule$visited <- 1
                            
                        self$RequiredDataTransformations$full.graph %<>% UpdateVertexAttributes(
                            vertex.name = vertex.name, 
                            new.rule    = tmp.rule, 
                            verbose     = F
                        ) 
                        
                    }
                }
                 
               # cat("\r\n") %if% (verbose)
                 
             } 
            
             return(T)
        }, 
		 
   
#         createRequiredDatasets = function(){
#  
#             # Print output to be user friendly 
#             cat("\r\n\r\n# Attempting to create the following datasets: \r\n\r\n")
#             print(self$Data$Requirements)
#             cat("\r\n\r\n")
#             
#             
#             cat("\r\n\r\n# Create a temporary container to store the data.tables")
#             datasets.container <- list(rep(NA,nrow(self$Data$Requirements))) 
#              
#             
#             # ---------------------------------------------------------------------------------------------------------- #
#             # Workflow:                                                                                                  #
#             # ---------------------------------------------------------------------------------------------------------- #
#             #   [1] Determine the properties (token, degree, etc.)                                                       #
#             #   [2] Exclude any a priori items from the dataset (as defined in dataset.exclude and dataset.exclude2)     #
#             #   [3] Apply required transformation on the dataset                                                         #
#             #   [4] Filter the transformed dataset                                                                       #
#             # ---------------------------------------------------------------------------------------------------------- #   
#             # Loop across datasets
#             
#             cat("\r\n\r\n")
#             
#             for(i in 1:nrow(self$Data$Requirements)){ 
#          
#                 cat("Create dataset: ", i, "\r\n")
#                 
#                 # For readability, assign the following properties to local variables            
#                 datasource                                <- self$Data$Requirements[i, datasource]              
#                 dataset.token                             <- self$Data$Requirements[i, dataset.token]           
#                 dataset.token.transformed                 <- self$Data$Requirements[i, dataset.token.transformed]
#                                                                       
#                 degree                                    <- self$Data$Requirements[i, degree]       
#                                                                       
#                 # To-be-merged databases:                                       
#                 synset.database                           <- self$Data$Requirements[i, synset.database]          
#                 hyperonym.database                        <- self$Data$Requirements[i, hyperonym.database]       
#                 property.database                         <- self$Data$Requirements[i, property.database ]       
#              
#                 # Define the proximity criterium (should be 'sentence' or 'syntactic')
#                 proximity.criterium                       <- self$Data$Requirements[i, proximity.criterium]      
#             
#                 # Define potential excludes that need to be applied before any data transformations 
#                 dataset.exclude                           <- self$objFeatures[[i]]$getDataExclude()
#                 dataset.exclude.frequency                 <- self$objFeatures[[i]]$getDataExcludeFrequency()
#                 dataset.select                            <- self$objFeatures[[i]]$getDataSelect()                
#                                                           
#                 # Define potential excludes that need to be on the transformed dataset
#                 dataset.transformed.exclude               <- self$objFeatures[[i]]$getTransformedDataExclude()
#                 dataset.transformed.exclude.frequency     <- self$objFeatures[[i]]$getTransformedDataExcludeFrequency()            
#                 
# 				dataset.transformed.select                <- self$objFeatures[[i]]$getTransformedDataSelect()               
#                         
#                  
#                 # Check whether data is of type frog
#                 if(datasource != "frog"){    
# 				
#                     cat("Invalid data source")   
# 					
#                 } else {   
# 				
#                     if(!(dataset.token %in% c("word","lemma","majorpos","pos","synset","hypernym", "ngram"))){   
#                       
# 						cat("Invalid token")                          
# 						
#                     } 
#                     else {    
#                         
# 						# Copy the original raw dataset to a temp object
#                         tmpdata <- copy(self$Data$Raw)
#                             
# 						# -------------------------------------------------------------------------#
# 						# Filter steps on the input datasets:                                      #
# 						# -------------------------------------------------------------------------#
# 						
# 						#   (1) Filter uncommon words from the input dataset, if required
# 						if(!is.null(dataset.exclude.frequency)){ 
#                            tmpdata <- filter_frog_frequency(
#                                dtfrog    = tmpdata, 
#                                ndocs     = uniqueN(self$Data$Raw$docid),
#                                token     = dataset.token,
#                                exclude   = dataset.exclude.frequency
#                            ) 
#                         }
#                          
# 						#   (2) Filter uncommon words from the input dataset, if required
# 						 
# 						 
#                           
#                         # Filter 3 - based on exclusion of content / pos, etc. 
#                         if(!is.null(dataset.exclude)){  
# 							cat("Apply exclude filter") 
# 							tmpdata <- filter_frog( tmpdata, exclude   = dataset.exclude  	)   
#                         }                          
#                          
#                         # Filter 3 - based on content / pos, etc.  
#                          
#                          
#                          
#                         if(degree < 0){  
#                             cat("Degree should be equal to or larger than zero") 
#                         } else {
#             
#                             # First determine what type of token we need to extract from the data: 
#                             
#                             # (a) Single-unit tokens that are directly available in the frog data object (provided for reference only): 
#                             if(dataset.token %in% c("word","lemma", "pos","majorpos"))  {
#                             
#                             }
#                             
#                             # (b) Single- or multi-unit tokens that require merges: 
#                             if(dataset.token.transformed == "synset")            { tmpdata <- merge_synsets(dtfrog = tmpdata, synset.database = synset.database) }  
#                             if(dataset.token.transformed == "hyperonym")         { tmpdata <- merge_hyperonyms(dtfrog = tmpdata, synset.database = get(hyperonym.database, envir = .GlobalEnv))  } 
#                             if(dataset.token.transformed == "lemma_pos_property"){ tmpdata <- merge_lemma_pos_properties(dtfrog = tmpdata, property.database = get(property.database, envir = .GlobalEnv))} 
#                             
#                             # Second, determine the proximity criterium (currently 'sentence' or 'syntactic')
#                             if(!proximity.criterium %in% c("sentence", "syntactic")){
#                                 cat("No (valid) proximity criterium provided; defaulting to 'sentence')\r\n")
#                                 proximity.criterium <- "sentence"
#                             } 
#                             
#                             # Create regular n-grams if degree > 0 and proximity-criterium is "sentence"
#                             if(proximity.criterium == "sentence"){ 
#                                  if(degree == 0){ # tmpdata <- tmpdata (do nothing; the data is already ok)
#                                  } else {  
#                                     tmpdata <- create_ngram_fast(  
#                                         dtfrog                      = tmpdata, 
#                                         degree                      = degree,
#                                         dataset.token               = dataset.token,    
#                                         dataset.token.transformed   = dataset.token.transformed,                                             
#                                         exclude                     = dataset.transformed.exclude,                           
#                                         exclude.frequency           = dataset.transformed.exclude.frequency,
#                                         select                      = dataset.transformed.select
#                                     )  
#                                 }       
#                             } 
#                             
#                             # Create regular n-grams if degree > 0 and proximity-criterium is "sentence"
#                             if(proximity.criterium == "syntactic"){ 
#                                 if(degree == 0){
#                                     cat("\r\n\r\nInvalid; syntactic proximity is only supported for bigrams; degree should be 1 (!)\r\n\r\n")
#                                 } else {
#                                     tmpdata <- merge_syntactic(
#                                         dtfrog              = tmpdata, 
#                                         token               = dataset.token, 
#                                         exclude                  = dataset.transformed.exclude,             
#                                         exclude.frequency   = dataset.transformed.exclude.frequency,
#                                         select              = dataset.transformed.select
#                                     )
#                                 } 
#                             }  
#                         } 
#                         print("about to save..") 
#                         datasets.container[[i]] <- tmpdata 
#                     } 
#                 }  
#             } 
#             
#             self$Data$Transformed <- datasets.container 
#         },
         
        # createRequiredDictionaries = function(){
        # 
        #     # Make a dictionary per feature class; this is somewhat redundant, 
        #     # but it allows us to filter per feature class (and make very custom dictionaries)
        #     
        #     dict_to_make   <- self$Dictionaries$Requirements
        #     datasets       <- self$Data$Requirements
        #      
        #     dictionaries.container <- list(rep(NA,nrow(dict_to_make)))
        # 
        #     for(i in 1:nrow(dict_to_make)){    
        #         
        #         print(dict_to_make)
        #                           
        #         dataset.token                   <- datasets[i, dataset.token]  
        #         dataset.token.transformed       <- datasets[i, dataset.token.transformed]  
        #         
        #         dictionary.token                <- dict_to_make[i, dictionary.token]
        #         
        #         prefix                          <- dict_to_make[i, dictionary.prefix]        
        #          
        #         degree                          <- dict_to_make[i, degree]  
        #         override                        <- dict_to_make[i, override] 
        #                                         
        #         synset.database                 <- dict_to_make[i, synset.database]  
        #                                         
        #         proximity.criterium             <- dict_to_make[i, proximity.criterium]  
        #                                         
        #         data.raw                        <- self$Data$Raw
        #         data.transformed                <- self$Data$Transformed[[i]]
        #                                     
        #         dictionary.exclude              <- self$Dictionaries$Settings$Exclude[[i]]
        #         dictionary.exclude.frequency    <- self$Dictionaries$Settings$ExcludeFrequency[[i]]
        #     
        #         if(!(override == FALSE)){  
        #         
        #             tmpdictionary <- get(override, envir = .GlobalEnv)  
        #             
        #         } else { 
        #             
        #             tmpdictionary <- NULL  
        #             
        #             if(!(dataset.token %in% c("word","lemma","majorpos","pos","synset","hypernym", "ngram"))){  
        #             
        #                 cat("Invalid token")   
        #                 
        #             } else {
        # 
        #                 if(degree < 0){
        #                     cat("Degree should be equal to or larger than zero") 
        #                     
        #                 } else {
        #                     
        #                     tmpdictionary <- cDictionary$new(  
        #                         
        #                         datasource.type                  = "frog", 
        #                         datasource.name                  = "data" %+% i,
        #                         datasource                       = data.transformed, 
        #                         
        #                         degree                           = degree,
        #                         dataset.token                    = dataset.token,
        #                         dataset.token.transformed        = dataset.token.transformed,
        #                         
        #                         dictionary.token                 = dictionary.token,
        #                         prefix                           = prefix,
        #                         
        #                         dictionary.exclude               = dictionary.exclude,
        #                         dictionary.exclude.frequency     = dictionary.exclude.frequency,
        #                         description                      = NULL,
        #                         dictionary.name                  = NULL,
        #                         dictionary.options               = dict.options.strict,
        #                         synset.database                  = synset.database,
        #                         hyperonym.database               = NULL 
        #                     
        #                     ) 
        #                     
        #                 }  
        #             }
        #         } 
        #         
        #         dictionaries.container[[i]] <- tmpdictionary
        #         
        #     }
        #  
        #     cat("        Assign dictionaries to self")
        #     self$Dictionaries$Raw <- dictionaries.container
        # 
        # }, 
         
        getRequiredDataTransformations =  function(FeatureDefinitions = NULL){
            
            if(FeatureDefinitions %>% is.null & self$FeatureDefinitions %>% is.null){
                stop("No features defined; cannot calculate required datatransformations.")
            }
            
            if(FeatureDefinitions %>% is.null & self$FeatureDefinitions %>% is.null %>% not){
                
                FeatureDefinitions <- self$FeatureDefinitions
                
                if(self$RequiredDataTransformations %>% is.null %>% not){
                    cat("Warning: data transformations already provided; overriding previously specified settings.\r\n")
                } 
                
            }
            
            self$RequiredDataTransformations <- getRequiredDataTransformations(FeatureDefinitions = FeatureDefinitions)
            
        },
        
		getDataTransformationCalls = function(RequiredDataTransformations = NULL){
		    
		     
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
                Settings   = self$Settings,
                objFeatures  = self$objFeatures, 
                Dictionaries = self$Dictionaries
            )
        
        },
        
        loadMethod = function(file){
        
            list(
            
            
            )
        
        },
         
        get_document_properties = function(dtfrog                       = NULL, 
                                           document.properties			= NULL, 
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
            
        }, 
  
        extractFeatures = function(datasets = NULL, objFeatures = NULL, exclude = c()){
     
            features     <- list(rep(NA, length(self$makeFeatures)))
          
            for(i in 1:length(objFeatures)){
            
                cat("Calculating feature: ", i)
            
                if(i %in% exclude){
                
                } else { 
                    features[[i]] <- objFeatures[[i]]$calculate(dtfrog     = self$Data$Transformed[[i]], 
                                                                dictionary = self$Dictionaries$Raw[[i]]$entities)
                }
            } 
			
            self$Features <- features
			
        },
         
        createFullFeatureDescriptions = function(){
            
            dt.feat   <- self$FeatureDescriptions
            dt.data   <- self$DataRequirements
            dt.dict   <- self$DictionaryRequirements
            
            m1 <- merge(x = dt.feat, y = dt.data, by = "featureID", allow.cartesian = T)  
            m2 <- merge(x = m1, y = dt.dict, by = "featureID", allow.cartesian = T, suffixes = c("_dat","_dict")) 
            
           self$FeatureDescriptions <- m2
            
        },
        
        summarizeDictionaries = function(){
  
        } 
          
    ),          
        
    
    private = list( 
        
        checkRawData = function(x){
            TRUE %>% return
        },
        
		checkSettings = function(x){
			TRUE %>% return
		},
		
		checkDataset = function(x){
			TRUE %>% return
		},
		
		checkDictionaries = function(x){
			TRUE %>% return
		},
		
        valid.frog.columns   = c("docid","sent","pos","majorpos","morph"),
        valid.frog.columns2  = c("docid","sent","pos","majorpos","morph"),        
        
        
        valid.values.predefined = list(
        
            ngrams = list(
        
                tokens               = list(allowed    = c("word", "lemma", "morph", "pos","majorpos"), notallowed = c("docid")),
                adjacency.criteria   = list(allowed    = c("sentence"),notallowed = c()),
                degrees              = list(1,2)
                
            ),
            
            dtfrog = list(
            
            
            ) 
        ) 
    )
)
    
    
    
     
        
