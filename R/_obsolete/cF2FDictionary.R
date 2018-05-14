
cDictionary <- R6Class("F2FDictionary", 
                       
      	public = list( 
      	    
      	    Input           = NULL, 
      	    Transformations = NULL,
      	    Data            = NULL,
      	     
      	    initialize = function(...){ 
      	         
      	        # Derive properties from whatever has been provided
      	        private$Properties %<>% modifyList(list(...)[names(list(...)) %intersect% names(private$Properties)]) 
      	        
      	        # Set the data object, if specified: 
      	        (self$setData(list(...)[["Data"]])) %if% ("Data" %in% (list(...) %>% names))
      	        
      	    },
      	     
      	     
      	    
      	    setData = function(x){
      	         
      	        stop("Data should be a data.table or data.frame") %if% (class(x) %notin% c("data.table","data.frame"))
      	        stop("Data should contain at least: docid, sent, position, and a token.") %ifnot% c("docid", "position", "sent") %allin% colnames(x)
      	        
      	        self$Data <- x
      	    },
      	    
      	    setName = function(x){
      	        private$Properties$Name <- x
      	    },
      	    
      	    setPrefix = function(x){
      	        private$Properties$Prefix <- x
      	    },
      	    
            setDescription = function(x){
      	        private$Properties$Description <- x
      	    },
      	    
      	    getName = function(x){
      	        private$Properties$Name %>% return
      	    },
      	    
      	    getPrefix = function(x){
      	        private$Properties$Prefix %>% return
      	    },
      	    
            getDescription = function(x){
      	        private$Properties$Description %>% return
      	    },
            
            getProperties = function(x){
                private$Properties %>% return
            },
            
            Make = function(){
                
                
            }, 
            
            Apply = function(Data = NULL, Return = NULL){
                  
            }
             
      	     
      	),
      	
      	private = list(
      	    
      	    Properties = list(
      	        Name        = NULL,
      	        Prefix      = NULL,
      	        Description = NULL 
      	    )  
      	    
      	)
                       
                       
)




















#' 
#' 
#' create_dictionary <- function(...){ 
#' 	dict <- cDictionary$new(dictionary.name = dictionary.name ,
#' 							dictionary.type = dictionary.type, 
#' 							datasource.type = "frog",
#' 							datasource      = datasource)  
#'     return(dict)   
#' } 
#' 
#' #' Dictionary class definition
#' #' @import data.table
#' #' @import magrittr
#' #' @import R6 
#' cDictionary <- R6Class("dictionary",
#' 
#' 	public = list( 
#' 	
#' 		description                           = NULL,
#' 		logbook                               = NULL,
#' 		
#' 		dictionary.options                    = NULL, 
#' 		dictionary.exclude 					  = NULL,
#' 		dictionary.exclude.frequency 		  = NULL,
#' 		
#' 		dataset.exclude 					  = NULL,
#' 		dataset.exclude.frequency 		      = NULL,
#' 		
#'         dictionary.token                      = NULL,
#' 		dataset.token                         = NULL,
#' 		dataset.token.transformed             = NULL,
#'         
#' 		degree                                = NULL,
#' 		
#' 		synset.database                       = NULL,
#' 		hyperonym.database                    = NULL,
#'         
#' 		entities                              = NULL, 
#' 		entities_retained		              = NULL, 
#' 		entities.list                         = NULL,
#' 		
#' 		prefix                                = NULL,
#' 		frog.columns.to.retain                = NULL,
#' 		
#' 		entity_properties                     = NULL,
#'         
#' 		entity_relations	                  = NULL,
#' 		entity_relation_types                 = NULL,
#' 		entity_relation_properties            = NULL, 
#' 		
#' 		datasource                            = NULL,	
#' 		datasource.type                       = NULL,		
#' 		
#' 		initialize = function(datasource.type               = "frog", 
#' 		                      datasource.name               = "data",
#' 							  datasource                    = NULL, 
#' 							  degree                        = 0,
#' 							  dataset.token                 = NULL,
#' 							  dataset.token.transformed     = NULL,
#' 							  dictionary.token	            = NULL,
#' 							  prefix                        = NULL,
#' 							  exclude                       = NULL,
#' 							  exclude.frequency             = list(ll = 0, ul = 1),
#' 							  dataset.exclude               = NULL,
#' 							  dataset.exclude.frequency     = list(ll = 0, ul = 1),
#' 							  dictionary.exclude            = NULL,
#' 							  dictionary.exclude.frequency  = list(ll = 0, ul = 1),
#' 							  description                   = NULL,
#' 							  dictionary.name               = NULL,
#' 							  dictionary.options            = dict.options.strict,
#' 							  synset.database               = NULL,
#' 							  hyperonym.database            = NULL){
#' 			 
#'    			self$dataset.token    		        <- dataset.token    %>% unlist %>% c 
#' 			self$dataset.token.transformed      <- dataset.token    %>% unlist %>% c 
#' 			                                    
#' 			self$dictionary.token 		        <- dictionary.token %>% unlist %>% c 
#' 							                    
#' 			self$dataset.exclude  		        <- dataset.exclude  
#' 			self$dataset.exclude.frequency      <- dataset.exclude.frequency 
#' 			 
#' 			self$degree    	        			<- degree
#' 			 
#' 			self$dictionary.exclude  		    <- dictionary.exclude  
#' 			self$dictionary.exclude.frequency   <- dictionary.exclude.frequency 
#' 			  
#' 			self$dictionary.options <- dictionary.options
#' 			
#'             if(self$dictionary.options$use_log){
#' 				self$logbook <- cLogBook$new(
#' 					name        = "Dictionary logbook", 
#' 					description = "Document the steps taken in the creation of the dictionary object."
#' 				) 
#' 			} 
#' 			 
#' 			cat("\r\n\r\n\tCreate a dictionary with the following properties:  \r\n")
#' 			cat("\t---------------------------------------------------   \r\n")
#' 			cat("\tName:  \t",           dictionary.name, "\r\n") 
#' 			cat("\tDescription:  ",      description, "\r\n")
#' 			cat("\tDatasource token:  ", dataset.token, "\r\n")
#' 			cat("\tDictionary token:  ", dictionary.token, "\r\n") 
#' 			cat("\t\r\n  ")
#' 			cat("\tExcludes:\r\n") 
#' 			cat("\t [-] dictionary.exclude \t= ");print(dictionary.exclude);  
#' 			cat("\t [-] dictionary.exclude.frequency = ");print(dictionary.exclude.frequency); cat("\r\n")
#' 			cat("\t--------------------------------------------------  \r\n\r\n")
#' 			self$datasource <- datasource
#' 			
#' 			# Check eerst of de dictionary wel aangemaakt kan worden: 
#' 	 
#' 			cat("\r\n\t\tCreating ", self$dataset.token %>% unlist %>% c, "dictionary. \r\n")
#' 			# Set the dictionary according to the appropriate entity type 
#' 			
#' 			(private$setAsWordDictionary()) 	%if% (self$dataset.token == "word")
#' 			(private$setAsLemmaDictionary())    %if% (self$dataset.token == "lemma")
#' 			(private$setAsMajorposDictionary()) %if% (self$dataset.token == "majorpos") 
#' 			(private$setAsPosDictionary())      %if% (self$dataset.token == "majorpos")  
#' 			(private$setAsSemanticDictionary()) %if% (self$dataset.token  %in% c("synset", "hypernyms"))
#' 			  
#' 			if(is.data.table(datasource)){  
#' 			
#' 				if(datasource.type %in% c("frog")){ 
#' 				
#' 					#cat("Frog data detected; can continue.\r\n") 
#' 					
#' 					private$setAsFrogDataSource(datasource.type)  
#' 					
#' 					if(self$dataset.token %in% c("word","lemma", "majorpos", "ngram", "majorpos", "synset", "hyperonym")){ 
#' 					
#' 						# All entities
#' 						self$extract_entities_from_frog(datasource) 
#' 					
#' 						# Retained entities
#' 						self$entities_retained <- self$entities
#' 					} 
#' 				} else {
#' 					if(c("docid","text") %allin% colnames(datasource)){
#' 					
#' 					}
#' 				}						
#' 			} else{  
#' 				print("Warning: no entries to enumerate. Did you intend to create an empty dictionary?")		 
#' 			}   
#' 			invisible(self)
#' 		},   
#' 		
#' 		get.valid.dictionary.types = function(...){
#' 			self$get.valid.types(...) 
#' 		},
#' 		
#' 		get.valid.types = function(dictionary.type = NULL, 
#' 								   datasource.type = NULL,
#' 								   print = TRUE, ...){ 
#' 		
#' 				filter_expresson <- list(
#' 					("dictionary.type  == \"" %+% dictionary.type %+% "\"") %ifnotnull% (dictionary.type),
#' 					("datasource.type  == \"" %+% datasource.type %+% "\"") %ifnotnull% (datasource.type)
#' 				)  %>% 	Filter(. %>% is.null %>% `!`, .)  %>% 
#' 					     unlist %>% paste0( collapse = " & ")  %>% as.expression  %>% parse(text = .)
#' 	  
#' 				if(print){
#' 					print(private$allowed_dictionaries[filter_expresson %>% eval,])
#' 					invisible(self)
#' 				} else { 
#' 					return(private$allowed_dictionaries)
#' 				}  		
#' 			},
#' 		
#' 		
#' 		is.similar.to = function(y,x = NULL){
#' 		 
#' 			if(is.null(x)){
#' 				if(exists("self")){
#' 					x <- self$entities
#' 				} else {
#' 					print("Not instantiated!") 
#' 				} 
#' 			} 
#' 		 
#' 			# Match on class type
#' 			ch1 <- ("dictionary" %in% class(x)) & (class(x) %identical% class(y)) 
#' 			   	   
#' 				   (print("Error. Both x and y must be dictionary objects")) %ifnot% (ch1)
#' 		 
#' 			ch2 <- (x$token == y$token)  
#' 
#'                    (print("Error. Both x and y must be the same dictionary type.")) %ifnot% (ch2)
#' 			 
#' 			ch3 <- (x$degree == y$degree)            # Match on degrees (i.e., whether n.gram or not)
#' 	  
#' 			return(ch1 & ch2)			
#' 			 
#' 		},
#' 		
#' 		
#' 		# Simple function to subset the entities of the current dictionary
#' 		subset = function(y, x = NULL, by = NULL, by.x = NULL, by.y = NULL, all.x = FALSE, all.y = FALSE){
#' 			 
#' 			# The default subset is an inner join (only entities both in x and y are returned)
#' 			 	 
#' 			return.subsetted <- FALSE
#' 			 
#' 			if(!is.data.table(y)){
#' 				if(!is.vector(y)){
#' 					if(!is.dictionary(y)){
#' 						print("Error: y is not a data.table object")
#' 						break;
#' 					} else { 
#' 						if(x %similar% y){
#' 							y <- y$entities
#' 						} else {
#' 							print("Dissimilar dictionaries.")
#' 						}
#' 					}
#' 				} else { 
#' 					y <- NA %>%      							              # Start with a missing value
#' 					     c  %>%  								              # Make it a vector 
#' 						 as.character %>%                                     # Make it (explicitly) a character vector  
#' 						 matrix(ncol = 1, nrow=length(y)) %>%                 # Make it a matrix
#' 						 data.table 	%>%                                   # Make it a data.table 
#' 						 set_colnames(dict.words$token)  %>%        # Fix column name
#' 						 set(j = dict.words$dictionary.type, value = y)       # Input the actual values
#' 				}
#' 			} else {	
#' 			
#' 				if(is.null(by) & is.null(by.x) & is.null(by.y)){
#' 				
#' 					print("Error: by and by.x are both not provided; please provide the column to subset the dictionary on.")				
#' 					break;
#' 					
#' 				}  else { 
#' 				
#' 					if(!is.null(x)){
#' 						print("Error: x is not a data.table object")		    
#' 						break;
#' 					}
#' 				
#' 					if(!is.data.table(self$entities)){
#' 						print("Error: y is not a data.table object") 
#' 						break;
#' 					} 
#' 				}
#' 			}
#' 			
#' 			by.column = self$dictionary.type
#' 			
#' 			retval <- data.table:::merge.data.table(x = self$entities, 
#' 													y = y[,c(self$dictionary.type), with=F],
#' 													by = by.column, all.x = all.x, all.y = all.y)
#' 			
#' 			if(return.subsetted) {
#' 				return(retval)
#' 			} else {
#' 				self$entities  <- retval
#' 			} 
#' 			
#' 			self %>% invisible  
#' 		},
#' 		
#' 		join = function(dictionary, type = NA, keep.x = FALSE, keep.y = FALSE){
#' 			
#' 			# x = the current dictionary;
#' 			# y = the subsetted dictionary 
#' 			
#' 			entities_selected
#' 		
#' 			self %>% invisible %>% return
#' 		},
#' 		
#' 		join.left = function(dictionary){ 
#' 			self %>% invisible %>% return
#' 		},
#' 		
#' 		join.right = function(dictionary){   
#' 			self %>% invisible %>% return
#' 		},
#' 		 
#' 		extract_entities_from_data.table = function(dtable){
#' 		
#' 			# Not yet fully implemented: bag-of-words approach
#' 	    	self$source_column_namesource_column_name <- "document" 
#' 			
#' 	    	if(!is.null(self$entity_properties)){
#' 	    		cols <- c(self$source_column_name,c(self$entity_properties)) %>% unique
#' 	    	} else {
#' 	    		cols <- c(self$source_column_name)
#' 	    	}   
#' 	    	 
#' 	    	self$entities <- dtable[,c(cols), with=F] %>% FixStringsFactors %>% unique
#' 			
#' 	    	self$enumerate_entities(prefix=prefix) 
#' 			 
#' 		},
#' 		 
#' 	    extract_entities_from_frog = function(dtfrog){ 
#'  
#' 	        cat("Extract entities from frog:")
#' 	        
#' 	    	if(!is.null(self$entity_properties)){ 
#' 	    		cols <- union(self$dictionary.token, c(self$entity_properties)) %>% unique 
#' 	    	} else { 
#' 	    		cols <- self$dictionary.token %>% unlist %>% c
#' 	    	}    
#' 			 
#' 	        if(is.null(self$frog.columns.to.retain)){
#' 	            self$frog.columns.to.retain <- self$dictionary.token
#' 	        } 
#'  
#' 	    	self$entities <- dtfrog[, c("docid",self$frog.columns.to.retain), with=F] %>% FixStringsFactors  
#' 			
#' 			self$entities[, ndoc := uniqueN(docid), by = c(self$frog.columns.to.retain)][, docid := NULL]
#' 			
#' 			self$entities <- self$entities %>% unique
#' 			
#' 	    	self$enumerate_entities(prefix=prefix) 
#' 			 
#' 		},
#' 		
#' 	    enumerate_entities = function(prefix = NA){   
#' 	        
#' 			if(!is.null(self$entities)){
#' 			    
#' 				self$prefix <- (prefix) %or_if_absent% (' ') 
#' 			    
#' 			    self$entities[, (self$dictionary.token %+% "Id")   := .I ]
#' 			    
#' 			    self$entities[, (self$dictionary.token %+% "Name") := self$prefix %+% self$dictionary.token %+% .I ]
#' 				
#' 				cols.main <- c(self$dictionary.token %+% "Id",
#' 							   self$dictionary.token %+% "Name",
#' 							   self$dictionary.token ) 
#' 							   
#' 				cols.other <- colnames(self$entities) %-% cols.main 
#' 				 
#' 				cols.order <- c(cols.main, cols.other) 
#' 		     	 
#' 			}
#' 		} 
#' 		
#' 		
#' 	), private = list(
#' 
#' 		allowed_dictionaries  = data.table(
#' 			datasource.type = c("frog", "frog",  "frog",     "frog", "frog.ngram", "frog.ngram","frog.ngram", "frog"),    
#' 			dictionary.type = c("word", "lemma", "majorpos", "pos",  "word", "lemma", "majorpos", "pos" )
#' 		),
#' 	
#' 		setAsWordDictionary = function(){ 
#' 			self$description <- "Word dictionary"  
#'   
#' 			self$logbook$append(action       = "Set dictionary type", 
#' 								description  = "dictionary type: word", 
#' 								functioncall = sys.call())  
#' 			
#' 		},
#' 		
#' 		setAsLemmaDictionary = function(){ 
#' 			self$description <- "Lemma dictionary"  
#'  
#' 			self$logbook$append(action       = "Set dictionary type", 
#' 								description  = "dictionary type: lemma", 
#' 								functioncall = sys.call())  
#' 			
#' 		},
#'         
#' 		setAsPosDictionary = function(){
#' 			self$description <- "Dictionary with all part-of-speech codings"  
#' 	 
#' 			self$logbook$append(action       = "Set dictionary type", 
#' 								description  = "dictionary type: pos", 
#' 								functioncall = sys.call())  
#' 			 
#' 		}, 
#' 		
#' 		setAsMajorposDictionary = function(){
#' 			self$description <- "Majorpos dictionary"  
#' 	 
#' 			self$logbook$append(action       = "Set dictionary type", 
#' 								description  = "dictionary type: majorpos", 
#' 								functioncall = sys.call())  
#' 			 
#' 		},
#' 		
#' 		
#' 		setAsSemanticDictionary = function(){
#' 			self$description <- "Semantic dictionary"  
#' 	 
#' 			self$logbook$append(action       = "Set dictionary type", 
#' 								description  = "dictionary type: semantic", 
#' 								functioncall = sys.call())  
#' 			 
#' 		},
#' 		 
#' 		setAsRelationalDictionary = function(type = NA){
#' 		
#' 			(type = '') %if% (type %>% is.na)
#' 		
#' 			self$logbook$append(action       = "Set as relational", 
#' 								description  = "relation type: " %+% type, 
#' 								functioncall = sys.call())  
#' 		
#' 		},
#' 
#' 		setAsFrogDataSource = function(datasource.type){
#' 		
#' 			(datasource.type = '') %if% (datasource.type %>% is.na)
#' 		
#' 			self$logbook$append(action       = "Set data source as Frog",
#' 								description  = '',
#' 								functioncall = sys.call())  
#' 
#'                                           
#' 		
#' 			if(datasource.type == "frog"){ 
#' 			
#' 				if(self$dataset.token == "word"){
#' 					if(self$degree > 0){
#' 						self$frog.columns.to.retain <- c(self$dataset.transformed.token)
#' 					} else {
#' 						self$frog.columns.to.retain <- c("word","lemma","majorpos")
#' 					}
#'                     
#'                 }
#'                 
#'                 if(self$dataset.token == "lemma"){ 
#' 					if(self$degree > 0){
#' 						self$frog.columns.to.retain <- c(self$dataset.transformed.token)
#' 					} else {
#' 						self$frog.columns.to.retain <- c("lemma","majorpos")
#' 					}  
#'                 }
#'                 
#'                 if(self$dataset.token == "majorpos"){
#' 					if(self$degree > 0){
#' 						self$frog.columns.to.retain <- c(self$dataset.transformed.token)
#' 					} else {
#' 						self$frog.columns.to.retain <- c("majorpos")
#' 					}  			
#'                 } 
#' 				
#' 			}
#' 		
#' 			if(datasource.type == "frog.ngram"){ 
#'                 
#' 				if(self$dataset.token %in% c("word", "lemma", "majorpos")){
#' 			 		self$frog.columns.to.retain <- c("ngram")
#' 				}
#' 				
#' 			}
#' 			
#' 			if(datasource.type == "frog.synset"){   
#' 			
#' 				if(self$dataset.token == "lemma"){
#'                     self$frog.columns.to.retain <- c("synset", "lemmas")
#' 				} 
#' 				
#' 			}
#' 			
#' 			if(datasource.type == "frog.hypernyms"){   
#' 			
#' 				if(self$dictionary.type == "lemma"){
#'                     self$frog.columns.to.retain <- c("synset", "lemmas")
#' 				} 
#' 				
#' 			}
#' 		}
#' 		
#' 
#' 	)
#' )
#' 
#'  