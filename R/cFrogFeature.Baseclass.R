 #' cFrogFeature
 #   
 #' @require data.table
 #' @require digest
 #' @require igraph
 cFrogFeature <- R6Class("frogfeature", lock_objects = FALSE,
  
        public = list(
        
            Description = list( 
                Name             = NULL,
                Description      = NULL,
                FeatureClass     = NULL,
                FeatureSubClass  = NULL
            ),         
            
			Settings = list (  
			
				Token	                  = NULL, 
				Token.Transformed         = NULL,
				Token.Merge               = NULL,
				
				Ngram.Degree			  = 0, 
				Ngram.Proximity.Citerium  = "adjacent", 
				
				Datasource.Input          = NULL, 
				Datasource.Merge          = NULL  
				
            ),
 
            DataTransformators = NULL, 
			
            Dictionary = NULL,
            
            initialize = function(Description          = NULL, 
								  Settings             = NULL, 
								  DataTransformators   = NULL, 
								  Dictionary           = NULL){
			 
				# First, copy whatever the user has specified
				(self$Settings            %<>% modifyList(Settings)) %ifnotnull% Settings
                (self$Description          <- Description)           %ifnotnull% Description
				(self$DataTransformators   <- DataTransformators)    %ifnotnull% DataTransformators
                (self$Dictionary           <- Dictionary)            %ifnotnull% Dictionary
                 
                if(DataTransformators %>% is.null){
                    self$initializeDataTransformators()
                }
                
                if(!is.null(Dictionary)){
                    if(Dictionary == TRUE){
                        self$initializeDictionary()
                    }    
                }
                
                invisible(self)
	 
            },
             
			initializeDataTransformators = function(){ 
			      
				self$DataTransformators <- cFrogDataTransformator$new()  
				self$DataTransformators$Add(type = "input")
				
				invisible(self)
				
			},
			
			 
            initializeDictionary = function(){ 
				self$Dictionary <- cDictionary$new() 
	            invisible(self)
			}, 
			
            getDescription = function(type = "list"){ 
                
                (return(self$Description))                   %if% (type == "list")        
                (return(self$Description %>% as.data.table)) %if% (type == "data.table")        
                (return(self$Description %>% as.data.frame)) %if% (type == "data.frame")        
                
            },
			
			getSettings = function(type = "list"){
			    
                (return(self$Settings))                   %if% (type == "list")        
                (return(self$Settings %>% as.data.table)) %if% (type == "data.table")        
                (return(self$Settings %>% as.data.frame)) %if% (type == "data.frame")
			     
			}, 
			
             
            setDescription = function(...){
                self$Description %<>% modifyList(list(...)) 
                invisible(self)
            },
			 
			setSettings = function(...){ 
			    self$Settings %<>% modifyList(list(...)) 
			    invisible(self)
			},
			 
			
            getDataDefinitions = function(){
			
			    x <- data.table(
                    
                    Token                   = self$Settings$Token,
                    Token.Transformation    = self$Settings$Token.Transformed,
                    
                    Degree                  = self$Settings$Token,
                    Proximity.Criterium     = self$Proximity.Criterium,
                    					                                                                                                    
                    Synset.Database         = self$Synset.Database 		, 
                    Hyperonym.Database      = self$Hyperonym.Database  ,     		  
                    Property.Database       = self$Property.Database       	 
                        
                )   %>% return  
			
			
			},
			
			 
            setDictionaryRequirements = function(...){ 
                
                # Set requirements regarding the data source
                self$DictionaryRequirements$override            =  as.character(attr(self$calculate, "dictionary.override"))            %or_if_empty% NA                
                self$DictionaryRequirements$dictionary.token    =  as.character(attr(self$calculate, "dictionary.token"))               %or_if_empty% "lemma"                     
                self$DictionaryRequirements$degree              =  as.numeric(attr(self$calculate,   "degree"))                         %or_if_empty% 0               
                self$DictionaryRequirements$proximity.criterium =  as.character(attr(self$calculate, "proximity.criterium"))            %or_if_empty% "sentence"  
                self$DictionaryRequirements$dictionary.prefix   =  as.character(attr(self$calculate, "dictionary.prefix"))              %or_if_empty% "l_"   
                self$DictionaryRequirements$synset.database     =  as.character(attr(self$calculate, "synset.database"))                %or_if_empty% NA 
                self$DictionaryRequirements$hyperonym.database  =  as.character(attr(self$calculate, "hyperonym.database"))             %or_if_empty% NA 
                        
                self$Dictionary$Settings$Exclude                =  as.character(attr(self$calculate, "dictionary.exclude"))             %or_if_empty% NA 
                self$Dictionary$Settings$ExcludeFrequency       =  as.character(attr(self$calculate, "dictionary.exclude.frequency"))   %or_if_empty% NA 
                                                             
                self$Dictionary$Settings$Include                =  as.character(attr(self$calculate, "dictionary.include"))             %or_if_empty% NA 
                self$Dictionary$Settings$IncludeFrequency       =  as.character(attr(self$calculate, "dictionary.include.frequency"))   %or_if_empty% NA 
                
                invisible(self)
                
            },
         
 
            calculate = NULL 
  
        ), 
         
        private = list( 
		
            checkDescription = function(){
			    any(self$Name            %>% is.null,
                    self$Description     %>% is.null,
                    self$FeatureClass    %>% is.null,
                    self$FeatureSubClass %>% is.null) %>% not %>% return  
			}, 
			
			checkSettings = function(){
			    return(T)    
			}, 
			
			checkSelectors = function(){
			    return(T)    
			},
			
			checkDictionary = function(){
			    return(T)    
			}
			
      ) 
 )
 
 
  
 
 
 
 
 
 
 