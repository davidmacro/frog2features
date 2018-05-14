    #' cFrogDataSelector
    #   
    #' @require data.table
    #' @require digest
      
    cFrogDataTransformator <-  R6Class("frog2featureDH", lock_objects = FALSE,
     
    	public = list(
    	   
    	    DataTransformators = NULL,
    	    
    		initialize = function(){  
    			self$DataTransformators <- private$getDataTransformationStructure() 
    			invisible(self)
    		},
    	
    		Add = function(type, rule = list(type = "input"), tag = NA, ...){
    		 
    		    invalid.rule <- FALSE
    		      
    		    # Override rule$type with the function=argument's "type" 
    		    (rule$type <- type) %if% (type %>% is.null %>% not)
    		    
    			if((rule$type %notin% getValidRuleTypes()) | rule %>% is.valid.dt.rule %>% not){
    				stop("Incorrectly specified rule.")
    				
    			} else { 
    			
    			    tmp.rule       <- getNaRuleList() %>% modifyList(rule,keep.null = T) %>% serialize_rule 
    			    tmp.rule$order <- nrow(self$DataTransformators) + 1
 
    			    tmp.rule$vertex.label   <- tmp.rule %>% getRuleVertexLabel
    			    tmp.rule$vertex.shape   <- tmp.rule %>% getRuleVertexShape
    			     
    			    tmp.rule$vertex.size    <- tmp.rule %>% getRuleVertexSize
    			    tmp.rule$vertex.size2   <- tmp.rule %>% getRuleVertexSize2  
                    tmp.rule$vertex.color   <- tmp.rule %>% getRuleVertexColor
    				 
    				self$DataTransformators <- rbindlist(l = list(self$DataTransformators, 
    				                                              tmp.rule %>% as.data.table), fill=T, use.names = T) 
    			 	
    			}  
    		    
    		    invisible(self) 
    		    
    		},
    		
    		getCallFromRule = function(rule){
    		    
    		    return(getCallFromRule(rule))
    		     
    		},
    		
    		
    		Finalize = function(do = "calculate_count", construct.features = T, output = T,  feature.type = "count", ...){
                
		    
    		    use.dictionary <- ("dictionary" %in% (formals(get(do)) %>% names))
    		   
    		    if(use.dictionary){ 
                    if(self$DataTransformators[.N, type == "create.dictionary"]){  
                        self$Add(type = "save.dictionary", rule=list(
                            prefix       = self$DataTransformators[.N, prefix],
                            filename     = self$DataTransformators[.N, filename],
                            allow.replay = 0
                        ))   
                    }
    		    }
    		    
    		    if(construct.features){ 
    		        if(self$DataTransformators[type == "construct.features", .N] == 0){
        		        
    		            rule <- list(type         = "construct.features",
            		                 feature.type = feature.type,
            		                 allow.replay = 1,
            		                 do           = do  
                        ) %>% modifyList(list(...))
    		            
                         self$Add(type = "construct.features", rule = rule)  
                        
    		        }
    		    }
 
    		    
    		    if(output){
                    if(self$DataTransformators[type == "output", .N] == 0){
    		            self$Add(type = "output")   
    		        } 
    		    } 
    		    
                invisible(self) 
                
            },
    		
    		getDataTransformators = function(){  
    			self$DataTransformators %>% return 
    		} 
    		
            	 
    	),
    	
    	private = list (
     	
    	
    		checkValidMergeRule = function(rule){	
    			
    			# Type 1 = synonym class 
    			type1 <- all(rule$merge.type == "synonym",
    						 rule %has% "synonym.database",
    						 rule %has% "token", 
    						 rule %has% "token.merged")  
     
    			
    			# Type 2 = hyperonym class
    			type2 <- all(rule$merge.type == "hyperonym",
    						 rule %has% "hyperonym.database",
    						 rule %has% "token", 
    						 rule %has% "token.merged")  
    	 
    			# Type 3 = property class
    			type3 <- all(rule$merge.type == "property",
    						 rule %has% "property.database",
    						 rule %has% "token", 
    						 rule %has% "token.merged")  
    	 
    
    			return(type1 %xor% type2 %xor% type3)
    		  
    		},
    	 
    		checkValidNgramRule = function(rule){	
    			
    			# Type 1 = proximity based on adjacency
    			type1 <- all(rule$proximity.criterium == "adjacency",
    						 rule %has% "token",
    						 rule %has% "degree")  
    	 
    			
    			# Type 2 = proximity based on adjacency
    			type2 <- all(rule$proximity.criterium == "syntactic",
    						 rule %has% "token",
    						 rule %has% "degree")  
    			 
    
    			return(type1 %xor% type2)
    		  
    		},
    	  
            getDataTransformationStructure = function(){
                return( getEmptyRuleList() %>% as.data.table ) 
            } 
    	)
     )
