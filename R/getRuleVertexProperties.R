getRuleVertexColor <- function(rule, override = FALSE){
   
    default <- "lightgrey"
    
    # Make sure we always work with a complete rule object, overridden by the user-specified options
    tmp.rule <- getNaRuleList() %>% modifyList(rule,keep.null = T) 
    
    if(rule$vertex.color %>% is.na){
        
        tmp.rule$vertex.color <- switch(tmp.rule$type,
    		input              = "lightgrey",
            identity           = "lightgrey", 
            apply.filter       = "lightsalmon1",
    		replace.chain      = "lightsalmon2",
            create.ngram       = "lightcyan", 
            merge              = "lightblue",    
            create.dictionary  = "sienna2",
            save.dictionary    = "sienna2",
            construct.features = "sienna2", 
            "lightgrey"
        ) 
        
    } 
    return(tmp.rule$vertex.color)   
    
}
    

getRuleVertexSize2 <- function(rule, override = FALSE){
     
    default <- 20
    
    # Make sure we always work with a complete rule object, overridden by the user-specified options
    tmp.rule <- getNaRuleList() %>% modifyList(rule,keep.null = T) 
     
    # Check whether the vertex.name has already been set; if so, do not override (!)
    if(rule$vertex.size2 %>% is.na | override == T){ 
    
        # Provide a sensible default:
        tmp.rule$vertex.size2 <- default
        
        # Override if needed:
        if(tmp.rule$type == "input"){          tmp.rule$vertex.size2 <- 20 }
        if(tmp.rule$type == "identity"){       tmp.rule$vertex.size2 <- default}
        if(tmp.rule$type == "apply.filter"){   tmp.rule$vertex.size2 <- default}
        if(tmp.rule$type == "create.ngram"){   tmp.rule$vertex.size2 <- default}
        if(tmp.rule$type == "apply.filter"){   tmp.rule$vertex.size2 <- default} 
        if(tmp.rule$type == "replace.chain"){  tmp.rule$vertex.size2 <- default} 
 
    }  
    return(tmp.rule$vertex.size2)
}


getRuleVertexSize <- function(rule, override = FALSE){
     
    default <- 50
    
    # Make sure we always work with a complete rule object, overridden by the user-specified options
    tmp.rule <- getNaRuleList() %>% modifyList(rule,keep.null = T) 
     
    # Check whether the vertex.name has already been set; if so, do not override (!)
    if(rule$vertex.size %>% is.na | override == T){ 
    
        # Provide a sensible default:
        tmp.rule$vertex.size <- default
        
        # Override if needed:
        if(tmp.rule$type == "identity"){
            tmp.rule$vertex.size <- 30
        }
        
        if(tmp.rule$type == "apply.filter"){
            tmp.rule$vertex.size <- default
        }
        
        if(tmp.rule$type == "create.ngram"){
            tmp.rule$vertex.size <- default
        }
        
        if(tmp.rule$type == "apply.filter"){
            tmp.rule$vertex.size <- default
        } 
        
        if(tmp.rule$type == "replace.chain"){
            tmp.rule$vertex.size <- default
        } 
        
        # ....
         
    }  
    return(tmp.rule$vertex.size)
}

getRuleVertexShape <- function(rule, override = FALSE){
    
    require(igraph)
    
    default.shape <- "rectangle"
    
    # Make sure we always work with a complete rule object, overridden by the user-specified options
    tmp.rule <- getNaRuleList() %>% modifyList(rule,keep.null = T) 
     
    # Check whether the vertex.name has already been set; if so, do not override (!)
    if(rule$vertex.shape %>% is.na | override == T){ 
    
        # Provide a sensible default:
        tmp.rule$vertex.shape <- default.shape
        
        # Override if needed:
        if(tmp.rule$type == "identity"){
            tmp.rule$vertex.shape <- "rectangle"
        }
        
        if(tmp.rule$type == "apply.filter"){
            tmp.rule$vertex.shape <- "rectangle"
        }
        
        if(tmp.rule$type == "create.ngram"){
            tmp.rule$vertex.shape <- "rectangle"
        }
        
        if(tmp.rule$type == "save.dictionary"){
            tmp.rule$vertex.shape <- "rectangle"
        } 
        
                
        if(tmp.rule$type == "replace.chain"){
            tmp.rule$vertex.shape <- "rectangle"
        } 
        
        # ....
         
    }
    
    # If for some reason the resuling shape is not in igraph's defaults, override with a rectangle
    if(tmp.rule$vertex.shape %in% vertex.shapes() %>% not){
        
        cat("Warning: provided vertex.shape ", tmp.rule$vertex.shape, " is not in iGraph's default list. Overridden with rectangled shape.\r\n")
        tmp.rule$vertex.shape <- default.shape
    
    }
    
    return(tmp.rule$vertex.shape)
}
 

getRuleVertexLabel <- function(rule, override = FALSE){
     
    # Make sure we always work with a complete rule object, overridden by the user-specified options
    tmp.rule <- getNaRuleList() %>% modifyList(rule,keep.null = T) 
 
    # Check whether the vertex.name has already been set; if so, do not override (!)
    if(rule$vertex.label %>% is.na | override == T){ 
      
        if(tmp.rule$type == "identity"){
            tmp.rule$vertex.label <- rule$tag
        }
          
        if(tmp.rule$type == "apply.filter"){
        				    
            if(tmp.rule$count.type %>% is.na %>% not){
        	
                if(tmp.rule$count.type == "frequency"){
                    
                    tmp.rule$vertex.label <- "Exclude " %+% tmp.rule$token  %+% " if DF " %+% 
                                               ifelse(tmp.rule$exclude.what =="inside", "is between\r\n ", "is not between\r\n ") %+% 
                                               "[" %+% tmp.rule$ll %+% ", " %+% tmp.rule$ul %+% "]"
                     
                }
                
                if(tmp.rule$count.type == "proportion"){
					 tmp.rule$vertex.label <- "Exclude " %+% tmp.rule$token  %+% " if %-docs " %+% 
                                               ifelse(tmp.rule$exclude.what =="inside", "is between\r\n ", "is not between\r\n ") %+% 
                                               "[" %+% tmp.rule$ll %+% ", " %+% tmp.rule$ul %+% "]"
                }
                
                if(tmp.rule$count.type == "tf-idf"){
					(tmp.rule$vertex.label <- "Filter tokens\r\ntf-idf[" %+% tmp.rule$ll %+% "%, " %+% tmp.rule$ul %+% "%]")
                } 
            				   
            }
        				    
            if(tmp.rule$items %>% is.na %>% not){
                tmp.rule$vertex.label <- "Filter if " %+% tmp.rule$token %+% " in: \r\n" %+% tmp.rule$tag 
            }   
        }
         
        if(tmp.rule$type == "create.ngram"){ 
            tmp.rule$vertex.label <- "Make ngram\r\n(" %+% tmp.rule$tokens[1]  %+% ", " %+% (tmp.rule$degree + 1) %+% "," %+% substring(tmp.rule$proximity.criterium,1,3) %+% ")" 
        }
 
        
        
        if(tmp.rule$type == "create.dictionary"){
           tmp.rule$vertex.label <- "Create Dictionary\r\n(" %+% tmp.rule$filename  %+% ")"
        }
        
        if(tmp.rule$type == "save.dictionary"){
            tmp.rule$vertex.label <- "Save Dictionary"
        }
        
        if(tmp.rule$type == "construct.features"){
            tmp.rule$vertex.label <- "Construct Features"
        }
        
        if(tmp.rule$type == "load.input"){
             tmp.rule$vertex.label <- "(RE)Load Raw Data"
        }
        
        if(tmp.rule$type == "input"){
           tmp.rule$vertex.label <- "input"
        } 
        
        if(tmp.rule$type == "replace.chain"){
           tmp.rule$vertex.label <- "Replacements (chained)"
        } 
        
        
    }
    
    return(tmp.rule$vertex.label)
}
