serialize_rule <- function(rule){
 
    for(name in names(rule)){   
        if(any(rule[[name]] %>% is.list(),
               rule[[name]] %>% is.atomic %>% not, 
               rule[[name]] %>% is.recursive,
               rule[[name]] %>% length %>% is_greater_than(1),
			   rule[[name]] %>% is.character)){
        
            if(name %in% properties.to.serialize){ 
                rule[[name]] <- {rule[[name]] %>% enquote %>% as.character}[[2]] 
            } 
        }    
    }
    rule %>% return
}


unserialize_rule <- function(rule){
       
    for(name in names(rule)){ 
         
        if(any(rule[[name]] %>% is.list(),
               rule[[name]] %>% is.atomic %>% not, 
               rule[[name]] %>% is.recursive,
               rule[[name]] %>% length %>% is_greater_than(1))){
             
        } else { 
            if(name %in% properties.to.serialize){   
                 try(rule[[name]] <- rule[[name]] %>% parse(text = .) %>% eval, silent=T)  
            } 
        }
         
    } 
    rule %>% return
}
   


is.valid.dt.rule <- function(rule){
    
    if("data.table" %in% (rule %>% class)){
        rule %<>% as.list
    }
    
    if("list" %notin% (rule %>% class)){
        return(FALSE)
    }
    
    # Make an empty rule object
    tmp.rule <- getNaRuleList()
    
    # Replace all provided rule-components
    tmp.rule %<>% modifyList(rule, keep.null = T)

    return(TRUE)
    
} 

is.valid.filter.rule <- function(rule){
    
    # Type 1: filter based on counts 
    type1 <- all(rule %has% "filter.type",       
                 rule %has% "count.type",     
                 rule %has% "include.what",   
    			 rule %has% "ll",             
                 rule %has% "ul" ,            
                 rule %has% "token")          
            
    
    # Type 2: filter based on counts 
    type2 <- all(rule %has% "rule.type",
                 rule %has% "count.type",
                 rule %has% "exclude.what",
                 rule %has% "ll",
                 rule %has% "ul",          
                 rule %has% "token",
                 rule %has% "items")          
     
    return(retval)
}

is.valid.merge.rule <- function(rule){
    
    return(retval)
}

is.valid.ngram.rule <- function(rule){
     
    
    return(retval)
}







