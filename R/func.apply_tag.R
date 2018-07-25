# --------------------------------------------------------------------------------
#      File: func.apply_tag.R
# --------------------------------------------------------------------------------
#    author: D.A. Macro (d.a.macro@minvenj.nl)
#      date: 2018-07-05 
#  
# --------------------------------------------------------------------------------
#   Purpose: Define the apply_tag function
#
# -------------------------------------------------------------------------------- 

# -------------------------------------------------------------------------------- 
# Introduction: 
# --------------------------------------------------------------------------------
#   Tagging is the (simple) process of assigning '1' to rows in a Frogged 
#   dataset that match a certain condition, and '0' to rows that don't.
# 
#   Many feature-construction tasks can be broken down into tagging and
#   aggregating tasks. 
# 
#   Consider the following examples: 
#
#   (1) Calculate the number of nouns per document
#   (2) Calculate the average number sentences with a subject, object, and verb. 
#   
# -------------------------------------------------------------------------------- 

apply_tag <- function(dtfrog, 
					  where.token = "lemma",
                      where.data.table.expression = NULL,
                      where.regex                 = NULL,
                      where.list                  = NULL,
                      entities                    = NULL, 
                      keywords                    = NULL,
                      tag.column.name             = "is_tagged",  
                      casefold                    = 0,
                      tagged.value                = 1,
                      tagged.value.not            = 0,
                      tagged.value.expression     = NA,
                      tagged.value.expression.not = NA,
                      match.type                  = "match.expression",
                      set.not                     = 1,
                      type                        = "apply.tag"){
     
    cat("\r\n\t Apply tag to frog object:  ") 
    cat(match.type %+% ": " %+% where.regex)                 %if% (where.regex %>% is.null %>% not)
    cat(match.type %+% ": " %+% where.data.table.expression) %if% (where.data.table.expression %>% is.null %>% not)
    
    
    # First process the recursive case:  
    if(match.type == "match.entity.list"){
         
        fname         <- "apply_tag" 
        
        dt.tmp <- dtfrog %>% copy
     
        entities[, set.not := 0]
        entities[, group := .GRP, by = tag.column.name][, by = "group"]
        entities[, set.not :=  .SD[,.I == 1]  * 1.0 , by = "group"]
        
        for(i in 1:nrow(entities)){
        
            entity <- entities[i] %>% as.list %>% lookupArgs(fname, .)
            
            dt.tmp <- apply_tag(dtfrog                      =  dt.tmp,
                                where.token                 =  entity$where.token,
                                where.data.table.expression =  entity$where.data.table.expression,
                                where.regex                 =  entity$where.regex,
                                where.list                  =  entity$where.entities,
                                entities                    =  entity$entities, 
                                keywords                    =  entity$keywords,
                                tag.column.name             =  entity$tag.column.name,  
                                casefold                    =  entity$casefold,
                                tagged.value                =  entity$tagged.value,
                                tagged.value.not            =  entity$tagged.value.not,
                                tagged.value.expression     =  entity$tagged.value.expression,
                                tagged.value.expression.not =  entity$tagged.value.expression.not,
                                match.type                  =  entity$match.type,
                                set.not                     =  entity$set.not) 
        }  
        
        return(dt.tmp)
        
    }  
    
	if(match.type == "match.exact.value"){
	
	
	}
	
    if(match.type == "match.data.table.expression"){
        
          c1 <- (where.data.table.expression %>% is.null)
          c2 <- (tag.column.name             %>% is.null)
          c3 <- (tagged.value                %>% is.null)
          c4 <- (tagged.value.expression     %>% is.na)
          c5 <- (tagged.value.not            %>% is.null)
          c6 <- (tagged.value.expression.not %>% is.na)
         
          if(any(c1, c2, (c3 %xor% c4 ) %>% not, (c5 %xor% c6 ) %>% not)){  
              stop("Invalid argument(s). Match variant \"match.data.table.expression\" requires: \r\n 
                   \t \"where.data.table.expression\" \r\n
                   \t \"tag.column.name\" \r\n
                   \t \"tagged.value\" or \"tagged.value.not.expression\"\r\n
                   \t \"tagged.value.not\" or \"tagged.value.not.expression\"\r\n")

          } else {  
              
              if(set.not){
                  dtfrog[, (tag.column.name) := tagged.value.not]
              }  
              
             return(
                 dtfrog[
                    where.data.table.expression %>% parse(text = .) %>% eval, 
                        (tag.column.name) := tagged.value
                 ] %>% setorderv(c("docid", "sent", "position"))
             )
          } 
        
    }
    
    if(match.type == "match.list"){
  
         if(where.list %>% is.null){  
             stop("Invalid argument(s). ") 
         } else { 
             
            c1 <- (where.token %>% is.null)
            c2 <- (tag.column.name             %>% is.null)
            c3 <- (tagged.value                %>% is.null)
            c4 <- (tagged.value.expression     %>% is.na)
            c5 <- (tagged.value.not            %>% is.null)
            c6 <- (tagged.value.expression.not %>% is.na)
             
            if(any(c1, c2, (c3 %xor% c4 ) %>% not, (c5 %xor% c6 ) %>% not)){
             
            stop("Invalid argument(s). Match variant \"match.regular.expresion\" requires: \r\n 
                   \t \"where.regex\" \r\n
                   \t \"where.token\" \r\n
                   \t \"tag.column.name\" \r\n
                   \t \"tagged.value\" or \"tagged.value.not.expression\"\r\n
                   \t \"tagged.value.not\" or \"tagged.value.not.expression\"\r\n") 
                
            } else {
                if(where.list %>% is.list){ 
                    
                } else { 
                     if(where.list %>% is.character){ 
                        if(length(where.list) == 1){
                            
                            where.list <- where.list %>% call %>% eval 
                            
                            if(where.list %>% is.data.table){
                                where.list <- where.list[match.type == "match.regular.expression", where.regex , by = tag.column.name][,where.regex]
                            }
                            
                        } 
                     }
                 } 
            } 
        }
        
        if(set.not){
             dtfrog[, (tag.column.name) := tagged.value.not] 
        }
 
        return(dtfrog)
        
    }
     
	
    
    if(match.type %in% c("match.regular.expression", "match.regex")){
        
        c1 <- (where.token %>% is.null)
        c2 <- (tag.column.name             %>% is.null)
        c3 <- (tagged.value                %>% is.null)
        c4 <- (tagged.value.expression     %>% is.na)
        c5 <- (tagged.value.not            %>% is.null)
        c6 <- (tagged.value.expression.not %>% is.na)
 
        
        if(any(c1, c2, (c3 %xor% c4 ) %>% not, 
                       (c5 %xor% c6 ) %>% not)){
             
            stop("Invalid argument(s). Match variant \"match.regular.expresion\" requires: \r\n 
                   \t \"where.regex\" \r\n
                   \t \"where.token\" \r\n
                   \t \"tag.column.name\" \r\n
                   \t \"tagged.value\" or \"tagged.value.not.expression\"\r\n
                   \t \"tagged.value.not\" or \"tagged.value.not.expression\"\r\n")
             
            
        } else
            
			dtfrog <<- dtfrog
			
            if(set.not){
                dtfrog[, c(tag.column.name):= tagged.value.not ];    
            }
            
            if(casefold){
                dtfrog[casefold(get(where.token)) %like%  casefold(where.regex),  c(tag.column.name) := tagged.value]  
            } else {
                dtfrog[get(where.token) %like%  where.regex,  c(tag.column.name) := tagged.value]  
            }
        
            setorderv(dtfrog, c("docid", "sent", "position"))  
            
        return(dtfrog)
        
    }
  
      
} 