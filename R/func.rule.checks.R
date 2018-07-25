is.f2f.rule <- function(rule, verbose =T){
     
    
    # (1) All f2f-rules are lists
    if(rule %>% is.list %>% not){
        cat("Error: specified parameter was not a list.") %if% verbose
        return(FALSE)
    }
    
    # (2) All f2f-rules should have a "do" item 
    if(rule$do %>% is.null){
        cat("Error: no 'do' item specified in the rule.") %if% verbose
        return(FALSE)
    }
    
    # (3) All f2f-rules should have a "do" item that matches a known function
    if(rule$do %>% exists %>% not){
        cat("Error: the 'do' item specified (i.e., '" %+% rule$do %+% "') does not match any known function.") %if% verbose
        return(FALSE)
    }
     
  #  # (4) All specified arguments should match the required arguments of the 
  #  if({names(formals(rule$do)) %-% "..."} %in% {rule %>% names} %>% all %>% not){
  #      cat("Error: not specified whether the rule should be allowed in a replay scenario.") %if% verbose
  #      return(FALSE)
  #  }  
    
    cat("\r\n The rule appears to be valid \r\n") %if% verbose
    
    return(TRUE)

}


assert.f2f.rule <- function(rule){
    stopifnot(is.rule(rule))
    return(rule)
}
