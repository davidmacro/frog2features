
unity <- function(data.input){
    return(data.input)
}

getCallFromRule <- function(rule, data.input = NULL, verbose = T, use.strict = T){
      
    stopifnot(rule %>% is.list)  
      
    data.input.tmp <- data.input     
    rule           <- rule
    
    # If the specified item is not a rule, assume rule is a list of rules:
    if(!is.f2f.rule(rule,verbose=T)){
    
        cat("The specified rule is not a singular rule.")
    
        for(i in 1:length(rule)){   
            data.input.tmp  <- getCallFromRule(
                rule           = rule[[i]], 
                data.input     = data.input.tmp %>% copy, 
                verbose        = verbose,
                use.strict     = use.strict
            ) 
        }
        
        data.input.tmp %>% return
        
    } else {  
       
        # Extract the function name from rule$do
        fname <- rule$do
         
        # Get the formals of the specified function in rule$do 
        args.formals.names <- formals(fun = fname) %>% as.list  %>% names
          
        # Set the data.object  
        if(rule$data.input.name %>% is.null %>% not){
            
           rule[[rule$data.input.name]] <- data.input.tmp
           rule$data.input.name <- NULL
           
        } else {
          rule$data.input <- data.input.tmp 
        } 
         
        # Only retain those arguments that are formals as specified 
        # in args.formals.names; this strips rule of all the unnessecary 
        # args. 
        rule <- rule[which({rule %>% names} %in% args.formals.names)]
        
        return(
            eval(
               c(list(fname %>% get), rule) %>% as.call
            )            
        )  
    }   
} 