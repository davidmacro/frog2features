 
default_rule_create_dictionary <- function(x = NULL,  
    type     = "create.dictionary",  
    token    = "lemma", 
    prefix   = "l_", 
    save     = FALSE, 
    filename = NA,
    replay   = FALSE, 
    include.metrics = TRUE,
    do       = "create_dictionary", ...){
        
    args.formals <- formals()    %>% as.list
    args.given   <- match.call() %>% as.list

    f.args <- args.formals %>% modifyList(args.given)
       
    if(is.null(f.args$x)){ 
         f.args %>% return
    } else{ 
         
        rules <- list(
        
            lemma = list(
                name   = "Lemma dictionary",
                token  = "lemma",
                prefix = "l_"
            ), 
        
            ngram = list(
                name   = "Ngram dictionary",
                token  = "ngram",
                prefix = "ng_"
            ), 
        
            majorpos = list(
                name   = "Majorpos dictionary",
                token  = "majorpos",
                prefix = "mp_"
            ) 
        )
        
        rules %<>% lapply(function(x){
           args.formals %>% modifyList(x) 
        })
         
        if(x == "all.definitions"){ 
            return(rules)
        } else {
            if(x == "all.names"){
               return(rules %>% names) 
            } else {
                if(x == "all.data.table"){
                    return(
                        rules %>% lapply(function(x) {
                            as.data.table(x)
                        }) %>% rbindlist
                    )
                } else { 
                    if(x %in% {rules %>% names}){
                        rules[[x]] %>% return 
                    } else {
                        stop("Rule does not exist!") 
                    }
                } 
            } 
        }     
    }
}