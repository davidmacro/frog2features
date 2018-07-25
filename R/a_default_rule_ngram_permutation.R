default_rule_ngrams_permute <- function(x, 
    tokens, 
    summary                 = FALSE,
    majorpos_sort_order,
    combinations_to_merge   = "all",
    combinations_to_exclude = "none",
    ties                    = "alphabetical",
    colname_ngram           = "ngram",
    colname_ngram_permuted  = "ngram_permuted",
    non.matched             = "keep",
    data.input.name         = "dtfrog",
    do                      = "permute_ngrams_on_majorpos",
    ...){
    

    # Get all formals and values of the present function, minus the dots and x
    args.formals <- formals(fun = "default_rule_ngrams_permute") %>% as.list  
    args.formals <- args.formals[which(args.formals %>% names %in% c("...", "x") %>% not)]
     
    # Get all values actually provided to the function; override 
    args.given   <- match.call(expand.dots = T) %>% as.list  
    
    # Modify formals with args given.
    args.formals %<>% modifyList(args.given)
    
    if(is.null(args.formals$x)){   
        # No default rule specified; return the formals modified with the given args. 
         args.formals %>% return
    } else{
        
        rules <- list(  
            word.bigram.verb.noun = list(
                tokens              = c("word_1", "word_2"),
                majorpos_sort_order = c("WW", "N")  
            ),
            
            word.bigram.verb.adverb = list(
                tokens              = c("word_1", "word_2"),
                majorpos_sort_order = c("WW", "BW"),
                non.matched         = "keep" 
            ),
            
            lemma.bigram.verb.noun = list(
                tokens              = c("lemma_1", "lemma_2"),
                majorpos_sort_order = c("WW", "N") 
            ),
            
            lemma.bigram.verb.adverb = list(
                tokens              = c("lemma_1", "lemma_2"),
                majorpos_sort_order = c("WW", "BW") 
            ) 
        )    
                
        rules %<>% lapply(function(x){
            args.formals %>% modifyList(x) 
        })
         
        if(x == "all.definitions"){    
            
            rules %<>% lapply(function(x.rule){ 
                x.rule <- formals(x.rule$do) %>% as.list %>% modifyList(x.rule)
                x.rule$type            <- "create.ngram.permutation"
                x.rule$data.input.name <- "dtfrog"
                x.rule %>% return
            }) 
            
            return(rules) 
            
        } else {
            if(x == "all.names"){    
               return(rules %>% names) 
            } else {
                if(x == "all.data.table"){ 
                     
                   rules.list <- default_rule_ngrams_permute("all.definitions") 
                   
                   return(rules.list %>lapply% 
                             as.list %>lapply% 
                                   t %>lapply% 
                            data.table %>% 
                        rbindlist(fill=T, use.names = T) 
                    )
                }  else {
                    if(x %in% {rules %>% names}){ 
                        # Extract the function name from rule$do
                        fname <- rules[[x]]$do
             
                        # Get the formals of the specified function in rule$do 
                        # and override with given args. 
                         
                        newrule <- formals(fun = fname) %>% 
                            as.list %>% 
                            modifyList(rules[[x]]) %>% 
                            modifyList(args.given) 
                        
                        newrule$allow.replay <- T
                        newrule$type <- "create.ngram.permutation" 
                        
                        return(newrule)  
                    
                    } else {    
                        stop("Rule does not exist!") 
                    } 
                } 
            }
        }    
    }        
}
    
    