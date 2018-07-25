#' default_rule_replace
#'
#' @param x Name of the rule to define
#' @param override explicit way to override parameters via a named list.
#' @param ... implicit way to override parameters directly. Items defined here take precedence over items defined in the override argument. 
#'
#' @return a list defining a single rule, a character vector of all the names of all defined rules, or the full list of all rule definitions.
#' @export
#'
#' @examples
default_rule_replace <- function(x,  
    where,
    match.type           = c("equals","like", "in"),
    token                = "lemma",
    new.value            = NULL,
    new.value.get        = NULL,
    new.value.expression = NULL,
    prefix               = "",
    suffix               = "",
    description          = "",
    do                   = "replace_in_frog",
    ...){

    # Get all formals and values of the present function, minus the dots and x
    args.formals <- formals(fun = "default_rule_replace") %>% as.list  
    args.formals <- args.formals[which(args.formals %>% names %in% c("...", "x") %>% not)]
     
    # Get all values actually provided to the function; override 
    args.given   <- match.call(expand.dots = T) %>% as.list  
    
    # Modify formals with args given.
    args.formals %<>% modifyList(args.given)
    
    if(is.null(args.formals$x)){   
        # No default rule specified; return the formals modified with the given args. 
         args.formals %>% return
    } else{
        
        # Specify all the default rules 
        
        rules <- list(   
    		substitute.majorpos.count = list( 
                where       = "majorpos %in% c('TW')",
                match.type  = "%in%",
                token       = "lemma",
                value.get   = "majorpos",
    			new.value   = "TW",
                prefix      = "[",
                suffix      = "]",
    			description = "Replace counts with [TW] tag." 
            ),
    		
    		substitute.majorpos.count = list( 
                where       = "majorpos %in% c('TW')",
                token       = "lemma",
                value.get   = "majorpos",
    			new.value   = "TW",
                prefix      = "[",
                suffix      = "]",
    			description = "Replace counts with [TW] tag." 
            ),
    		
            substitute.names.person = list( 
                where = "(pos %like% \"SPEC\\\\(deeleigen\\\\)\") & (ner %like% \"B\\\\-PER\") & (ner %like% \"B\\\\-ORG\" %>% not)",
                token = "lemma",
                new.value.expression = NA,
                new.value  = "persoon",
                prefix = "[",
                suffix = "]",
    			description = "Replaces known nomens with the [persoon] tag"
            ),
    		
            substitute.website = list( 
                where = "(word %like% \"\\\\.nl\" | word %like% \"\\\\.be\" | word %like% \"http\" | word %like% \"\\\\.com\" | word %like% \"www\\\\.\")",
                token = "lemma",
                new.value.expression = NA,
                new.value  = "website",
                prefix = "[",
                suffix = "]", 
    			description = "Replaces regex matches to websites with [website]"			
            ),
    		
            substitute.dutch.banks = list( 
                where                = entities.dutch.banks(), 
                where.token          = "lemma",
                replace.token        = "lemma", 
                new.value            = "bank",
                prefix               = "[",
                suffix               = "]",
    			description          = "Replaces regex matches to websites with [website]"
            )
        )
    
 
        rules %<>% lapply(function(x){
            args.formals %>% modifyList(x) 
        })
         
        if(x == "all.definitions"){   
            
            rules %<>% lapply(function(x.rule){ 
                x.rule <- formals(x.rule$do) %>% as.list %>% modifyList(x.rule)
                x.rule$type            <- "replace.rule"
                x.rule$data.input.name <- "dtfrog"
                x.rule %>% return
            }) 	
            
        } else {
            if(x == "all.names"){
               return(rules %>% names) 
            } else { 
                if(x == "all.data.table"){ 
                     
                   rules.list <- default_rule_replace("all.definitions") 
                   
                   return(rules.list %>lapply% 
                             as.list %>lapply% 
                                   t %>lapply% 
                            data.table %>% 
                        rbindlist(fill=T, use.names = T) 
                    )
                } else {
                    if(x %in% {rules %>% names}){ 
                        # Extract the function name from rule$do
                        fname <- rules[[x]]$do
             
                        # Derive basic requirements from formals
                        newrule <- formals(fun = fname) %>% as.list
                        
                        # Override with items specified in rule
                        newrule %<>% modifyList(rules[[x]]) 
                        
                        # Override with some globals that always need to apply
                        newrule$allow.replay    <- T
                        newrule$type            <- "replace.rule"
                        newrule$data.input.name <- "dtfrog"
                     
                        # Override with user specific settings (user always has the last word)
                        newrule %<>% modifyList(args.given) 
                        
                        return(newrule)  
                    
                    } else {    
                        stop("Rule does not exist!") 
                    } 
                } 
            } 
        }  
    }

}
