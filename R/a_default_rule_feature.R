#' Transformation rules: Feature Construction
#'
#' This function creates a transformation rule that constructs the intended features. 
#' Typically, a feature construction rule is the last call in a feature construction pipeline. 
#'  
#' @param x                 Either a summary indicator; this can one of the following: 
#' \itemize{
#'      \item{\code{"all.names"}} to return vector of all names of all feature construction rules defined
#'      \item{\code{"all"}}  to return the full list of all featureconstruction rules and the default parameters. 
#' } \cr 
#' Name of the feature construction rule to define. This can be one of the following: 
#' \itemize{
#'     \item{\code{"tf"} or \code{"term.frequency"} for term frequencies (within each document)}
#'     \item{\code{"to"} or \code{"term.occurence"} for binary coded term occurences (within each document)}
#'     \item{\code{"tfidf"} for term-frequencies scaled by the (logged) inverse document frequencies.}
#'     \item{\code{"custom"} for non-standard feature construction methods.}
#' }
# 
#' @param override          Explicit way to override parameters via a named list; mainly kept here for compatibility. Use ... instead. 
#' @param ...               Implicit way to override parameters directly. Items defined here take precedence over items defined in the override argument. 
#' @param token             The column in the dataset to operate upon. Can be any column in the frogged/modified dataset. 
#'                          Common values are: \code{"lemma"}, \code{"ngram"}, \code{"majorpos"}, or \code{"merge"}
#' @param dictionary        Optional: a data.table with a column that has the same name as token. If specified, only items in the dictionary are used in the feature construction procedure.     
#' @param return.measure    Define the metric to return; valid values are:
#' \itemize{
#'     \item{\code{"term.frequency"} or \code{"tf"} - the number of times a term occurs in a document} 
#'     \item{\code{"term.occurence"} or \code{"to"} - whether (1) or not (0) a term occurs in a document}
#'     \item{\code{"tf-idf"} - term frequency corrected for the number of documents they occur in} 
#' } 
#' @param do                Name of feature construction function to call. 
#' @return                  This function returns one of the following: 
#' \itemize{
#'      \item{a list that defines a single featureconstruction rule}
#'      \item{a character vector containing all the names of all defined rules; for this, use: \code{x = "all.names"}}
#'      \item{the full list of all rule definitions; for this, use: \code{x = "all"}}
#'
#'
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' # Example 1:  Extract term frequencies of the 10 most common bigram combinations 
#'   
#' data(wb.sr.frogged)  # Load data
#'    
#' result <- wb.sr.frogged %>% 
#'   apply_rule(default_rule_ngram("lemma.bigram.adjacency")
#'    
#' 
#' 
#' } 
default_rule_make_features <- function(x = NULL,
    return.measure   = "term.frequency",
    token            = "lemma",
    token.dictionary = token,
    type             = "construct.features",
    do               = "calculate_count",
    data.input.name = "dtfrog",
    dictionary       = NA){

    # Get all formals and values of the present function, minus the dots and x
    args.formals <- formals(fun = "default_rule_make_features") %>% as.list  
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
	
            tf = list( 
                name             = "tf",
                return.measure   = "term.frequency",
                token            = "lemma",   
                dictionary       = NULL 
            ),
            
            term.frequency = list( 
                name             = "term.frequency",
                return.measure   = "term.frequency",
                token            = "lemma",    
                dictionary       = NULL 
            ),
            
            to = list( 
                name             = "to",
                token            = "lemma",      
                dictionary       = NULL 
            ),
            
            term.occurence = list( 
                name             = "term.occurence", 
                token            = "lemma",                        
                dictionary       = NULL 
            ),
             
            tfidf = list(
                name             = "tfidf", 
                return.measure   = "tfidf",  
                token            = "lemma",     
                dictionary       = NULL 	
            ) 
        ) 
        
        rules %<>% lapply(function(x){
            args.formals %>% modifyList(x) 
        })
         
        if(x == "all.definitions"){   
            
            rules %<>% lapply(function(x.rule){ 
                x.rule <- formals(x.rule$do) %>% as.list %>% modifyList(x.rule)
                x.rule$type            <- "construct.features"
                x.rule$data.input.name <- "dtfrog"
                x.rule %>% return
            }) 	
            
        } else {
            if(x == "all.names"){
               return(rules %>% names) 
            } else { 
                if(x == "all.data.table"){ 
                     
                   rules.list <- default_rule_make_features("all.definitions") 
                   
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
             
                        # Get the formals of the specified function in rule$do 
                        # and override with given args. 
                         
                        newrule <- formals(fun = fname) %>% 
                            as.list %>% 
                            modifyList(rules[[x]]) %>% 
                            modifyList(args.given) 
                        
                        newrule$allow.replay <- T
                        newrule$type <- "construct.features"
                              
                        return(newrule)  
                    
                    } else {    
                        stop("Rule does not exist!") 
                    } 
                } 
            } 
        }  
    }

}
