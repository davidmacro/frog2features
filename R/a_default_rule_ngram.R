#' F2F DH Rules: Creating Ngrams
#'
#' Make a rule set to create ngrams. 
#' Typically, \emph{ngrams} are created by concatenating adjacent lemmas, but various alternatives are possible, such as concatenating lemmas based on syntactic dependencies, and concatenating other tokens.
#' @param x                 Either a summary indicator; this can one of the following: 
#' \itemize{
#'      \item{\code{"all.names"}} to return vector of all names of all \code{ngram} rules defined
#'      \item{\code{"all"}}  to return the full list of all \code{ngram}-creation rules and their parameter's default values.
#' } \cr 
#' Or a name of the ngram construction rule to define. Examples are: 
#' \itemize{
#'     \item{\code{"lemma.bigram.adjacency"} for lemma bigrams;}
#'     \item{\code{"lemma.trigram.adjacency"} for lemma bigrams, etc.}
#'     \item{\code{...}}
#' } 
#' 
#' @param ngram.tokens 
#' Character vector; names of the tokens to combine. Should be valid column names in \code{dtfrog}. \cr\cr 
#' Often \code{ngram.tokens} will be \code{c("lemma", "lemma", ...)} to create lemma ngrams. 
#' Other useful specifications are \code{c("majorpos", "majorpos",...)}, which create part-of-speech n-grams (\emph{i.e.}, 'woordsoorten'). 
#' 
#' @param ngram.length 
#' Integer; the number of tokens in the ngram. 
#'  
#' @param prefix,suffix,collapse.separator
#' (Optional) character to prefix, suffix and separate the (parts of) the ngram. \cr\cr
#' If \code{create_ngram_fast} is called directly, \code{prefix} and \code{suffix} 
#' default to empty strings and \code{collapse.separator} defaults to " - ".  \cr\cr
#' The samples in \code{\link{default_rule_ngram}} apply different settings 
#' to help provide distinguishable output.
#' 
#' @param make.copy 
#' Boolean flag to indicate whether to \code{data.table::copy()} the provided \code{dtfrog} object, or to apply the changes in place. 
#' 
#' 
#' @param proximity.criterium The criterium to determine which two or more tokens need to be joined.  
#'                            The default value is \code{"adjacency"}, which concatenates consecutive tokens; 
#'                            another possibility is to specify \code{"syntactic"}, which parses the syntactic 
#'                            dependency graphs. 
#' @return                    This function returns one of the following: 
#' \itemize{
#'      \item{a list that defines a single \emph{ngram}-creation rule;}
#'      \item{a character vector containing all the names of all defined \emph{ngram}-creation rule; for this, use: \code{x = "all.names"}}
#'      \item{the full list of all \emph{ngram}-creation rule definitions; for this, use: \code{x = "all"}} 
#' }
#' 
#' @usage 
#' 
#' # Typical usage: use an existing rule and (optionally) override some properties
#' default_rule_ngram("lemma.bigram.adjacency", ...) 
#' 
#' # Non-standard usage: build the whole rule out of the arguments.   
#' default_rule_ngram(x,
#'     ngram.tokens              = NULL,
#'     ngram.length              = NULL, 
#'     ngram.proximity.criterium = c("adjacency", "syntactic"),
#'     ngram.new.colname         = "ngram", 
#'     ngram.overwrite           = FALSE,
#'     collapse.separator        = " - ",
#'     prefix                    = "", 
#'     suffix                    = "", 
#'     verbose                   = FALSE,
#'     allow.replay              = 1, 
#'     do                        = "create_ngram_fast",
#'     data.input.name           = "dtfrog",
#'     ...
#' ) 
#' 
#' @export
#'
#' @examples 
#' 
#' 
#' \dontrun{
#' 
#' # Load the example data
#' data(wb.sr.frogged)
#' 
#' # Make lemma bigrams based on positional proximity
#' result1 <- wb.sr.frogged %>% 
#'     apply_rule(default_rule_ngram("lemma.bigram.adjacency", 
#'          ngram.new.colname = "ngram_1")) 
#' 
#' # Make lemma bigrams based on syntactic proximity
#' result2 <- wb.sr.frogged %>% 
#'     apply_rule(default_rule_ngram("lemma.bigram.syntactic", 
#'         ngram.new.colname = "ngram_2")) 
#'
#' # Compare the results
#' merged_result <- merge(
#'     x = result1[, .(docid,sent,position, ngram_1, mpos_a1 = majorpos_1, mpos_a2 = majorpos_2)],
#'     y = result2[, .(docid,sent,position, ngram_2, mpos_b1 = majorpos_1, mpos_b2 = majorpos_2)],
#'     by = c("docid", "sent", "position"),
#'     all.x = FALSE,
#'     all.y = FALSE
#' ) 
#' }
#' 
default_rule_ngram <- function(x = NULL, 
    ngram.tokens                = NULL,
    ngram.length                = NULL,
    ngram.proximity.criterium   = c("adjacency", "syntactic"),
    ngram.new.colname           = "ngram",
    ngram.overwrite             = FALSE,  
    collapse.separator          = " - ",
    prefix                      = "",
    suffix                      = "",
    verbose                     = FALSE ,
    allow.replay                = 1, 
    do                          = "create_ngram_fast" ,
    data.input.name             = "dtfrog",
    ...){

    # Get all formals and values of the present function, minus the dots and x
    args.formals <- formals(fun = "default_rule_ngram") %>% as.list  
    args.formals <- args.formals[which(args.formals %>% names %in% c("...", "x") %>% not)]
      
    # Get all values actually provided to the function; override  
    args.given  <- list()
      
    if(!(args.given %>% class %in% "language")){
        args.given <- match.call(expand.dots = T) %>% as.list  
    }
      
    # Modify formals with args given.
    args.formals %<>% modifyList(args.given)
    
    if(is.null(args.formals$x)){   
        # No default rule specified; return the formals modified with the given args. 
         args.formals %>% return
    } else{
        
        # Specify all the default rules 
        
        rules <- list(        
            word.bigram.adjacency = list( 
                ngram.tokens  = c("word","word"),	
                ngram.length  = 2,   
                ngram.proximity.criterium  = "adjacency",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),
            word.trigram.adjacency = list( 
                ngram.tokens  = c("word","word","word"), 
                ngram.length  = 3, 
                ngram.proximity.criterium  = "adjacency",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),                                                                                                          
            lemma.bigram.adjacency = list( 
                ngram.tokens  = c("lemma","lemma"),
                ngram.length  = 2,  
                ngram.proximity.criterium  = "adjacency",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),
            lemma.trigram.adjacency	= list( 
                ngram.tokens  = c("lemma","lemma","lemma"), 
                ngram.length  = 3,  
                ngram.proximity.criterium  = "adjacency",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),                                                                                                         
            majorpos.bigram.adjacency = list( 
                ngram.tokens  = c("majorpos","majorpos"),
                ngram.length  = 2,  
                ngram.proximity.criterium  = "adjacency",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),
            majorpos.trigram.adjacency	= list( 
                ngram.tokens  = c("majorpos","majorpos","majorpos"),
                ngram.length  = 3, 
                ngram.proximity.criterium  = "adjacency",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),                                                                                                         
            word.bigram.syntactic = list( 
                ngram.tokens  = c("word","word"),
                ngram.length  = 2,  
                ngram.proximity.criterium  = "syntactic",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ), 
            word.trigram.syntactic = list( 
                ngram.tokens = c("word","word","word"),
                ngram.length = 3,   
                ngram.proximity.criterium  = "syntactic",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),  
            lemma.bigram.syntactic = list( 
                ngram.tokens  = c("lemma","lemma"), 
                ngram.length  = 2,   
                ngram.proximity.criterium  = "syntactic",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ), 
            lemma.trigram.syntactic	= list( 
                ngram.tokens  = c("lemma","lemma","lemma"),
                ngram.length  = 3,  
                ngram.proximity.criterium  = "syntactic",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ),  
            majorpos.bigram.syntactic	= list(	 
                ngram.tokens  = c("majorpos","majorpos"), 
                ngram.length  = 2,   
                ngram.proximity.criterium  = "syntactic",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - ["
            ), 
            majorpos.trigram.syntactic	= list( 
                ngram.tokens = c("majorpos","majorpos","majorpos"), 
                ngram.length = 2,  
                ngram.proximity.criterium = "syntactic",
                prefix        = "[",
                suffix        = "]",
                collapse.separator = "] - [" 
            )
        ) 
        
        rules %<>% lapply(function(x){
            args.formals %>% modifyList(x) 
        })
         
        if(x == "all.definitions"){   
            
            rules %<>% lapply(function(x.rule){ 
                x.rule <- formals(x.rule$do) %>% as.list %>% modifyList(x.rule)
                x.rule$type            <- "create.ngram"
                x.rule$data.input.name <- "dtfrog"
                x.rule %>% return
            }) 	
            
        } else {
            if(x == "all.names"){
               return(rules %>% names) 
            } else { 
                if(x == "all.data.table"){ 
                     
                   rules.list <- default_rule_ngram("all.definitions") 
                   
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
                        newrule$type <- "create.ngram"
                             
                        stop("No token specified")   %if% (newrule$ngram.tokens %>% is.null)
                        stop("Length not specified") %if% (newrule$ngram.length %>% is.null)
                        stop("Length should be a positive, non-zero integer.") %if% (newrule$ngram.length %>% is.integer)
                        
                        return(newrule)  
                    
                    } else {    
                        stop("Rule does not exist!") 
                    } 
                } 
            } 
        }  
    }

}
