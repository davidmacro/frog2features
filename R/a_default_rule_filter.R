#' F2F DH Rules: Filters
#'
#' Make a rule set to filter a frogged dataset. 
#' This function creates a transformation rule that \code{filters} a FROG-object according to some blue print.
#' Typically, filters are used near the beginning of a feature construction procedure, however a common practice is to use filters after the creation of ngrams on (parts of) the resulting ngram object. 
#'  
#' @param x                 Either a summary indicator; this can one of the following: 
#' \itemize{
#'      \item{\code{"all.names"}} to return vector of all names of all \code{filter} rules defined
#'      \item{\code{"all"}}  to return the full list of all \code{filter} rules and their parameter's default values.
#' } \cr 
#' Name of the feature construction rule to define. Examples are: 
#' \itemize{
#'     \item{\code{"exclude.infrequent.lemmas"} for lemma that have a low document frequency;}
#'     \item{\code{"exclude.punctuation"} for lemma are punctuation characters; }
#'     \item{\code{"exclude.dutch.stopwords"} for lemma that occur in the list of Dutch stopwords. }
#' }
#' @param ...               Implicit way to override parameters directly. Items defined here take precedence over items defined in the override argument. 
#' @param token             The column in the dataset to operate upon. The \code{token} parameter can refer to any column in frogger or modified dataset. 
#'                          Common values are: \code{"lemma"}, \code{"ngram"}, \code{"majorpos"}, and \code{"merge"} \cr \cr
#'                          
#'                          
#'                          Note: for \emph{ngrams}, filters can be applied to specific parts of the n-gram by using underscored notations. 
#'                          For example; by specifying \code{token = "majorpos_1"} a filter can be applied to the part-of-speech tag of the \emph{first} part of an \emph{ngram}. 
#'                          
#'                          
#' @return                  
#' This function returns one of the following: 
#' \itemize{
#'      \item{a list that defines a single filter rule}
#'      \item{a character vector containing all the names of all defined rules; for this, use: \code{x = "all.names"}}
#'      \item{the full list of all rule definitions; for this, use: \code{x = "all"}} 
#' }
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' 
#' 
#' 
#' }
#' 
#' 
default_rule_filter <- function(x, 
    where.token                    = "lemma", 
    allow.replay                   = 1, 
    ll                             = 0,
    ul                             = +Inf,
    remove.what                    = c("match","nomatch", "inside", "outside"), 
    count.type                     = c("frequency","tf-idf"),
    do                             = "exclude_on_token_prevalence", 
    where.token.value              = NULL,  
    where.token.value.match.type   = NULL,
    where.expression               = NULL,  
    ...){
    
    #Get the formals of the current function
    args.formals <- formals()    %>% as.list
    
    # Get the given arguments of the current functions
    args.given   <- match.call() %>% as.list 
	
    # Override the formals with the arguments given
    f.args <- args.formals %>% modifyList(args.given)
    
    if(is.null(f.args$x)){ 
         f.args %>% return
    } else{   
        rules <- list(   
            exclude.infrequent.lemmas = list(
                do = "filter_on_token_prevalence",   
                    count.type   = "frequency", 
                    exclude.what = "outside",
                    ll           = 5,
                    ul           = Inf, 
                    token        = "lemma", 
                    allow.replay = 0
            ),
            
            exclude.infrequent.lemma = list(
                do = "filter_on_token_prevalence",   
                    count.type   = "frequency", 
                    exclude.what = "outside",
                    ll           = 5,
                    ul           = Inf, 
                    token        = "lemma", 
                    allow.replay = 0  
            ),
            
            exclude.infrequent.ngrams = list(
                do = "filter_on_token_prevalence",   
                    count.type   = "frequency", 
                    exclude.what = "outside",
                    ll           = 10,
                    ul           = Inf,
                    token        = "ngram",
                    type         = "apply.filter",
                    allow.replay = 0 
            ),			
            
            exclude.infrequent.ngram = list(
                do = "filter_on_token_prevalence",
                    count.type   = "frequency", 
                    exclude.what = "outside",
                    ll           = 10,
                    ul           = Inf,
                    token        = "ngram", 
                    allow.replay = 0
            ),				
             
            exclude.dutch.stopwords = list(
                do = "filter_on_token_content", 
                    where.token                  = "lemma",
                    where.token.value            =  entities_dutch_stopwords(),   
                    where.token.value.match.type = "in",
                    remove.what                  = "match", 
                    allow.replay                 = 1 
            ),	
             
            exclude.non.odw.types = list(   
                do = "filter_on_token_content" ,
                    where.token         = "lemma",
                    where.token.value   = majorpos.semantic(),  
                    allow.replay        = 1 
            ),			 
            
            exclude.if.majorpos = list(  
                do = "filter_on_token_content",   
                    where.token         = "majorpos",
                    where.token.value   = c("ADJ", "BW","LET","LID","N","SPEC","TSW","TW","VG","VNW","VZ","WW"), 
                    allow.replay        = 1 
            ),		
             
            include.if.majorpos = list(
                do = "filter_on_token_content",
                    allow.replay   = 1, 
                    include.what   = "inside", 
                    token          = "majorpos",
                    items          = c("ADJ", "BW","LET","LID","N","SPEC","TSW","TW","VG","VNW","VZ","WW"), 
                    allow.replay   = 1  
            ),	
            
            exclude.punctuation = list(
                do = "filter_on_token_content", 
                    exclude.what   = "inside", 
                    token          = "majorpos",    
                    items          =  special.majorpos.punctuation(),
                    tag            =  "special.majorpos.punctuation()", 
                    allow.replay   = 1 
            ), 
            
            include.only.svo = list( 
                do = "filter_on_token_content", 
                    exclude.what   = "inside", 
                    token          = "majorpos",    
                    items          =  special.majorpos.svo(),
                    tag            = "majorpos.svo()", 
                    allow.replay   = 1
            ),
            
            exclude.rare.word = list( 
                do = "filter_on_token_prevalence",
                    count.type     = "proportion",
                    filter.type    = "exclude",
                    exclude.what   = "outside",
                    ll             = 0,
                    ul             = 0.5,
                    token          = "word" , 
                    allow.replay   = 0 
            ),
            
            exclude.rare.bigram = list( 
                do = "filter_on_token_prevalence", 
                    count.type     = "proportion",
                    filter.type    = "exclude",
                    exclude.what   = "outside",
                    ll             = 0,
                    ul             = 0.5,
                    token          = "bigram" , 
                    allow.replay   = 0 
            ),
            
            exclude.frequent.lemma = list( 
                do = "filter_on_token_prevalence", 
                    count.type   = "frequency",
                    filter.type  = "exclude",
                    exclude.what = "outside",
                    ll           = 2,
                    ul           = Inf,
                    token        = "lemma", 
                    allow.replay = 0 
            ),
            
            exclude.frequent.word = list( 
                do = "filter_on_token_prevalence",
                    count.type   = "proportion",
                    filter.type  = "exclude",
                    exclude.what = "outside",
                    ll           = 0,
                    ul           = 0.5,
                    token        = "lemma", 
                    allow.replay = 0 
            ),
            
            exclude.frequent.bigram = list( 
                do = "filter_on_token_prevalence",
                    count.type   = "frequency",
                    filter.type  = "exclude",
                    exclude.what = "outside",
                    ll           = 2,
                    ul           = Inf,
                    token        = "ngram", 
                    allow.replay = 0
            ),
            
            exclude.frequent.word = list(
                do           = "filter_on_token_prevalence", 
                    count.type   = "proportion",
                    filter.type  = "exclude",
                    exclude.what = "outside",
                    ll           = 0,
                    ul           = 0.5,
                    token        = "ngram" , 
                    allow.replay = 0 
            )      
        )   
      
        rules %<>% lapply(function(x){ 
            x$data.input.name <- "dtfrog"   
            x
        })
        
        
        if(x == "all.definitions"){  
            
            rules %<>% lapply(function(x){ 
                x <- formals(x$do) %>% modifyList(x)
                x$type <- "allow.filter"
                x$data.input.name <- "dtfrog"
                x %>% return
            }) 
                           
            return(rules) 	
        } else {
            if(x == "all.names"){
                
               return(rules %>% names) 
                
            } else {
                
                if(x == "all.data.table"){ 
                    default_rule_filter("all.definitions") %>lapply% 
                        as.list %>lapply% 
                        t %>lapply% 
                        data.table %>% 
                        rbindlist(fill=T) %>% return
                     
                } else { 
                    if(x %in% {rules %>% names}){ 
                        newrule <- rules[[x]] %>% modifyList(args.given)
                        newrule$type <- "apply.filter" 
                        return(newrule)  
                        
                    } else {
                        stop("Rule does not exist!") 
                    }
                }
            }
        }    
    }        
}
