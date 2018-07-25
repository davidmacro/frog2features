#' Transformation rules: Merge
#'
#' Create a transformation rule that merges the original FROG dataset on some columns with an external database. 
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
#' \notrun {
#' 
#'    # Example 1:  Extract term frequencies of the 10 most common bigram combinations 
#'   
#'    data(wb.sr.frogged)  # Load data
#'    
#'    result <- wb.sr.frogged %>% 
#'                   apply_rule(default_rule_ngram("lemma.bigram.adjacency")
#'    
#' 
#' 
#' }  
default_rule_merge <- function(x, override = NULL, ...){

	retval.rule <- getNaRuleList()

	opts.dotted  <- list(...)    
    override     <- list() %if% is.null(override)
      
    if(opts.dotted %>% length %>% is_greater_than(0)){ 
        override %<>% modifyList(opts.dotted, keep.null = T)
    }   
	
	rules <- list(   
	
		merge.lemma.synonym = list( 
		    type              = "merge", 
		    token.by.x        =  c("lemma", "majorpos"),
			token.by.y        =  c("lemma", "majorpos"),
		    token.replacement = "lemma.replacement.2",
			merge.type        = "synset",
			merge.database    = "lemma.synonyms.dutch",
            allow.replay      = 1
		),
		
		merge.lemma.hyperonym = list( 
			token.by.x        = "lemma",
			token.by.y        = "lemma",
			merge.type        = "hyperonymset",
			merge.database    = "default.hyperonym.database",
			degree            = 1,
			allow.replay      = 1
		),
		
		merge.lemma.property = list( 
			token.by.x        = "lemma",
			token.by.y        = "lemma",
			property          = "",
			merge.type        = "synset",
			merge.database    = "default.property.database",
            degree            = 1,
			allow.replay      = 1 
		) 
		
	)
	
	if(x == "all.definitions"){ 
        return(rules) 	
    } else {
	    if(x == "all.names"){
           return(rules %>% names) 
        } else {
	        if(x %in% {rules %>% names}){
                
				newrule <- rules[[x]]
					
				if(length(override)>0){
					newrule %<>% modifyList(override, keep.null=T)       
				}
		
				if(newrule$allow.replay %>% is.null){
					newrule$allow.replay <- 1
				} 
							
				newrule$type <- "merge"
					 
				stop("No token specified") %if% (newrule$tokens %>% is.null)
				stop("Degree not specified") %if% (newrule$degree %>% is.null)
				stop("Degree should be a positive, non-zero integer.") %if% (newrule$degree %>% is.integer)
				
                return(newrule)  
				
            } else {
                stop("Rule does not exist!") 
            }
        } 
    }    
	return(newrule) 
	
} 