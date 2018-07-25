# --------------------------------------------------------------------------------#
#      File: func.apply_rule.R                                                    #
# --------------------------------------------------------------------------------#
#    author: D.A. Macro (d.a.macro@minvenj.nl)                                    #
#      date: 2018-07-05                                                           #
#                                                                                 #
# --------------------------------------------------------------------------------#
#   Purpose: Apply a simple rule to a FROG data.table object and return the       #
#            resulting dt.                                                        #
#                                                                                 #
# --------------------------------------------------------------------------------#

#' Apply a data transformation rule to a FROG dataset.
#' 
#' The \code{apply_rule()} function can be used to directly apply a data transformation rule to a
#' frogged dataset. 
#' 
#' Note that the \code{apply_rule()} function is a direct, light weight method
#' to access functionality that the Object Oriented (OO) approach masks from the end-user. 
#' The \code{apply_rule()} function is intended for testing purposes, in the 
#' beginning of a text-mining procedure. For production environments, it is highly
#' recommended to follow the more rigid OO-feature construction procedures as outlined 
#' in the manual, for this approach involves a lot of additional checks and creates
#' a fully documented data handling-object that can easily be used on new data. 
#' 
#' @param dtfrog Frogged dataset (use \code{is.frog()} to validate)
#' @param rule Valid transformation rule
#'
#' @return Returns data.table with rule applied
#' @export
#' @author D.A. Macro (David@DataIM.nl)
#' @examples
#' \dontrun{ 
#' 
#' # Example:  
#' require(magrittr) 
#' data(wb.sr.frogged) 
#'
#' # Define some rules
#' r1 <- default_rule_filter("exclude.infrequent.lemmas") 
#' r2 <- default_rule_ngram("lemma.bigram.adjacency")
#' 
#' # Apply rules to the example dataset:
#' wb.sr.frogged  %>% 
#'   apply_rule(r1) %>%  
#'   apply_rule(r2)  
#' }
apply_rule <- function(data.input, rule, copy = T){  
    
    if(copy){
        data.input.tmp <- data.input %>% copy    
    } else {
        data.input.tmp <- data.input
    } 
    
    return(getCallFromRule(
		rule        = rule,
		data.input  = data.input.tmp,  
		verbose     = T
	))
}






