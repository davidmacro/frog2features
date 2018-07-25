#' Frog DH: create ngrams from frogged dataset. 
#' 
#' This function can be used to create ngrams within a frogged dataset. 
#' Note that this function was intended to be an (invisible) back-end to the 
#' \code{default_rule_ngram} function. Since this function can also be 
#' used on it's own, it seemed natural to expose it publically.  
#'  
#' @param dtfrog 
#' Frogged dataset. Note: \code{dtfrog} must be a \code{data.table} and must pass 
#' the \code{is.frogged(dtfrog)} check.
#'
#' @param ngram.tokens 
#' Character vector; names of the tokens to combine. Should be valid column names in \code{dtfrog}. \cr\cr 
#' Often \code{ngram.tokens} will be \code{c("lemma", "lemma", ...)} to create lemma ngrams. 
#' Other useful specifications are \code{c("majorpos", "majorpos",...)}, which create part-of-speech n-grams (\emph{i.e.}, 'woordsoorten'). 
#' 
#' @param ngram.length 
#' Integer; the number of tokens in the ngram. 
#' 
#' @param ngram.proximity.criterium
#' Character; if \code{"adjacency"}, ngrams are created based on positional proximity within the sentence. 
#' if \code{"syntactic"}, ngrams are creaded based on syntactic dependencies.
#'    
#' @param ngram.new.colname   
#' Character; name of the new column in \code{dtfrog} to assign the ngram to. Defaults to "ngram". 
#' Note: if changed to a custom name, make sure to use that new custom name in follow-up steps. 
#'    
#' @param collapse.separator
#' Character(s) to use to separate the tokens that comprise the ngram. 
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
#' @param verbose 
#' Boolean indicating whether to print verbose information.
#' 
#' @param return.type 
#'  
#' @usage 
#' 
#' create_ngram_fast(dtfrog, 
#'     ngram.tokens, 
#'     ngram.length              = NULL,
#'     ngram.proximity.criterium = c("adjacency", "syntactic"),
#'     ngram.new.colname         = "ngram", 
#'     ngram.overwrite           = FALSE,
#'     prefix                    = "", 
#'     suffix                    = "", 
#'     collapse.separator        = " - ",              
#'     verbose                   = F,
#'     make.copy                 = TRUE
#' ) 
#'  
#'  
#' @return
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' # ------------------------------------------------------
#' # Example 1: create common (positional) lemma bigrams.
#' # ------------------------------------------------------
#'
#' data(wb.sr.frogged)
#' 
#' result <- wb.sr.frogged
#' result <- wb.sr.frogged 
#' 
#' }
#'  
#' @export
create_ngram_fast <- function(dtfrog,  
    ngram.tokens,
    ngram.length              = NULL, 
    ngram.proximity.criterium = c("adjacency", "syntactic"),
    ngram.new.colname         = "ngram",
    ngram.overwrite           = FALSE, 
    collapse.separator        = " - ",
    prefix                    = "",
    suffix                    = "",
    verbose                   = F ,
    make.copy                 = TRUE){
 
    # Check mandatory arguments
	stopifnot(is.frog(dtfrog))
	
    # Check optional arguments 
    args.given  <- match.call() %>% as.list %>% names %-% ""
 
    if(ngram.overwrite == FALSE){
        # Stop if ngram.new.colname already exists in dtfrog
        stopifnot(ngram.new.colname %in% colnames(dtfrog) %>% not)    
    }
     
    ngram.proximity.criterium <- match.arg(ngram.proximity.criterium)
    
	dtfrog[, ngram_valid := 0]
	  
	# Per default, take the most useful FROG columns and lag these
	cols.to.lag  <- c("word", "lemma", "pos", "majorpos", "ner", "position") %>% 
						union(ngram.tokens) %>% 
						unique
 	 
	if(ngram.proximity.criterium == "adjacency"){ 
	
	    setorderv(dtfrog, cols =  c("docid","sent", "position"))
	    
        for(i in 1:ngram.length){ 
             
	        cols.lagged <- cols.to.lag %+% "_" %+% i
	    
	        dtfrog[, c(cols.lagged) := shift(.SD, i-1, type="lead"),  by = c("docid", "sent"), .SDcols = cols.to.lag]
	            
	        dtfrog[cols.lagged[[i]] %>% get %>% is.na %>% not, ngram_valid := i]
	        
	        if(i > 1){
	            dtfrog <- dtfrog[get("position_" %+% (i-1)) != (get("position_" %+% (i)) + 1),]     
	        } 
	    }  
	}
	 
	if(ngram.proximity.criterium == "syntactic"){
	    
	    # Note: currently only syntactic bygrams
	     
	    cols.pos <- which(colnames(dtfrog) %in% c((colnames(dtfrog) %-% c("docid", "sent","position")))) 
        cols.val <- colnames(dtfrog)[cols.pos]
	  
        for(i in 1:1){  
	    
	         dtfrog_l <- dtfrog 
             dtfrog_r <- dtfrog %>% copy
	         
             col.pos.l <- which(colnames(dtfrog_l) %in% c((colnames(dtfrog_l) %-% c("docid", "sent","position")))) 
             col.pos.r <- which(colnames(dtfrog_l) %in% c((colnames(dtfrog_r) %-% c("docid", "sent","position")))) 
              
	         colnames(dtfrog_l)[col.pos.l] %<>% paste0("_" %+% i)
             colnames(dtfrog_r)[col.pos.r] %<>% paste0("_" %+% (i+1))
           
             dtfrog_l[, position := get("parse1_" %+% (i))]  
              
	         dtfrog  <-  merge(x        = dtfrog_l, 
                               y        = dtfrog_r,  
                               by.x     = c("docid","sent","position"),
                               by.y     = c("docid","sent","position"), all = T)
	          
	         dtfrog[(("word" %+% "_" %+% (i))   %>% get %>% is.na %>% not &
                     ("word" %+% "_" %+% (i+1)) %>% get %>% is.na %>% not), 
	                    ngram_valid := (i+1)]
	     }  
	} 
	
	cols.to.glue  <- ngram.tokens %+% "_" %+% 1:ngram.length
	    
    # Filter all incomplete ngrams:   
    dtfrog <- dtfrog[ngram_valid == ngram.length,]
  
    ngram.new.colname %>% print
    
    dtfrog[, (ngram.new.colname) := prefix %+% do.call(paste, c(dtfrog[,..cols.to.glue], sep = collapse.separator)) %+% suffix ]
     
	  
    return(dtfrog)
	 
}