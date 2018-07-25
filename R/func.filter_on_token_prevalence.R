#' Frog DH: filter based on token prevalence
#'
#' Filter a frogged dataset (\emph{i.e.,} \code{dtfrog}) based on the token's prevalence. \cr \cr
#' \emph{Note:} Initially, this function was intended as an (invisible) back-end to the 
#' \code{default_rule_filter} function. But since this function can also be 
#' used on it's own, it seemed natural to expose the function publically. 
#'  
#' @param dtfrog 
#' Frogged dataset. Note: \code{dtfrog} must be a \code{data.table} and must pass the \code{is.frogged(dtfrog)} check.
#'
#' @param where.token 
#' Name of the column to select values of. Must resolve to an existing column in \code{dtfrog}.
#'
#' @param count.type 
#' The metric to determine token prevalence. Possible values: \code{"frequency"}, \code{"proportion"}, or \code{"tf-idf"}.
#'
#' @param remove.what 
#' \code{"match"} removes all rows matched; \code{"nomatch"} removes all non-matched rows. 
#' 
#' @param ll 
#' Non-negative numeric \emph{lower bound} of the selection interval. Defaults to \code{0}.
#'
#' @param ul 
#' Non-negative numeric \emph{upper bound} of the selection interval. Defaults to \code{+Inf}.
#'
#' @param group.by 
#' Extra column(s) to group by. If specified, all calculations will be done within groups formed by \code{group.by}.
#'
#' @param make.copy 
#' Boolean flag to indicate whether to \code{data.table::copy()} the provided \code{dtfrog} object, 
#' or to apply the changes in place.  
#' \emph{Note:} this parameter will not do anything until \code{data.table} implements by-reference row deletion. 
#'
#' @param dry.run 
#' Boolean flag to indicate whether to apply the filter, or to just print out descriptive information. 
#' If \code{dry.run = TRUE}, the original \code{dtfrog} object is returned unchanged. 
#'
#' @return 
#' A filtered dtfrog object.
#' 
#' @details
#' This function allows for three types of prevalence calculations, namely: 
#'
#' \describe{
#'   \item{Absolute document frequency:}{
#'       Filters based on the absolute number of documents in which a term occurs. \cr
#' 
#'        
#'       \code{count.type = "document.frequency"} \cr
#'    }
#'
#'   \item{Relative document frequency:}{
#'       Filters based on the proportion of documents in which a term occurs. 
#'       \code{count.type = "document.proportion"} or \code{count.type = "ratio"} \cr
#'    }
#'
#'   \item{Absolute term frequency:}{
#'       Filters based on the amount of times a term occurs in the entire corpus, irrespective of documents. \emph{Not recommended}
#'       \code{count.type = "term.frequency"} \cr
#'    }
#'
#'   \item{Absolute term frequency:}{
#'       Filters based on the relative occurence in the entire corpus, irrespective of documents. \emph{Not recommended}
#'       \code{count.type = "term.frequency"} \cr
#'    }
#'  
#'   \item{TF-IDF:}{
#'       Filters based on the proportion of documents in which a term occurs. 
#'       \code{count.type = "tf-idf"} \cr
#'    }
#' } 
#' @export
#' 
#' @usage
#' filter_on_token_prevalence(dtfrog, 
#'     where.token   = "lemma",
#'     count.type    = c("document.frequency",  
#'                       "doc.frequency",
#'                       "document.proportion", 
#'                       "doc.proportion",
#'                       "term.frequency", 
#'                       "term.proportion", 
#'                       "tf-idf"),
#'     remove.what   = c("match", "nomatch", "inside", "outside"), 
#'     ll            = 0,
#'     ul            = +Inf, 
#'     group.by      = NULL, 
#'     store.metrics = FALSE, 
#'     make.copy     = FALSE,
#'     verbose       = TRUE, 
#'     dry.run       = FALSE,
#'     ndocs         = NULL
#' ) 
#'
#' @examples
#'
#' \dontrun{
#' 
#' # ------------------------------------------------------------#
#' # Example 1. Remove lemma that occur in less than 4 documents #
#' # ------------------------------------------------------------#
#'
#' # Load the data: 
#' data("wb.sr.frogged")
#'
#' # Filter 
#' result <- filter_on_token_prevalence(wb.sr.frogged, 
#'     where.token   = "lemma", 
#'     count.type    = "document.frequency",
#'     remove.what   = "outside",
#'     ll            = 4, 
#'     ul            = +Inf, 
#'     store.metrics = T
#' )
#'  
#' result[, list( 
#'     `min doc frequency` = min(token_in_ndocs, na.rm = T),
#'     `max doc frequency` = max(token_in_ndocs, na.rm = T) 
#' )] 
#' 
#' }
#'
#' 
filter_on_token_prevalence  <- function(dtfrog,  
    where.token        =  "lemma",  
    count.type         =  c("document.frequency",  
                            "doc.frequency",
                            "document.proportion", 
                            "doc.proportion",
                            "term.frequency", 
                            "term.proportion", 
                            "tf-idf"),
    remove.what        =  c("match", "nomatch", "inside", "outside"), 
    ll                 =  0,
    ul                 =  +Inf,  
    group.by           =  NULL,
    store.metrics      =  FALSE,
    make.copy          =  FALSE,
    verbose            =  TRUE,
    dry.run            =  FALSE,
    ndocs              =  NULL){
    
    # Checks:	    
	stopifnot(is.frog(dtfrog)) # dtfrog must be a FROG object
    stopifnot(ul >= ll) # ul must be weakly greater than ll 
      
    # Check whether the provided frog object actually has the column name specified in where.token
    stop("Error: the dtfrog does not contain a column named " %+% where.token ) %if%  (!(where.token  %in% (dtfrog %>% colnames)))
  
    args.given  <- match.call() %>% as.list %>% names %-% ""
  
    # Fix the group.by arguments
    group.by.token          <- c(where.token)
    group.by.document       <- c("docid")
    group.by.token.document <- c(group.by.token,group.by.document) 
    
    if("group.by" %in% args.given){
        if(!is.null(group.by)){
            group.by.token           <- c(group.by, group.by.token)
            group.by.document        <- c(group.by, group.by.document)
            group.by.token.document  <- c(group.by, group.by.token.document) 
        }  
    }
    
    # Fix the ndocs_total
    dtfrog[, ndocs_total := uniqueN(docid)]   
    
    if("ndocs" %in% args.given){
        if(!is.null(ndocs)){ 
            stopifnot(ndocs > 0)
            dtfrog[, ndocs_total := ndocs]   
        }
    }        
    
    if("group.by" %in% args.given){
        if(!is.null(group.by)){
            dtfrog[, ndocs_total := .N, by = group.by.document, keyby = c("docid","sent","position")] 
        }
    }
       
    if(verbose){ 
        cat(" \r\n Data transformation: \r\n")
        cat(" \r\n  --------------------------------------------------------------------------- ")  
        cat(" \r\n  Filter (remove) items in FROG object" %+% ifelse(dry.run, " (dryrun)", " (real)")) 
        cat(" \r\n  --------------------------------------------------------------------------- ")   
    }    
       
    remove.what <- match.arg(remove.what) # check validity of remove.what arg (and use default if empty)
    count.type  <- match.arg(count.type)  # check validity of count.type arg (and use default if empty)
 
    # Make counters
    dtfrog[, token_in_ndocs := uniqueN(docid), by = group.by]
    dtfrog[, token_in_ndocs := uniqueN(docid), by = group.by]  
    
    dtfrog[, token_n_in_doc := .N, by = c(group.by, "docid")]  
    dtfrog[, token_in_pdocs := token_in_ndocs / (docid %>% uniqueN) ,]    
    dtfrog[, ndocs := ndocs]
    
    dtfrog[, token_tfidf := token_n_in_doc * log(ndocs/(1+token_in_ndocs))]    		      
             
    setkeyv(dtfrog, c("docid","sent","position")) 
    	    
    setcolorder(dtfrog,neworder = c(c("docid","sent","position"), colnames(dtfrog) %-% c("docid","sent","position")))
      
 
    if(verbose){
        cat("\r\n\r\n  DELETE FROM frogged  data.table \r\n\r\n\t WHERE (i): \r\n\t\t" %+% count.type) 
        cat("Between: ll: ", ll)
        cat("and ul: ", ul)    
    } 
    
    
	if(count.type == "frequency"){    
        if(remove.what %in% c("inside","match")){
            dtfrog <- dtfrog[ll > token_in_ndocs | token_in_ndocs > ul,][order(docid,sent,position)] 
        } else {
            dtfrog <- dtfrog[ll <= token_in_ndocs & token_in_ndocs <= ul,][order(docid,sent,position)]
        }
	}
    
    if(count.type == "proportion"){  
        if(remove.what %in% c("inside","match")){
            dtfrog <- dtfrog[ll > token_in_pdocs | token_in_pdocs > ul,][order(docid,sent,position)] 
        } else {
            dtfrog <- dtfrog[ll <= token_in_pdocs & token_in_pdocs <= ul,][order(docid,sent,position)]
        }
    }  
    
    if(count.type == "tf-idf"){  
        if(remove.what %in% c("inside","match")){
            dtfrog <- dtfrog[ll > token_tfidf | token_tfidf > ul,][order(docid,sent,position)] 
        } else {
            dtfrog <- dtfrog[ll <= token_tfidf & token_tfidf <= ul,][order(docid,sent,position)]
        }
    }  
    
    
    dtfrog[, keyby = c("docid","sent","position")]
    
    return(dtfrog)
     
 }