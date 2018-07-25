#' Frog DH: Merge FROG dataset
#' 
#' Merge columns in a frogged dataset (\emph{i.e.,} \code{dtfrog}) with some external datasource.  
#' \emph{Note:} Initially, this function was intended as an (invisible) back-end to the 
#' \code{default_rule_,merge} function. But since this function can also be 
#' used on it's own, it seemed natural to expose the function publically. 
#'   
#' @param dtfrog 
#' Frogged dataset. Note: \code{dtfrog} must be a \code{data.table} and must pass 
#' the \code{is.frogged(dtfrog)} check.
#'  
#' @param dtmerge
#' Dataset with columns to append to the dtfrog dataset. 
#'  
#' @param by
#' Column(s) in both \code{dtfrog} and \code{dtmerge} that are the keys to merge on. 
#'    
#' @param by.dtfrog,by.dtmerge,  
#' Vectors of column(s) in both \code{dtfrog} and \code{dtmerge} to merge on. 
#' 
#' @param all.dtfrog 
#' Logical; indicates whether to keep rows in \code{dtfrog} that do not match \code{all.merge}. 
#' Defaults to \code{TRUE}. 
#' This ensures that the returned \code{data.table} has all original data in it. 
#' 
#' @param all.dtmerge  
#' Logical; inicates whether to keep non-merged rows in \code{all.merge} that do not match \code{dtfrog}.  
#' Defaults to \code{FALSE}.IT wouldn't really make sense to set \code{all.dtmerge = TRUE}, 
#' but the option is provided for compatibility with the generic \code{merge} functionality of the \code{data.table} package. 
#'
#' @param cols.dtmerge 
#' Columns in \code{dtmerge} to extract and merge to \code{dtfrog}
#'
#' @param allow.carthesian 
#' Logical 
#'
#' @param collapse 
#' Logical; if true, double entries in \code{dtmerge} will be collapsed together on the fly during merge. 
#' This ensures that the output \code{data.table} retains unique values for each "docid","sent","position" combination. 
#' 
#' @param collapse.separator
#' Character(s) to use to separate items.
#' 
#' @param na.value
#' Value to fill na.values with that were not matched by any of the merges. Defaults to \code{NA}.
#'   
#' @return
#' @export
#'
#' @examples
merge_frog = function(dtfrog, dtmerge,
    by                 = NULL,
    by.dtfrog          = NULL,
    by.dtmerge         = NULL,
    all.dtfrog         = TRUE,
    all.dtmerge        = FALSE,
    cols.dtmerge       = NULL,
    allow.carthesian   = FALSE,
    collapse           = TRUE,
    na.value           = NA,
    collapse.separator = " - "){
  
    # Check mandatory arguments
    stopifnot(is.frog(dtfrog))
    stopifnot(is.data.table(dtmerge)) 
    
    # Check optional arguments 
    args.given  <- match.call() %>% as.list %>% names %-% ""
    
    # Check whether "by" or "by.dtfrog" and "by.dtmerge" are specified, but not both:
    c1 <- "by" %in% args.given
    c2 <- c("by.dtfrog", "by.dtmerge") %in% args.given %>% all
    
    if(!xor(c1,c2)){
        stop("Param \"by\" or \"by.dtfrog\" and \"by.dtmerge\" need to be specified (not both)")
    }
    
    
    # Fix by.dtfrog
    if(c1){
        by.dtfrog  <- by
        by.dtmerge <- by
    }
    
    # Check whether to-merge-columns exist
    stopifnot(by.dtfrog  %in% colnames(dtfrog)  %>% all) 
    stopifnot(by.dtmerge %in% colnames(dtmerge) %>% all) 
  
    # If allow.carthesian, merge everything together
    if(allow.carthesian){ 
        
        dtfrog.merged <- merge(
    	    x     = dtfrog, 
    		y     = dtmerge[, c(by.dtmerge, cols.dtmerge), with=F], 
    		by.x  = by.dtfrog,
    	 	by.y  = by.dtmerge, 
    		all.x = all.dtfrog,
    		all.y = all.dtmerge, 
    		allow.carthesian = T, 
         ) 
         
    }  else {
        
        dtfrog.merged <- dtfrog
        
        col.to.merge.previous <- ""
        
        # If we merge multiple properties, check per column whether there are duplicates
        for(col.to.merge in cols.dtmerge){
            
            cat("Trying to merge column: " %+% col.to.merge %+% "\r\n")
            
            dtmerge.tmp <- dtmerge[, c(by.dtmerge, col.to.merge), with = F]
            
            # Check whether the column contains duplicates; if so: fix
            if(dtmerge.tmp[, .N, by = by.dtmerge][, max(N) > 1]){
                
                if(collapse){
                    
                    setorderv(dtmerge.tmp, c(by.dtmerge,col.to.merge))
                    
                    dtmerge.tmp <- dtmerge.tmp[,
                        (col.to.merge) := paste0(get(col.to.merge) %>% unique, collapse = collapse.separator), by = c(by.dtmerge)
                    ] %>% unique 
                   
                } else {
                    stop("Error: Cannot merge " %+% col.to.merge  %+% " because allow.carthesian = FALSE 
                          and collapse = FALSE, yet there are duplicates.")
                } 
            } 
             
            dtfrog.merged <- merge(
                x     = dtfrog.merged, 
                y     = dtmerge.tmp, 
                by.x  = by.dtfrog,
                by.y  = by.dtmerge, 
                all.x = all.dtfrog,
                all.y = all.dtmerge, 
                suffixes = c("", ".y"),
                allow.carthesian = F
            )  
        }
         
    }
     
    if(!is.null(na.value)){
        
        cat("Fix na.value")
        
        dtfrog.merged[, (cols.dtmerge) := lapply(.SD, function(x){
            ifelse(is.na(x),na.value,x)
        }), .SDcols = cols.dtmerge]
    }
    
	cols <- c("docid", "sent", "position")
				
	setcolorder(x = dtfrog.merged, neworder = c(c(cols), c(colnames(dtfrog.merged) %-% cols))) 
	
	setorderv(dtfrog.merged, c("docid", "sent", "position")) 
	
	return(dtfrog.merged) 
}