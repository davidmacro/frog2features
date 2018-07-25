#' Entities: Dutch Organizations::Banks
#'
#' This is an example entity list with hard-coded items that refer to Dutch Banks
#'
#' Note: this list is not (yet) validated. 
#' 
#' @param return.type 
#' Use \code{return.type = "character"} to return a character vector (default); 
#' return.type = "data.table" returns a data.table} 
#'
#' @return Character vector or \code{data.table}.
#' @export
#'
#' @examples
#' 
#' 
entities_dutch_organizations_banks <- function(return.type = c("character","data.table")){ 
  
    return.type  <- match.arg(return.type)
    
    entities.raw <- '  
        lemma 
        rabo 
        rabobank
        sns 
        nibc
        royal_bank 
        ing 
        sns 
        abn 
        asn 
        knab 
        dnb 
        triodos 
        binck 
        bunq
        leaseplan 
    '    
    
    Encoding(entities.raw) <- "utf-8"
        
    retval <- data.table::fread(input = entities.raw, 
        stringsAsFactors = F, 
        blank.lines.skip = T,
        header = T, 
        sep = ";"   
    )
    
    retval[, lemma := stringi::stri_trim(retval$lemma)]
    retval[, code  := "dutch.org.bank"]
     
    if(return.type == "character"){
        return(retval$lemma)  
    } else {
        return(retval) 
    }     
} 
