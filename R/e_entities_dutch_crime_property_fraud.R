#' Entities: Dutch Indicators of Property Crimes (Fraud)
#'
#' This is an example entity list with hard-coded items that could indicate property crimes. 
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
#'
#'
#'
entities_dutch_crime_property_fraud <- function(return.type = c("character","data.table")){ 

    return.type  <- match.arg(return.type)
    
    entities.raw <- '  
        majorpos ; lemma              ;  code
        N        ; bedrog             ;  dutch.dutch.crime.property.fraud
        WW       ; bedriegen          ;  dutch.dutch.crime.property.fraud
        N        ; fraude             ;  dutch.dutch.crime.property.fraud
        WW       ; frauderen          ;  dutch.dutch.crime.property.fraud
        ADJ      ; frauduleus         ;  dutch.dutch.crime.property.fraud
        BIJW     ; frauduleus         ;  dutch.dutch.crime.property.fraud
        WW       ; malversatie        ;  dutch.dutch.crime.property.fraud
        N        ; malverseren        ;  dutch.dutch.crime.property.fraud
        WW       ; onregelmatigheid   ;  dutch.dutch.crime.property.fraud
        N        ; ontvreemding       ;  dutch.dutch.crime.property.fraud
        WW       ; ontvreemden        ;  dutch.dutch.crime.property.fraud
        N        ; verdonkeremaning   ;  dutch.dutch.crime.property.fraud
        WW       ; verdonkeremanen    ;  dutch.dutch.crime.property.fraud
        WW       ; verduisteren       ;  dutch.dutch.crime.property.fraud
        WW       ; verduistering      ;  dutch.dutch.crime.property.fraud
        WW       ; zwendelen          ;  dutch.dutch.crime.property.fraud
        WW       ; zwendel            ;  dutch.dutch.crime.property.fraud

    '
    Encoding(entities.raw) <- "utf-8"
    
    retval <- data.table::fread(input = entities.raw, 
        stringsAsFactors = F, 
        blank.lines.skip = T,
        header = T, 
        sep = ";"   
    )
       
    retval[, lemma := stringi::stri_trim(retval$lemma)]
     
    if(return.type == "character"){
        return(retval$lemma)  
    } else {
        return(retval) 
    }    
}
 