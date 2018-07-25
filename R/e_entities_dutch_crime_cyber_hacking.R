#' Entities: Dutch Indicators of Cyber Crime (Hacking)
#'
#' @param return.type 
#' Use \code{return.type = "character"} to return a character vector (default); 
#' return.type = "data.table" returns a data.table} 
#'
#' @return Character vector or \code{data.table}.
#' @export
#'
#' @examples
entities_dutch_crime_hacking <- function(return.type = c("character","data.table")){ 
 
    return.type <- match.arg(return.type)
  
    entities.raw <- ' 
        majorpos ; lemma                ;    code
        N        ; hack                 ;    dutch.crime.cyber.hacking
        WW       ; hacken               ;    dutch.crime.cyber.hacking
        WW       ; kraken               ;    dutch.crime.cyber.hacking
        N        ; computervredebreuk   ;    dutch.crime.cyber.hacking
        WW       ; encryptie            ;    dutch.crime.cyber.hacking
        WW       ; versleutelen         ;    dutch.crime.cyber.hacking
        BIJW     ; versleuteld          ;    dutch.crime.cyber.hacking
        ADV      ; versleutelen         ;    dutch.crime.cyber.hacking 
    ' 
    
} 
