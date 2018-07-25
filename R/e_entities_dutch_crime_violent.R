#' Entities: Dutch Indicators of Violent Crime 
#'
#' @param return.type 
#' Use \code{return.type = "character"} to return a character vector (default); 
#' return.type = "data.table" returns a data.table} 
#'
#' @return Character vector or \code{data.table}.
#' @export
#'
#' @examples 
entities_dutch_crime_violent <- function(return.type = c("character","data.table")){ 
     
    return.type  <- match.arg(return.type)
    
    entities.raw <- ' 
         majorpos  ;     lemma           ;  code
         N         ;     agressie        ;  dutch.crime.violent
         ADJ       ;     agressief       ;  dutch.crime.violent
         ADV       ;     agressief       ;  dutch.crime.violent
         WW        ;     doden           ;  dutch.crime.violent
         N         ;     dood            ;  dutch.crime.violent
         WW        ;     doodmaken       ;  dutch.crime.violent
         WW        ;     geweld          ;  dutch.crime.violent
         ADJ       ;     gewelddadig     ;  dutch.crime.violent
         ADV       ;     gewelddadig     ;  dutch.crime.violent
         N         ;     knuppel         ;  dutch.crime.violent
         WW        ;     martelen        ;  dutch.crime.violent
         WW        ;     mishandelen     ;  dutch.crime.violent
         WW        ;     neermaaien      ;  dutch.crime.violent
         WW        ;     pijnigen        ;  dutch.crime.violent
         N         ;     pistool         ;  dutch.crime.violent
         WW        ;     schieten        ;  dutch.crime.violent
         WW        ;     schoppen        ;  dutch.crime.violent
         WW        ;     slaan           ;  dutch.crime.violent
         WW        ;     sterven         ;  dutch.crime.violent
         WW        ;     vermoorden      ;  dutch.crime.violent
         WW        ;     verwonden       ;  dutch.crime.violent
         N         ;     verwonding      ;  dutch.crime.violent
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