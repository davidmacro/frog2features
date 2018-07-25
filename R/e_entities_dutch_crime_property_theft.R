#' Entities: Dutch Stopwords
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
entities_dutch_crime_property_theft <- function(return.type = c("character","data.table")){ 
 
    return.type  <- match.arg(return.type)
    
    entities.raw <- ' 
        majorpos ; lemma                ;    code
        WW       ; achterhouden         ;    dutch.crime.property.theft
        WW       ; achteroverdrukken    ;    dutch.crime.property.theft
        WW       ; afnemen              ;    dutch.crime.property.theft
        WW       ; benemen              ;    dutch.crime.property.theft
        N        ; dief                 ;    dutch.crime.property.theft
        N        ; diefstal             ;    dutch.crime.property.theft
        WW       ; gappen               ;    dutch.crime.property.theft
        WW       ; inpikken             ;    dutch.crime.property.theft
        WW       ; jatten               ;    dutch.crime.property.theft
        WW       ; kapen                ;    dutch.crime.property.theft
        WW       ; leegstelen           ;    dutch.crime.property.theft
        WW       ; nakken               ;    dutch.crime.property.theft
        WW       ; ontfutselen          ;    dutch.crime.property.theft
        WW       ; ontnemen             ;    dutch.crime.property.theft
        WW       ; ontvreemden          ;    dutch.crime.property.theft
        WW       ; pijlen               ;    dutch.crime.property.theft
        WW       ; pikken               ;    dutch.crime.property.theft
        WW       ; plunderen            ;    dutch.crime.property.theft
        ADJ      ; proletarisch         ;    dutch.crime.property.theft
        WW       ; roven                ;    dutch.crime.property.theft
        WW       ; schachten            ;    dutch.crime.property.theft
        WW       ; snaaien              ;    dutch.crime.property.theft
        WW       ; stelen               ;    dutch.crime.property.theft
        WW       ; stropen              ;    dutch.crime.property.theft
        WW       ; toeëigenen           ;    dutch.crime.property.theft
        WW       ; verdonkeremanen      ;    dutch.crime.property.theft
        WW       ; verdonkeren          ;    dutch.crime.property.theft
        WW       ; verduisteren         ;    dutch.crime.property.theft
        WW       ; vervreemden          ;    dutch.crime.property.theft
        WW       ; wegfutselen          ;    dutch.crime.property.theft
        WW       ; weggraaien           ;    dutch.crime.property.theft
        WW       ; wegkapen             ;    dutch.crime.property.theft
        WW       ; wegnemen             ;    dutch.crime.property.theft
        WW       ; wegpikken            ;    dutch.crime.property.theft
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