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
entities_dutch_stopwords <- function(return.type = c("character","data.table")){ 
   
    return.type <- match.arg(return.type)
   
    entities.raw <- ' 
        lemma           ;   code
        aan             ;   dutch.stopwords
        af              ;   dutch.stopwords
        al              ;   dutch.stopwords
        alles           ;   dutch.stopwords
        als             ;   dutch.stopwords
        altijd          ;   dutch.stopwords
        andere          ;   dutch.stopwords
        ben             ;   dutch.stopwords
        bij             ;   dutch.stopwords
        daar            ;   dutch.stopwords
        dan             ;   dutch.stopwords
        dat             ;   dutch.stopwords
        de              ;   dutch.stopwords
        der             ;   dutch.stopwords
        deze            ;   dutch.stopwords
        die             ;   dutch.stopwords
        dit             ;   dutch.stopwords
        doch            ;   dutch.stopwords
        doen            ;   dutch.stopwords
        door            ;   dutch.stopwords
        dus             ;   dutch.stopwords
        een             ;   dutch.stopwords
        eens            ;   dutch.stopwords
        en              ;   dutch.stopwords
        er              ;   dutch.stopwords
        ge              ;   dutch.stopwords
        geen            ;   dutch.stopwords
        geweest         ;   dutch.stopwords
        haar            ;   dutch.stopwords
        had             ;   dutch.stopwords
        heb             ;   dutch.stopwords
        hebben          ;   dutch.stopwords
        heeft           ;   dutch.stopwords
        hem             ;   dutch.stopwords
        het             ;   dutch.stopwords
        hier            ;   dutch.stopwords
        hij             ;   dutch.stopwords
        hoe             ;   dutch.stopwords
        hun             ;   dutch.stopwords
        iemand          ;   dutch.stopwords
        iets            ;   dutch.stopwords
        ik              ;   dutch.stopwords
        in              ;   dutch.stopwords
        is              ;   dutch.stopwords
        ja              ;   dutch.stopwords
        je              ;   dutch.stopwords
        kan             ;   dutch.stopwords
        komen           ;   dutch.stopwords
        kon             ;   dutch.stopwords
        kunnen          ;   dutch.stopwords
        maar            ;   dutch.stopwords
        me              ;   dutch.stopwords
        meer            ;   dutch.stopwords
        men             ;   dutch.stopwords
        met             ;   dutch.stopwords
        mij             ;   dutch.stopwords
        mijn            ;   dutch.stopwords
        moet            ;   dutch.stopwords
        na              ;   dutch.stopwords
        naar            ;   dutch.stopwords
        niet            ;   dutch.stopwords
        niets           ;   dutch.stopwords
        nog             ;   dutch.stopwords
        nu              ;   dutch.stopwords
        of              ;   dutch.stopwords
        om              ;   dutch.stopwords
        omdat           ;   dutch.stopwords
        onder           ;   dutch.stopwords
        ons             ;   dutch.stopwords
        ook             ;   dutch.stopwords
        op              ;   dutch.stopwords
        over            ;   dutch.stopwords
        reeds           ;   dutch.stopwords
        te              ;   dutch.stopwords
        tegen           ;   dutch.stopwords
        toch            ;   dutch.stopwords
        toen            ;   dutch.stopwords
        tot             ;   dutch.stopwords
        u               ;   dutch.stopwords
        uit             ;   dutch.stopwords
        uw              ;   dutch.stopwords
        van             ;   dutch.stopwords
        veel            ;   dutch.stopwords
        voor            ;   dutch.stopwords
        want            ;   dutch.stopwords
        waren           ;   dutch.stopwords
        was             ;   dutch.stopwords
        wat             ;   dutch.stopwords
        we              ;   dutch.stopwords
        wel             ;   dutch.stopwords
        werd            ;   dutch.stopwords
        weten           ;   dutch.stopwords
        wezen           ;   dutch.stopwords
        wie             ;   dutch.stopwords
        wij             ;   dutch.stopwords
        wil             ;   dutch.stopwords
        worden          ;   dutch.stopwords
        wordt           ;   dutch.stopwords
        zal             ;   dutch.stopwords
        ze              ;   dutch.stopwords
        zei             ;   dutch.stopwords
        zelf            ;   dutch.stopwords
        zich            ;   dutch.stopwords
        zij             ;   dutch.stopwords
        zijn            ;   dutch.stopwords
        zo              ;   dutch.stopwords
        zonder          ;   dutch.stopwords
        zou             ;   dutch.stopwords
        zullen          ;   dutch.stopwords
        
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




