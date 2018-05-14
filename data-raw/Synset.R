rm(list=ls())
setwd("H:/Flow/Todo/2017-11-02-Synset")
 
require(XML)
require(data.table)
require(magrittr)

library(plyr)

input <- xmlParse(file="odwn_cili.xml",encoding = "UTF-8")

# ----------------------------------------------------------------------------------------------------
# Lexical entries 
# ----------------------------------------------------------------------------------------------------

LexicalEntries <- getNodeSet(input, path = "/LexicalResource/Lexicon/LexicalEntry") 
 
lemmas2sense <- rbindlist(lapply(LexicalEntries, FUN = function(x){  
  
    x.list <- xmlToList(x)
      
    return(data.frame(t(c(x.list$Lemma,x.list$Sense, x.list$.attrs)),stringsAsFactors = F)) 
      
})) 

lemmas2sense[, `:=`(lemma        = writtenForm,
                    pos          = partOfSpeech,
                    synset_id    = id,
                    lemma_id     = id.1) ] 

Encoding(lemmas2sense$lemma) <- "UTF-8"

save(lemmas2sense, file="lemmas2sense.out")


# ----------------------------------------------------------------------------------------------------
# Synset entries 
# ----------------------------------------------------------------------------------------------------

SynsetEntries             <- getNodeSet(input, path = "/LexicalResource/Lexicon/Synset") 
 
SynsetEntries.split       <- split(SynsetEntries, f = cut(1:length(SynsetEntries),breaks = 100) )
  
synsets.split <- rbindlist(lapply(SynsetEntries.split, FUN = function(SynsetEntries){
    
    synsets <- rbindlist(lapply(SynsetEntries, FUN = function(x){  
     
        sourceid <- xmlGetAttr(x, "id")
        
        x.relations <- rbindlist(getNodeSet(x, path = "SynsetRelation", fun = function(xitem){
            
            return(data.frame(sourceid  = sourceid,
                              targetid  = xmlGetAttr(xitem, "target"),
                              relType   = xmlGetAttr(xitem, "relType"),stringsAsFactors = F))
            
        } )) 
      
        return(x.relations) 
          
    })) 
         
    return(synsets)
})) 

save(synsets.split, file = "synsets.split.out")
 


 
 