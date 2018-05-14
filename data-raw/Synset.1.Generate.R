rm(list=ls())
setwd("J:/Jeugdcriminaliteit 3/2849b Pilot textmining/Data jaar/stap7-ontwikkelenmachinelearning/deel2-featureconstructie/frog2features/data-raw")
 
require(XML)
require(data.table)
require(magrittr)

library(plyr)

input <- xmlParse(file="odwn_cili.xml",encoding = "UTF-8")

# ----------------------------------------------------------------------------------------------------
# Lexical entries 
# ----------------------------------------------------------------------------------------------------

LexicalEntries <- getNodeSet(input, path = "/LexicalResource/Lexicon/LexicalEntry") 
 
lemma2semantic <- rbindlist(lapply(LexicalEntries, FUN = function(x){  
  
    x.list <- xmlToList(x)
      
    return(data.frame(t(c(x.list$Lemma,x.list$Sense, x.list$.attrs)),stringsAsFactors = F)) 
      
})) 

lemma2semantic[, `:=`(lemma = writtenForm,
                       pos = partOfSpeech,
                       synsetID = id,
                       lemmaID = id.1)] 

Encoding(lemma2semantic$lemma) <- "UTF-8"

lemma2semantic[pos == "a", majorpos := "ADJ"]
lemma2semantic[pos == "n", majorpos := "N"]
lemma2semantic[pos == "v", majorpos := "WW"]

lemma2semantic <- lemma2semantic[, .(synsetID, lemmaID, lemma, synset, majorpos)]

save(lemma2semantic, file="lemma2semantic.dutch.out")


# ----------------------------------------------------------------------------------------------------
# Semantic relations
# ----------------------------------------------------------------------------------------------------

SemanticEntries <- getNodeSet(input, path = "/LexicalResource/Lexicon/Synset") 
 
SemanticEntries.split <- split(SemanticEntries, f = cut(1:length(SemanticEntries),breaks = 100) )
  
semantic2semantic <- rbindlist(lapply(SemanticEntries.split, FUN = function(Entries){
    
    semrels <- rbindlist(lapply(Entries, FUN = function(x){  
     
        sourceid <- xmlGetAttr(x, "id")
        
        x.relations <- rbindlist(getNodeSet(x, path = "SynsetRelation", fun = function(xitem){
            
            return(data.frame(sourceid  = sourceid,
                              targetid  = xmlGetAttr(xitem, "target"),
                              relType   = xmlGetAttr(xitem, "relType"),stringsAsFactors = F))
            
        })) 
      
        return(x.relations) 
          
    })) 
         
    return(semrels)
}))   

save(semantic2semantic, file = "semantic2semantic.dutch.out")
 


 
 