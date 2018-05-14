rm(list=ls())
setwd("J:/Jeugdcriminaliteit 3/2849b Pilot textmining/Data jaar/stap7-ontwikkelenmachinelearning/deel2-featureconstructie/frog2features/data-raw")
 
require(XML)
require(data.table)
require(magrittr)
require(tydr)
require(igraph) 
# ----------------------------------------------------------------------------------------------------
# Lexical entries 
# ----------------------------------------------------------------------------------------------------

load(file="../data/lemma2semantic.dutch.rda")
load(file="../data/semantic2semantic.dutch.rda")
 
lemma2semantic[majorpos == "a", majorpos := "ADJ"]
lemma2semantic[majorpos == "n", majorpos := "N"]
lemma2semantic[majorpos == "v", majorpos := "WW"]

#lemma2semantic[, partOfSpeech := NULL]
#lemma2semantic[, writtenForm := NULL] 
#lemma2semantic[,  id := NULL]
#lemma2semantic[,  synsetID  := NULL]
#lemma2semantic[,  lemmaID  := NULL]
#lemma2semantic[,  id.1 := NULL]
#lemma2semantic[,  pos := NULL]

# Subset columns
lemma2semantic <- lemma2semantic[, .(lemma, synset, majorpos)]

# Bereken de grootte van de synsets:
lemma2semantic[, synset_size := .N, by = synset]

# Bereken in hoeveel synsets een lemma voorkomt: 
lemma2semantic[, in_n_synsets := .N, by = c("lemma", "majorpos")]

# Selecteer alleen maar synsets waar meer dan één woord in zit
lemma2semantic <- lemma2semantic[synset_size > 1,][order(synset)]

# Selecteer alleen maar lemma's die niet in meer dan één synset voorkomen:
lemma2semantic <- lemma2semantic[in_n_synsets == 1,][order(synset)]

# Herbereken de grootte van de synsets:
lemma2semantic[, synset_size := .N, by = synset]

# Selecteer alleen maar synsets waar meer dan één woord in zit
lemma2semantic <- lemma2semantic[synset_size > 1,][order(synset)][, in_n_synsets := NULL]

# Tel het aantal lemma: 
lemma2semantic[, uniqueN(lemma)]

# Tel het aantal synsets: 
lemma2semantic[, uniqueN(synset)]

# Sorteer op synset en dan op lemma: 
setorderv(lemma2semantic, c("synset", "lemma"))

# Zet de replacement values goed: 
lemma2semantic[, lemma.replacement.1 := .SD[1, lemma], by = "synset"]

# Replacement values: 
lemma2semantic[, lemma.replacement.2 := "[" %+% lapply(lemma, function(x){
    paste0(.SD[, lemma], collapse=", ")}) %+% "]"  , by = "synset"] 

lemma.synonyms.dutch <- lemma2semantic

save(lemma.synonyms.dutch, file = "../data/lemma.synonyms.dutch.rda")




