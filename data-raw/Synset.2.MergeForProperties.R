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


load(file = "lemma2semantic.dutch.out")

load(file = "synsets.split.out")
load(file = "lemmas2sense.out")

# Relaties tussen synsets 
ties          <-  synsets.split[, .(sourceid, targetid,relType)]  %>% unique

# Filter ties op hyperonymen (hogere betekenisklasses)
ties          <-  ties[relType == "hypernym",]

lemmas2synset <- lemmas2sense[, .(lemma, synset )]
lemmas2synset <- lemmas2synset[, lemmas := paste0(lemma, collapse = ","), by=synset][, .(synset, lemmas)] %>% unique

# Elke synset kan meerdere lemmas bevatten 
lemmas_source <- lemmas2synset[, .(sourceid = synset, lemmas_source=lemmas)]; setkeyv(lemmas_source, "sourceid")
lemmas_target <- lemmas2synset[, .(targetid = synset, lemmas_target=lemmas)]; setkeyv(lemmas_target, "targetid")

ties <- merge(x = ties, y = lemmas_source, by = "sourceid", all.x = T, all.y=F)
ties <- ties[!is.na(lemmas_source),]
 
ties <- merge(x = ties, y = lemmas_target, by = "targetid", all.x = T, all.y=F)
ties <- ties[!is.na(lemmas_target),]
 

synset.hypernyms.dutch <- ties

save(synset.hypernyms.dutch, file = "synset.hypernyms.dutch.out")
 

# Maak een graph-object om relevante centraliteitsmaten te berekenen:
g.ties <- graph.edgelist(as.matrix(ties[, .(targetid, sourceid)]))

# Properties
 
V(g.ties)$indegree    <-  degree(g.ties, mode=c("in"))
V(g.ties)$outdegree   <-  degree(g.ties, mode=c("out")) 
V(g.ties)$betweenness <-  log(betweenness.estimate(g.ties, cutoff=3,directed = T)+1)
V(g.ties)$closeness   <-  -1/log(closeness.estimate(g.ties, cutoff=3))
V(g.ties)$coreness    <-  coreness(g.ties)
V(g.ties)$pagerank    <-  exp(page.rank(g.ties,algo="prpack")$vector)
V(g.ties)$nbsize      <-  neighborhood.size(g.ties, 2, mode="in", mindist = 2)
 

synset.properties.dutch <- data.table(data.frame(
    synset        = V(g.ties)$name, 
    indegree      = V(g.ties)$indegree ,
    outdegree     = V(g.ties)$outdegree ,
    betweenness   = V(g.ties)$betweenness ,
    closeness     = V(g.ties)$closeness, 
    coreness      = V(g.ties)$coreness,
    pagerank      = V(g.ties)$pagerank,
    nbsize        = V(g.ties)$nbsize
))
  
# Merge de lemmas er weer bij 

# (1) Extraheer lemma, pos en synset
lemmas_mergeback <- lemmas2sense[,  .(lemma, partOfSpeech, synset)];

# (2) Hercodeer de majorpos variable; onderscheidt momenteel maar drie varianten:
#  a = bijwoord
#  n = bijvoeglijk naamwoord
#  v = werkwoord
 

lemmas_mergeback[partOfSpeech == "a", majorpos := "ADJ"]
lemmas_mergeback[partOfSpeech == "n", majorpos := "N"]
lemmas_mergeback[partOfSpeech == "v", majorpos := "WW"]

setkeyv(       lemmas_mergeback, "synset")
setkeyv(synset.properties.dutch, "synset") 

synset.properties.dutch <- lemmas_mergeback[synset.properties.dutch]
save(synset.properties.dutch, file = "synset.properties.dutch.out")










# Todo: relationele kenmerken

#dt <- as_long_data_frame(g.ties) %>% setDT()
#dt[, by = from]

#cor(synset.properties[, .(indegree,outdegree,betweenness,coreness,closeness,pagerank,nbsize)])



#lemmas <- lemmas_source[, .(synset = sourceid, lemmas = lemmas_source)]
#lemmas <- merge(synset.properties, lemmas, by = "synset", all.x = T, all.y=F)
#lemmas <- lemmas[,   lapply(.SD , FUN=function(x){unlist(tstrsplit(x, ","))}), by=.I] 

#setkeyv(lemmas, "synset")



 