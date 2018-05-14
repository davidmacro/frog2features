rm(list=ls())
setwd("J:/Jeugdcriminaliteit 3/2849b Pilot textmining/Data jaar/stap7-ontwikkelenmachinelearning/deel2-featureconstructie/frog2features/data-raw")
 
require(XML);require(data.table);require(magrittr);require(tydr);require(igraph); require(stringi)
# ----------------------------------------------------------------------------------------------------
# Lexical entries 
# ----------------------------------------------------------------------------------------------------

load(file="../data/lemma2semantic.dutch.rda")
load(file="../data/semantic2semantic.dutch.rda")
 
setwd("J:/Jeugdcriminaliteit 3/2849b Pilot textmining/Data jaar/stap7-ontwikkelenmachinelearning/deel2-featureconstructie/frog2features/data-raw")
source("../R/functionsConvenienceR.R" ) 
source("graph.functions.R" ) 

lemma2semantic[majorpos == "a", majorpos := "ADJ"]
lemma2semantic[majorpos == "n", majorpos := "N"]
lemma2semantic[majorpos == "v", majorpos := "WW"]
 
create <- F

# Create an edgelist
edges <- semantic2semantic[, .(sourceid, targetid,relType)]
edges <- edges[relType == "hypernym"]
 

# Filter the edgelist; all edge names should occur in the lemma2semantics database: 
edges  <- edges[sourceid %chin% lemma2semantic$synset,] 
edges  <- edges[targetid %chin% lemma2semantic$synset,] 

# Get all lemma instances of the semantic set and concatenate them:
if(create == T){
    synsets.dutch.collapsed <- lemma2semantic[, 
        instances := "[" %+% lapply(lemma, function(x){
       paste0(.SD[, lemma], collapse=", ")}) %+% "]"  , by = "synset"
    ]
    save(synsets.dutch.collapsed, file = "synset.dutch.collapsed.Rda")
} else {
    load(file = "synset.dutch.collapsed.Rda")
}
 
graph.semantic.dutch <- graph.data.frame(edges)
  
# Merge instances
instances <- merge(
    x = {data.table(synset = V(graph.semantic.dutch)$name)},
    y = {synsets.dutch.collapsed[, .(synset, instances )]} %>% unique,
    by = c("synset"), 
    all.x = T,
    all.y = F, sort=F
) 

# Fix the sources
edges <- merge(edges, instances, by.x="sourceid",by.y=c("synset"))
edges <- edges[, .(sourceid, targetid, relType, sourceinstance = instances)] 

edges <- merge(edges, instances, by.x="targetid",by.y=c("synset"))
edges <- edges[, .(sourceid, targetid, relType, sourceinstance, targetinstance = instances)] 
  
# Rebuild graph:
graph.semantic.dutch <- graph.data.frame(
    d = edges[, .(sourceid, targetid)]
) 

instances <- merge(
    x = {data.table(synset = V(graph.semantic.dutch)$name)},
    y = {synsets.dutch.collapsed[, .(synset, instances )]} %>% unique,
    by = c("synset"), 
    all.x = T,
    all.y = F, sort=F
) 

V(graph.semantic.dutch)$instances <- instances$instances

V(graph.semantic.dutch)$outdegree <- degree(graph.semantic.dutch, mode = "out")
V(graph.semantic.dutch)$indegree  <- degree(graph.semantic.dutch, mode = "in")

ego.graphs.o5 <- make_ego_graph(graph.semantic.dutch, order = 5, mode = "out")   
ego.graphs.o2 <- make_ego_graph(graph.semantic.dutch, order = 2, mode = "out")   

semantic.properties.dutch <- data.table(
    synset                  = V(graph.semantic.dutch)$name,
    instances               = V(graph.semantic.dutch)$instances,
    outdegree               = V(graph.semantic.dutch)$outdegree,
    neighb.density          = get.neighborhood.density(ego.graphs = ego.graphs.o5),
    neighb.transitivity     = get.neighborhood.transitivity(ego.graphs = ego.graphs.o5),
    neighb.diameter         = get.neighborhood.diameter(ego.graphs = ego.graphs.o5),
    neighb.average.indegree = get.neighborhood.aggregate(ego.graphs = ego.graphs.o2, attribute.name = "indegree")
) 

semantic.properties.dutch.lemma = merge(
    x = semantic.properties.dutch,
    y = lemma2semantic[, .(synset, majorpos, lemma)],
    all.x = F,
    all.y = F, allow.cartesian = T
)

save(semantic.properties.dutch.lemma, file="semantic.properties.dutch.lemma.Rda")


 