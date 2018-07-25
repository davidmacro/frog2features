#' Internal method to process all data transformations
#'
#' @param FeatureDefinitions 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
getRequiredDataTransformations <- function(FeatureDefinitions, verbose = F){
     
    # Concatenate all required data transformations    
    dt.edit <- FeatureDefinitions %>% lapply(function(x){
        x$DataTransformators$getDataTransformators()
    }) %>% rbindlist(use.names = T, idcol = "feature", fill = T)
 
    stop("All features should be named.") %if% any({FeatureDefinitions %>% names %>% nchar} == 0)
  
    # Replace the label of the output object with the featurename
    dt.edit[type == "output", vertex.label := feature]
     
    # (1) Give each unique transformation a unique ID
    dt.edit[, tid := "t" %+% .GRP, by = c(colnames(dt.edit) %-% c("feature","type"))]
    
    # (2) Copy the dt and fix strings-as-factors 
    dt.optim <- dt.edit %>% copy %>% FixStringsFactors
    
    # (3) Lag to get the full history of a dh step 
    dt.optim[, tr1.hist := shift(tid, (.N-1):0, type = "lag") %>% paste  , by = feature] 
    
    # (4) Replace the historical paths for readability
    dt.optim[ , tr1.hist := tr1.hist %>% 
                str_replace_all(pattern = "NA, ","") %>% 
                str_replace_all(pattern = "NA","")] 
     
    # (5) Group by similar histories; note: nodes that share histories can use the same dataset
    dt.optim[, tr1.hist := "th" %+% .GRP, by = tr1.hist]
    
    # (6) Lag by similarity in history   
    dt.optim[, `:=`(trans1 = tr1.hist, 
                    trans2 = shift(tr1.hist,1,type = "lead")) , by = feature]

    # (7) if the "to" is empty, replace with feature
    dt.optim[trans2 %>% is.na, trans1 := feature]  
    dt.optim[trans2 %>% is.na, trans2 := feature]  
 
    dt.optim[order %>% is.na, order := 0]
    dt.optim[, order := (order == 0) * .N + (order > 0) * order , by = feature]
 
    
    # 
    forelast.elements.ids          <- {dt.optim[, .SD[,.I] == .N - 1, by = feature]}$V1 %>% which
    forelast.elements.replacements <- {dt.optim[, trans2[.N]  , by = feature]}$V1

    dt.optim[forelast.elements.ids, trans2 := forelast.elements.replacements]
    
    dt.optim[tr1.hist != trans1, tr1.hist := trans1]
    
    # Get paths: 
    edge.data  <- {dt.optim[trans2 %>% is.na %>% not, .(trans1, trans2)] }[order(trans2)] %>% unique %>% data.frame
  
    # Vertex attributes: 
    vertex.data <- dt.optim[, c("trans1", colnames(dt.optim) %-% c("tr1.hist","trans2","trans1","tid", "order", "feature")), with=F] %>% unique

    dh.graph <- graph.data.frame(d=edge.data,vertices = vertex.data) 
   
    
    # Prepare object to save:
    full.dt <- dt.optim
    
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    # Graph representations:
    # --------------------------------------------------------------------------------------------------------------------------------------------------
    
    # (1) all required data.transformation steps for all features (full)
    full.graph <-  dh.graph %>% simplify
    
    # Determine whether to save an intermediary dataset; this needs to be done if outdegree > 1 
    vertex.attributes(full.graph)$save.intermediary <- (degree(full.graph, mode = "out") > 1) * 1.0
    
    # (2) all required data.transformation steps per features 
    full.paths <- full.graph %>% 
                  all_simple_paths(from = V(full.graph)[vertex.attributes(full.graph)$type == "input"],
                                    to  = V(full.graph)[vertex.attributes(full.graph)$type == "output"]) %>% 
                  lapply(FUN =  function(x){induced_subgraph(full.graph, x)})
    
    # (3) all required names steps per features 
    names(full.paths) <- lapply(full.paths, function(x){
        {x %>% vertex.attributes}$vertex.label %>% tail(1)
    })
    
    # (4) Reorder the paths in order of paths
    full.paths <- full.paths[full.paths %>% names %>% order]
 
    # (5) Fix the graph (assign datasets and parent datasets)
    full.graph <- full.graph %>% 
                    assign_dataset_ids(dt.paths = full.paths) %>% 
                        assign_parent_dataset_ids  
     
    # (6) Derive the graph for the replay-actions
    replay.graph <- full.graph %>% 
                           replace.vertex.attribute(attr.selected        = "type", 
                                                    attr.selected.values = "save.dictionary", 
                                                    replacement.value    = "load.dictionary")  %>%
                           replace.vertex.attribute(attr.selected        = "type", 
                                                    attr.selected.values = "load.dictionary", 
                                                    attr.to.replace      = "vertex.label",
                                                    replacement.value    = "Load dictionary")  %>% 
                           replace.vertex.attribute(attr.selected        = "type", 
                                                    attr.selected.values = "load.dictionary", 
                                                    attr.to.replace      = "allow.replay",
                                                    replacement.value    = 1)                  %>% 
                           delete.vertex.collapse(attr.name = "allow.replay", attr.values = 0) 
     
    # (7) Assign proper paths 
    replay.paths <-  replay.graph %>% 
                          all_simple_paths(from = V(replay.graph)[vertex.attributes(replay.graph)$type == "input"],
                                            to  = V(replay.graph)[vertex.attributes(replay.graph)$type == "output"]) %>% 
                          lapply(FUN =  function(x){induced_subgraph(replay.graph, x)})
    
    # (8) Assign proper paths 
    names(replay.paths) <- lapply(replay.paths, function(x){
        {x %>% vertex.attributes}$vertex.label %>% tail(1)
    })
    
    # (9) Reorder the paths in order of paths
    replay.paths <- replay.paths[replay.paths %>% names %>% order]
    
    # (10) Fix the graph (assign datasets and parent datasets)
    replay.graph <- replay.graph %>% 
                       assign_dataset_ids(dt.paths = replay.paths) %>% 
                           assign_parent_dataset_ids  
    
    list(
        full.dt            = dt.optim,
        full.graph         = full.graph, 
        full.paths         = full.paths,
        replay.graph       = replay.graph, 
        replay.paths       = replay.paths
    )  %>% return     
 
} 