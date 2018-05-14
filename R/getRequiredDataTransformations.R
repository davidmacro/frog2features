getCallFromRule <- function(rule, 
                            data.input              = NULL, 
                            dictionary              = NULL,
                            verbose                 = F,
                            return.type             = "output",
                            transformations.to.save = NULL){
     
    rule.tmp <- getNaRuleList() %>% modifyList(rule  , keep.null = T)
    rule.tmp %<>% unserialize_rule
    rule.tmp <- rule.tmp[!is.na(rule.tmp)] 
    
    retval <- data.input
    
    stop("Rule-type not defined") %if% (rule.tmp$type %>% is.na)
    stop("Rule-type invalid")     %if% (rule.tmp$type %notin% getValidRuleTypes())
    stop("Return-type invalid")   %if% (return.type %notin% c("output", "call"))
     
    call.tmp <- "return(TRUE)"
     
    if(data %>% is.null){  
        
        call.tmp %>% return 
        
    } else {
        
        if(rule.tmp$type == "input"){  
             call.tmp %>% return             
        } 
        
        if(rule.tmp$type == "output"){  
             call.tmp %>% return             
        } 
        
        if(rule.tmp$type == "apply.tag.chain"){
 
            retval <- data.input %>% copy
             
            for(tag.item in rule.tmp$tag.items){ 
                  
                retval <- getCallFromRule(
                    rule       = tag.item,
                    data.input = retval
                ) %>% setorderv(cols = c("docid","sent","position")) 
                
            }
            
            retval %>% eval %>% return
        } 
        
        if(rule.tmp$type == "apply.tag"){
             
            fname <- "apply_tag" 
            args.c <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
            args.c$dtfrog <- data.input %>% copy 
            cat(fname) %if% {verbose == T} 
            
            retval <- c(list(fname %>% get), args.c) %>% as.call 
            
        } 
         
        if(rule.tmp$type == "replace.chain"){
            
            retval <- data.input
            
            for(replacement.rule in rule.tmp$replace.items){ 
                 
                retval <- getCallFromRule(
                        rule       = replacement.rule,
                        data.input = retval
                ) %>% setorderv(cols = c("docid","sent","position")) 
            }
            
            retval %>% return
        } 
        
        if(rule.tmp$type == "replace.rule"){
            
            fname <- "replace_in_frog" 
            args.c <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
            args.c$dtfrog <- data.input 
            cat(fname) %if% {verbose == T} 
            retval <- c(list(fname %>% get), args.c) %>% as.call 
 
        }
        
        if(rule.tmp$type == "apply.filter"){
            
            if(rule.tmp$filter.type == "exclude"){
         
                if(all(rule.tmp$ll %>% is.na %>% not,rule.tmp$ul %>% is.na %>% not)){ 
                    
                     fname         <- "exclude_on_token_prevalence" 
                     args.c        <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
                     args.c$dtfrog <- data.input
                     
                     cat(fname) %if% {verbose == T}
                     
                     retval <- c(list(fname %>% get), args.c) %>% as.call 
                } 
               
                if( (rule.tmp$items %>% length) > 1 ){
                      
                     fname         <- "exclude_on_token_content" 
                     args.c        <- lookupArgs(fname, rule.tmp,drop = c("dtfrog", "items")) 
                     args.c$dtfrog <- data.input 
                     args.c$items  <- rule.tmp$items
                         
                     cat(fname) %if% verbose
                       
                     retval <- c(list(fname %>% get), args.c) %>% as.call 
                     
                } 
                
                if( (rule.tmp$items %>% length) == 1 ){
                    
                     if(rule.tmp$items %>% is.na %>% not){
                    
                         fname         <- "exclude_on_token_content" 
                         args.c        <- lookupArgs(fname,rule.tmp,drop = c("dtfrog", "items")) 
                         args.c$dtfrog <- data.input
                         
                         if(length(args.c$items) == 1){
                            args.c$items  <- call(rule$items) %>% eval    
                         }  
                         
                         cat(fname) %if% verbose
                         
                         retval <- c(list(fname %>% get), args.c) %>% as.call 
                         
                    } 
                }
                 
            } 
             
        }
        
        if(rule.tmp$type == "construct.features"){
             
              cat("Execute " %+% rule.tmp$do) %if% verbose
             
              fname          <- rule.tmp$do
              args.c         <- lookupArgs(fname, rule = rule.tmp, drop = c("dtfrog", "dictionary")) 
              args.c$dtfrog  <- data.input
                 
              if("dictionary" %in% (args.c %>%names)){
                  if(dictionary %>% is.null %>% not){
                      args.c$dictionary <- dictionary
                  }
              }
           
              retval <- c(list(fname %>% get), args.c) %>% as.call 
            
        }
        
        
        if(rule.tmp$type == "create.ngram"){ 
               
             cat("Create ngram") %if% verbose
            
             fname       <- "create_ngram_fast" 
             
             args.c        <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
             args.c$dtfrog <- data.input
                     
             cat(fname) %if% verbose
                     
             retval <- c(list(fname %>% get), args.c) %>% as.call 
             
        } 
         
        
        if(rule.tmp$type == "create.dictionary"){ 
               
             cat("Create dictionary") %if% verbose
            
             fname       <- "create_dictionary" 
             
             args.c        <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
             args.c$dtfrog <- data.input
                     
             cat(fname) %if% verbose
                     
             retval <- c(list(fname %>% get), args.c) %>% as.call 
             
        }  
        
        if(rule.tmp$type == "save.dictionary"){ 
               
             cat("Save dictionary") %if% verbose
            
             fname       <- "save_dictionary" 
             
             args.c        <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
             args.c$dtfrog <- data.input
                     
             cat(fname) %if% verbose
                     
             retval <- c(list(fname %>% get), args.c) %>% as.call 
             
        }  
        
        
        if(rule.tmp$type == "load.dictionary"){ 
               
             cat("Load dictionary") %if% verbose
            
             fname       <- "load_dictionary" 
             
             args.c      <- lookupArgs(fname, rule.tmp) 
           
             args.c %>% print
             
             cat(fname) %if% verbose
                     
             retval <- c(list(fname %>% get), args.c) %>% as.call 
             
        }  
        
        
        if(rule.tmp$type == "merge"){ 
               
            
            if(rule.tmp$merge.type == "synset"){
                
                 cat("Merge synsets") %if% verbose
            
                 fname       <- "merge_synsets" 
                 
                 args.c        <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
                 args.c$dtfrog <- data.input
                         
                 cat(fname) %if% verbose
                         
                 retval <- c(list(fname %>% get), args.c) %>% as.call 
                    
            }
            
            if(rule.tmp$merge.type == "hyperonymset"){
                
                 cat("Merge synsets") %if% verbose
            
                 fname       <- "merge_hyperonymset" 
                 
                 args.c        <- lookupArgs(fname, rule.tmp,drop = c("dtfrog")) 
                 args.c$dtfrog <- data.input
                         
                 cat(fname) %if% verbose
                         
                 retval <- c(list(fname %>% get), args.c) %>% as.call 
                     
            }
             
        }  
          
    } 
     
    if(rule.tmp$type %in% c("apply.filter", "replace.chain", "replace.rule", "apply.tag")){
        retval  %>% eval %>% setorderv(cols = c("docid","sent","position")) %>% return    
    } else {
         retval %>% eval %>% return
    }
     
} 

getRequiredDictionaryList <- function(paths, graph){
  
    Dictionaries <- list()
  
    
    for(path.name in c(paths %>% names)){
        
        dictionaryID <- NA
        dataID       <- NA
        
        bool1 <- ("create.dictionary" %in% V(paths[[path.name]])$type) 
        bool2 <- ("load.dictionary"   %in% V(paths[[path.name]])$type)
        
        if(any(bool1,bool2)){ 
            
            if(bool1){ 
                dic.node    <- V(paths[[path.name]])[V(paths[[path.name]])$type %in% c("create.dictionary") %>% which][1]  
            } 
         
            if(bool2){ 
                dic.node    <- V(paths[[path.name]])[V(paths[[path.name]])$type %in% c("load.dictionary") %>% which][1]  
            }
            
            dic.node.graph <- V(graph)[V(graph)$name == dic.node$name]
             
            dt <- get.parent.nodes(node.name = dic.node.graph$name, graph = graph, attr = "data.id.current")
             
            dictionaryID <- dic.node.graph$data.id.current
            dataID       <- dt[, (data.id.current %-% (dictionaryID)) %>% max]
    
        } else {
            
            fc.create.node <- V(paths[[path.name]])[V(paths[[path.name]])$type %in% c("construct.features") %>% which][1]  
            
            dt <- get.parent.nodes(node.name = fc.create.node$name, graph = graph, attr = "data.id.current")
            
            dataID <- dt[, (data.id.current) %>% max]
        }
        
        Dictionaries[[path.name]] <- list(
            dictionaryID = dictionaryID, 
            dataID = dataID
        )
       
    }
    return(Dictionaries)
}


 
replace.vertex.attribute <- function(graph, attr.selected, attr.selected.values, attr.to.replace = attr.selected, replacement.value){
    
    vertices.to.modify <- (vertex_attr(graph, attr.selected) %in% attr.selected.values) %>% which
    
    for(vertex.to.modify in vertices.to.modify){ 
        graph %<>% set_vertex_attr(name = attr.to.replace , index = V(graph)[vertex.to.modify], value = replacement.value)
    }
    
    return(graph)
}

delete.vertex.collapse <- function(graph, attr.name = "name", attr.values){
     
    if(is.directed(graph)){
     
        for(value in attr.values){
            
            vnames <- V(graph)$name[vertex_attr(graph, attr.name) %in% value %>% which]
       
            for(vname in vnames){
                
                vid <- (V(graph)$name == vname) %>% which
                
                v.from   <- make_ego_graph(graph, order = 1, mode="in")[[vid]]
                v.to     <- make_ego_graph(graph, order = 1, mode="out")[[vid]]
                
                v.from %<>% delete.vertices((V(v.from)$name == vname) %>% which) 
                v.to   %<>% delete.vertices((V(v.to)$name == vname)   %>% which)  
                     
                for(v1 in V(v.from)$name){ 
                    for(v2 in V(v.to)$name){ 
                         graph <- add.edges(graph, edges = c(v1,v2)) 
                    }
                } 
                  
                graph %<>% delete.vertices(V(graph)[vid]) %>% simplify
                 
            }    
        } 
    } 
    return(graph)
    
}
 

 
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

f2f.rank.proper <- function(x){ 
    
    {list(V1 = x) %>% as.data.table}[, rank := .GRP, by = V1][, c(rank)] %>% return
    
} 

getVertexAttributes <- function(dt.graph, vertex.name){

    attributes <- {dt.graph %>% vertex.attributes} %>% lapply(function(x){
        return(x[which(V(dt.graph)$name == vertex.name)])  
    })
    
    return(attributes)
}
 
getRuleFromVertexAttributes <- function(dt.graph, vertex.name){
    
    rule.from.attributes <- dt.graph %>% getVertexAttributes(vertex.name=vertex.name)
    tmp.rule <- getNaRuleList() %>% modifyList(rule.from.attributes,keep.null = T) 
    return(tmp.rule)
}

UpdateVertexAttributes <- function(dt.graph, vertex.name, new.rule, verbose=T){
    
    cat("Updating attributes for: ", vertex.name, "\r") %if% (verbose)
    
    for(name in names(new.rule)){
        
        cat("Name: ",  name,       "\r\n") %if% (verbose)
        cat("Value: ", new.rule[[name]], "\r\n") %if% (verbose)
        
        dt.graph %<>% set_vertex_attr(name  = name, 
                                      index = which(V(dt.graph)$name == vertex.name),
                                      value = new.rule[[name]])
          
    }
    
    return(dt.graph)
}

 
 
assign_parent_dataset_ids <- function(dt.graph){
 
    # Create an empty vector to store the parent ids: 
    V(dt.graph)$data.id.previous <- NA
    
    for(node.name in V(dt.graph)$name){
    
        node.id <- (V(dt.graph)$name == node.name) %>% which
        
        V(dt.graph)[node.id]$data.id.previous <- get.parent.node(node.name, dt.graph, "data.id.current", 1)
        
    }
     
    return(dt.graph)
}

get.indegree <- function(node.name, g){
    degree(g, v = V(g)[(V(g)$name == node.name) %>% which], mode="in")
}

get.outdegree <- function(node.name, g){
    degree(g, v = V(g)[(V(g)$name == node.name) %>% which], mode="out")
}

is.tree <- function(tree){
    
     # Path should be an igraph object:
    return(FALSE) %ifnot% (tree %>% is.igraph)
    
    # Path should be directed:
    return(FALSE) %ifnot% (tree %>% is.directed)
     
}

is.path <- function(path){
    
    # Path should be an igraph object:
    return(FALSE) %ifnot% (path %>% is.igraph)
    
    # Path should be directed:
    return(FALSE) %ifnot% (path %>% is.directed)
    
    # Paths should have exactly one component:
    return(FALSE) %ifnot% (components(path)$no == 1) 
    
    d.out <- degree(path,mode = "out")
    d.in <- degree(path,mode = "in")
    
    # In a path, there should be one and only one vertex with out-degree = 0
    return(FALSE) %ifnot% (sum((d.out == 0)*1.0) == 1)
    
    # In a path, there should be one and only one vertex with out-degree = 0
    return(FALSE) %ifnot% (sum((d.in == 0)*1.0) == 1)
  
    # Should be one and only one vertex with out-degree = 0
    return(FALSE) %ifnot% (sum((d.out == 1)*1.0) == length(V(path)) -1)
    
    # Should be one and only one vertex with in-degree = 0
    return(FALSE) %ifnot% (sum((d.in == 1)*1.0) == length(V(path)) -1)
  
    return(TRUE)
     
}
 

has.parent <- function(node.name, graph){
    
    node.id <- (V(graph)$name == node.name) %>% which 
    
    return(degree(graph = graph, v = node.id, mode = "in") > 0)
}

get.parent.nodes <- function(node.name, graph, attr = c("name", "vertex.color"),na.value = NA, data.table = T){
     
    node.id <- (V(graph)$name == node.name) %>% which 
    
    path <- all_simple_paths(graph, 
                             from = V(graph)[vertex.attributes(graph)$type == "input"],
                             to   = V(graph)[vertex.attributes(graph)$name == node.name])[[1]]
     
    subgraph <- induced_subgraph(graph,path)
    
    if("name" %notin% attr){
        attr <- c("name", attr)
    }
    
    result <- lapply(attr, function(x){
        get.vertex.attribute(subgraph, x)
    }) %>% as.data.table %>% FixStringsFactors 
    
    colnames(result) <- attr
    
    result <- result[(name == node.name) %>% not, ]
   
    return(result)
    
}

get.parent.node <- function(node.name, graph, attr = "name", na.value = FALSE){
    
    #stop("Provided graph not a path") %ifnot% is.path(path)
    
    node.id <- (V(graph)$name == node.name) %>% which 
    
    # Get neightborhood of ego (only indegree)
    parent.graph <- make_ego_graph(graph, order = 1, mode = "in", nodes = node.id)[[1]]  
    
    # Remove ego from the parent graph
    parent.graph <- delete.vertices(parent.graph, (V(parent.graph)$name == node.name) %>% which)
    
    # If the parent.graph is empthy, return the na.value
    return(na.value) %if% (length(V(parent.graph)) == 0)
    
    # Get the parent vertex:
    return(get.vertex.attribute(parent.graph, attr, 1))
    
}
 
assign_dataset_ids <- function(dt.graph,dt.paths){
    
    V(dt.graph)$visited <- FALSE 
    V(dt.graph)$data.id.current <- 0
    
    for(path in dt.paths){
        
        # Walk through each node of each path
        for(node.name in V(path)$name){
            
            # Get the id of the current node:
            node.id <- (V(dt.graph)$name == node.name) %>% which 
  
            # Get the name of the parent node:
            node.name.parent  <- get.parent.node(node.name, path,attr = "name", na.value = node.name)
            
            if(V(dt.graph)[node.id]$visited %>% not){
                if(node.name == node.name.parent){ 
                    V(dt.graph)[node.id]$data.id.current <- 1 
                } else { 
                    if(get.outdegree(node.name.parent,g = dt.graph) == 1 & V(dt.graph)[node.id]$save.data == 0){
                        V(dt.graph)[node.id]$data.id.current <- get.parent.node(node.name,dt.graph,attr = "data.id.current")
                    } else {
                        V(dt.graph)[node.id]$data.id.current <- (V(dt.graph)$data.id.current %>% max) + 1 
                    }
                }
            } 
            
            # Get the out-degree of the current node in the full graph:
            outdegree.current <- get.outdegree(node.name, dt.graph)
            
            # Get the out-degree of the parent node in the full graph:
            outdegree.parent  <- get.outdegree(node.name.parent, dt.graph)
             
            
            V(dt.graph)[[node.id]]$visited <- TRUE
            
            dt.graph <- set_vertex_attr(dt.graph,name = "visited", index = node.id, value = TRUE)
             
        } 
    } 
     
    return(dt.graph)
     
}




plotRequiredDataTransformations <- function(dt.graph, features = "all", 
                                            vertex.label.cex   = .7,
                                            edge.arrow.size    = .7,
                                            layout             = layout_as_tree(dt.graph,flip.y = T),
                                            vertex.color       = vertex.attributes(dt.graph)$vertex.color,
                                            vertex.shape       = vertex.attributes(dt.graph)$vertex.shape,
                                            vertex.size        = vertex.attributes(dt.graph)$vertex.size,
                                            vertex.size2       = vertex.attributes(dt.graph)$vertex.size2,
                                            vertex.label       = vertex.attributes(dt.graph)$vertex.label, ...){
      
    if(features == "all"){
     
        plot.igraph(dt.graph, 
                    layout            = layout,
                    edge.arrow.size   = edge.arrow.size, 
                    vertex.label.cex  = vertex.label.cex,
                    vertex.color      = vertex.color,
                    vertex.shape      = vertex.shape,
                    vertex.size       = vertex.size,
                    vertex.size2      = vertex.size2,
                    vertex.label      = vertex.label)   

    }  
}

`%select.regex%` <- function(x,y){
    
    grep(y,x, value=T)
    
}

collapse_vertex_attributes <- function(graph){
    
    tmp.graph <- newu
    
    # Get vertex names
    tmp.graph %>% vertex.data
     
    
    # Get attributes with suffix 
    tmp.attr.names <- {tmp.graph %>% vertex.attributes %>% names} %select.regex% "_"
    
    tmp.graph
    
}




