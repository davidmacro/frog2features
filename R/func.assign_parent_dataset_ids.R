assign_parent_dataset_ids <- function(dt.graph){
   
    V(dt.graph)$data.id.previous <- NA
    
    for(node.name in V(dt.graph)$name){ 
        node.id <- (V(dt.graph)$name == node.name) %>% which 
        V(dt.graph)[node.id]$data.id.previous <- get.parent.node(node.name, dt.graph, "data.id.current", 1) 
    } 
    return(dt.graph)
}
