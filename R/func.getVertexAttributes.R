getVertexAttributes <- function(dt.graph, vertex.name){
    attributes <- {dt.graph %>% vertex.attributes} %>% lapply(function(x){
        return(x[which(V(dt.graph)$name == vertex.name)])  
    })
    return(attributes)
}