
UpdateVertexAttributes <- function(dt.graph, vertex.name, new.rule, verbose=T){
    
    cat("Updating attributes for: ", vertex.name, "\r") %if% (verbose)
    
    for(name in names(new.rule)){
        
        cat("Name: ",  name,             "\r\n") %if% (verbose)
        cat("Value: ", new.rule[[name]], "\r\n") %if% (verbose)
        
        dt.graph %<>% set_vertex_attr(
            name  = name, 
            index = which(V(dt.graph)$name == vertex.name),
            value = new.rule[[name]]
        )
    }
    
    return(dt.graph)
}
