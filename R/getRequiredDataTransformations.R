



 
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
  


f2f.rank.proper <- function(x){ 
    
    {list(V1 = x) %>% as.data.table}[, rank := .GRP, by = V1][, c(rank)] %>% return
    
} 


 


 
 
assign_parent_dataset_ids <- function(dt.graph){
  
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
    d.in  <- degree(path,mode = "in")
    
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




