get.neighborhood.aggregate <- function(g         = NULL, 
                                       depth     = 5, 
                                       mode      = "out", 
                                       ego.graphs = NULL,
                                       attribute.name = "indegree",
                                       fun.aggregate = "mean"){
 
    if(ego.graphs %>% is.null){
        ego.graphs <- make_ego_graph(g, depth, mode = mode)    
    }
    
    sapply(ego.graphs, function(x){  
        get(fun.aggregate)(c(vertex_attr(x, attribute.name)))
    } ) %>% return  
      
}

get.neighborhood.diameter <- function(g = NULL, 
                                      depth = 5, 
                                      mode = "out", 
                                      ego.graphs = NULL){
  
    if(ego.graphs %>% is.null){
        ego.graphs <- make_ego_graph(g, depth, mode = mode)    
    }
    
    sapply(ego.graphs, function(x){ 
        diameter(graph = x, unconnected = F, directed = T)   
    } ) %>% return 
}


get.neighborhood.density <- function(g = NULL, depth = 5, mode = "out", ego.graphs = NULL ){
  
    if(ego.graphs %>% is.null){
        ego.graphs <- make_ego_graph(g, depth, mode = mode)    
    }
    sapply(ego.graphs, function(x){ 
        graph.density(graph = x)   
    } ) %>% return 
}
 
get.neighborhood.transitivity <- function(g, depth = 5, mode = "out",ego.graphs = NULL ){
  
    if(ego.graphs %>% is.null){
        ego.graphs <- make_ego_graph(g, depth, mode = mode)    
    }
    
    sapply(ego.graphs, function(x){ 
        transitivity(graph = x, type="global", isolates = "zero")   
    } ) %>% return 
}