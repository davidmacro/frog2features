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
            
            Dictionaries[[path.name]] <- list(
                dictionaryID = dictionaryID, 
                dataID = dataID
            )  
        } else { 
		
            if(any(V(paths[[path.name]])$type %in% c("construct.features"))){
                
                fc.create.node <- V(paths[[path.name]])[V(paths[[path.name]])$type %in% c("construct.features") %>% which][1]  
                
                dt <- get.parent.nodes(node.name = fc.create.node$name, graph = graph, attr = "data.id.current")
                
                dataID <- dt[, (data.id.current) %>% max]
                
                Dictionaries[[path.name]] <- list(
                    dictionaryID = dictionaryID, 
                    dataID = dataID
                ) 
            } 
        }  
    }
    return(Dictionaries)
}
