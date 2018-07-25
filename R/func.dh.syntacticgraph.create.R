# --------------------------------------------------------------------------------
#      File: func.CreateSyntacticGraph.R
# --------------------------------------------------------------------------------
#    author: D.A. Macro (d.a.macro@minvenj.nl)
#      date: 2018-07-05 
#  
# --------------------------------------------------------------------------------
#   Purpose: Create a syntactic graph of a sentence of a sentence of a frog
#            frog object. 
#
# --------------------------------------------------------------------------------
create_syntactic_graphs <- function(dtfrog, docid, sent = NULL, return.type = "igraph", use.strict = T, save.plots = F, plot.name.prefix = "", verbose=T){
    
    # Extract the sentence 
    docid.match <- docid
    
    sent.match  <- sent 
    
    # Filter the FROG data.table
    dtfrog.selected <- dtfrog[docid %in% docid.match,]
    
    # Split the selected data.table by docid and sent
    dtfrog.split <- dtfrog.selected %>% split(by =  c("docid", "sent"))
    
    if(verbose){
        cat("Creating syntactic graphs of",
            dtfrog.selected[, group := .GRP , by = c("docid", "sent")][, group %>% max],
            "sentences in ", 
            dtfrog.selected[, docid %>% uniqueN], "documents.\r\n")
    }
      
    result <- dtfrog.split %>% lapply(function(frog.split){
        
        docid.to.use <- frog.split[1, docid]
        sent.to.use  <- frog.split[1, sent]
        
        create_syntactic_graph(frog.split,
            docid       = docid.to.use,
            sent        = sent.to.use,
            return.type = return.type,
            use.strict  = use.strict
        ) %>% return
    })
    
}
  
plot_syntactic_graph <- function(x, save.plots = F, plot.name.prefix = "", verbose=T, title = NULL, subtitle = NULL){
    
    if(!"igraph.syntactic" %in% class(x)){
        stop("The graph supplied is not a syntactic graph.")
    } 
    
    s.graph <- x
  
    if(is.null(title)){
        if(is.null(attr(s.graph, "title"))){
            title <- "Syntactic dependency graph"
        } else {
            title <- attr(s.graph, "title")
        }
    }
    
    if(is.null(subtitle)){
        if(is.null(attr(s.graph, "subtitle"))){
            subtitle <- "subtitle"
        } else {
            subtitle <- attr(s.graph, "subtitle")
        }
    }

    
    retval <- plot(s.graph, layout = layout_(s.graph, as_tree()),
                   vertex.shape       = "none" ,
                   vertex.label.cex   = 1.2,
                   vertex.label.color = "darkblue",
                   edge.arrow.size    = 0.2,   
                   vertex.label       = V(s.graph)$node.word,
                   edge.label         = E(s.graph)$reltype, 
                   edge.label.cex     = 0.8,
                   edge.label.color   = "darkred")
    
    title(main = title)
    title(sub =  subtitle) 
    
    
}

extract_frog_subset <- function(dtfrog, docid = NULL, sent = NULL){
    
    if(is.null(docid) & is.null(sent)){
        
        cat("Warning: not extracting anything")
        return(dtfrog)
        
    }
    
    # If docid is not set, assume that we want all docids
    if(is.null(docid)){
      
         # At this point, sent must be not null 
        if(sent == "all"){
            
            # Special case: we want all docids and all sentences; this is just the original dtfrog
            return(dtfrog)
            
        } else { 
            
            # Special case: we want all first sentences from all docids
            if("first" %in% sent){
                sent    
            } 
            
            # Special case: we want all first sentences from all docids
            if("last" %in% sent){
                
            }
        }
       
        
    }
    
    
}
    
extract_sentence_from_frog <- function(dtfrog, docid = NULL, sent = NULL){
    
    if(is.null(docid) & is.null(sent)){
        return(dtfrog)
    }
    
    docid.match <- c(1,2,3)
    sent.match  <- 1
    
    match.dt <- CJ(docid  = docid.match,
                   sent   = sent.match)
    
    dtfrog[match.dt, on = c("docid", "sent")]
    
    
    
    dtfrog[docid == docid.match & sent == sent.match, sent = paste0(word, collapse=" "), by = c("docid", "sent")]
    
    
}    
    

create_syntactic_graph <- function(dtfrog, docid, sent, return.type = "data.table", use.strict = T, save.plots = F, plot.name.prefix = "", verbose=T){
 
    dtfrog <- {
        dtfrog %>% data.table
    }[, .(docid, word, lemma, sent, position, pos, majorpos, parse1, parse2,  morph)] %>% FixStringsFactors
   
    # Extract the sentence 
    docid.match <- docid
    sent.match  <- sent
    
    if(dtfrog[docid == docid.match & sent == sent.match,.N] == 0){
        if(!use.strict){
            return(NULL)
        } else {
            stop("Sentence does not exist.")
        }
    }
    
	vertices.data <- dtfrog[docid == docid.match & sent == sent.match,]  
	  
	# Extract information for the "from" part
	left.part <- vertices.data[, .( 
	    node          = position, 
        node.parent   = parse1,
        reltype       = parse2,
        node.pos      = pos, 
        node.morph    = morph, 
        node.majorpos = majorpos,
        node.word     = word
    )]      
	
	# Extract information for the "to" part
	right.part <- vertices.data[position %in% left.part$node.parent, .(
	    node              = position, 
        parent.majorpos   = majorpos,
		parent.word       = word)
    ]    
			
	merged <- merge(
	    x     = left.part, 
		y     = right.part , 
		by.x  = "node.parent",
		by.y  = "node", 
		allow.carthesian = T,
		all.x = T,
		all.y = F
    )[order(node)]
		 
	cols.first <- c("node", "node.parent", "reltype", "node.word", "parent.word")
	cols.order <- c(cols.first, c(colnames(merged) %-% cols.first) %>% sort)
	
	retval <- FALSE
	
	if(return.type == "data.table"){
		retval <- merged
	}   
	
	if(return.type %in% c("igraph", "igraph.plot")){
	
		# Stel de loops goed in
		merged[node.parent == 0, node.parent:=node] 
	
		s.graph <- merged[, .(node.parent,node, reltype)] %>% 
            graph.data.frame(vertices = merged[, .(node,node.word)]) %>% 
            simplify(remove.multiple = F, remove.loops = T)
		
		
		class(s.graph) <- c("igraph", "igraph.syntactic")
		
		attr(s.graph, "title")    <- "Syntactic dependency graph"
		attr(s.graph, "docid")    <- docid.match
	    attr(s.graph, "sent")     <- sent.match
	    attr(s.graph, "sentence") <- dtfrog[docid == docid.match & sent == sent.match, paste0(word, collapse=" ")]
		attr(s.graph, "subtitle") <- "New subtitle3"
		
		if(return.type == "igraph.plot"){ 
		    plot_syntactic_graph(s.graph, 
                save.plots       = save.plots, 
                plot.name.prefix = plot.name.prefix, 
                verbose          = verbose) 
		}  else {  
			return(s.graph)  
		}	 
	
		retval <- merged[, .(node, node.parent)]
	}   
			  
	return(merged) 
		 
} 