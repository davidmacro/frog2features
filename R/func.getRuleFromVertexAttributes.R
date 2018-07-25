getRuleFromVertexAttributes <- function(dt.graph, vertex.name){
    
    rule.from.attributes <- dt.graph %>% getVertexAttributes(vertex.name=vertex.name)
    tmp.rule <- getNaRuleList() %>% modifyList(rule.from.attributes,keep.null = T) 
    return(tmp.rule)
}