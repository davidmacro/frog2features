# ---------------------------------------------------------------------------------------#
#                                                                                        #
#  Features :: 4 :: Meta-semantische kenmerken                                           #
#                                                                                        #
# ---------------------------------------------------------------------------------------#                                                                                    
#                                                                                        #
#  Auteur: D.A. Macro                                                                    #
#           d.a.macro@minvenj.nl // davidmacro@gmail.com                                 #
#                                                                                        #
# ---------------------------------------------------------------------------------------#
 
 

f_meta_semantics_dutch <- function(dtfrog, prefix = "f_meta_semantics_" ){
  
    data(semantic.properties.dutch.lemma)
    
    prop             <- semantic.properties.dutch.lemma
    prop.unambiguous <- prop[, nlemma := .N, by = c("lemma", "majorpos")][nlemma == 1,]
    
    dtfrog[, sentence_length := .N, by = c("docid", "sent")]
     
    dtfrog[, p_N   := sum(majorpos == "N")   / sentence_length,  by = c("docid", "sent")] 
    dtfrog[, p_WW  := sum(majorpos == "WW")  / sentence_length,  by = c("docid", "sent")] 
    
    dtfrog.merged <- merge(
        x = dtfrog[, list(docid, sent,position, lemma,majorpos, sentence_length,p_N,p_WW)],
        y = prop.unambiguous[, list(lemma, majorpos, 
                        outdegree,
                        neighb.density,
                        neighb.transitivity, 
                        neighb.diameter,
                        neighb.average.indegree)] %>% unique,
        all.x = FALSE,
        all.y = FALSE,
        by = c("lemma", "majorpos")
    )[order(docid, sent,position)]
    
    dtfrog.merged[neighb.density      %>% is.na, neighb.density := 0]
    dtfrog.merged[neighb.transitivity %>% is.na, neighb.transitivity := 0]
  
    dtfrog.merged[, by = c("docid", "sent")]
      
    cat("\t\t\t Calculate sentence-level averages: |")
    dtfrog.merged[sentence_length %between% c(0,10),   `:=`(mean_ambig_ww_1    = mean(.SD[majorpos == "WW", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]
    cat("*")
    
    dtfrog.merged[sentence_length %between% c(11,20),  `:=`(mean_ambig_ww_2    = mean(.SD[majorpos == "WW", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]    
    cat("*")
    
    dtfrog.merged[sentence_length %between% c(21,Inf), `:=`(mean_ambig_ww_3    = mean(.SD[majorpos == "WW", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]    
    cat("*")
    
    dtfrog.merged[,                                    `:=`(mean_ambig_ww_all  = mean(.SD[majorpos == "WW", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]    
    cat("*")
    
    dtfrog.merged[sentence_length %between% c(0,10),   `:=`(mean_ambig_n_1     = mean(.SD[majorpos == "N", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]
    cat("*")
    
    dtfrog.merged[sentence_length %between% c(11,20),  `:=`(mean_ambig_n_2     = mean(.SD[majorpos == "N", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]    
    cat("*")
    
    dtfrog.merged[sentence_length %between% c(21,Inf), `:=`(mean_ambig_n_3     = mean(.SD[majorpos == "N", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]    
    cat("*")
    
    dtfrog.merged[,                                    `:=`(mean_ambig_n_all   = mean(.SD[majorpos == "N", neighb.density], na.rm = T)), by = c("docid", "sent"), .SDcols = c("majorpos","neighb.density")]    
    cat("*|\r\n")
    
    cat("\t\t\t Calculate document-level averages: |")
    
    dtfrog.merged[, doc_ambig_ww_1  := mean(mean_ambig_ww_1, na.rm=T), by = c("docid")];cat(" . ")
    dtfrog.merged[, doc_ambig_ww_2  := mean(mean_ambig_ww_2, na.rm=T), by = c("docid")];cat(" . ")
    dtfrog.merged[, doc_ambig_ww_3  := mean(mean_ambig_ww_3, na.rm=T), by = c("docid")];cat(" . ")
    
    dtfrog.merged[, doc_ambig_n_1   := mean(mean_ambig_n_1, na.rm=T), by = c("docid")];cat(" . ")
    dtfrog.merged[, doc_ambig_n_2   := mean(mean_ambig_n_2, na.rm=T), by = c("docid")];cat(" . ")
    dtfrog.merged[, doc_ambig_n_3   := mean(mean_ambig_n_3, na.rm=T), by = c("docid")];cat(" . ")
    
    dtfrog.merged[, doc_p_ww  := mean(p_WW,  na.rm=T), by = c("docid")];cat(" . ")
    dtfrog.merged[, doc_p_n   := mean(p_N,   na.rm=T), by = c("docid")];cat(" . |")
    
    dtfrog.resulting <- dtfrog.merged[, .(docid, doc_p_ww,
                                                 doc_p_n,
                                                 doc_ambig_ww_1,
                                                 doc_ambig_ww_2,
                                                 doc_ambig_ww_3,
                                                 doc_ambig_n_1,
                                                 doc_ambig_n_2,
                                                 doc_ambig_n_3)] %>% unique
     
    
    dtfrog.resulting[doc_ambig_ww_1 %>% is.na,  doc_ambig_ww_1 := 0]
    dtfrog.resulting[doc_ambig_ww_2 %>% is.na,  doc_ambig_ww_2 := 0]
    dtfrog.resulting[doc_ambig_ww_3 %>% is.na,  doc_ambig_ww_3 := 0]
     
    dtfrog.resulting[doc_ambig_n_1 %>% is.na,  doc_ambig_n_1 := 0]
    dtfrog.resulting[doc_ambig_n_2 %>% is.na,  doc_ambig_n_2 := 0]
    dtfrog.resulting[doc_ambig_n_3 %>% is.na,  doc_ambig_n_3 := 0] 
 
    
    cols.old    <- c("doc_p_ww", "doc_p_n","doc_ambig_ww_1" ,"doc_ambig_ww_2","doc_ambig_ww_3" ,"doc_ambig_n_1"  ,"doc_ambig_n_2"  ,"doc_ambig_n_3")
    cols.prefix <- prefix %+% cols.old
 
    colnames(dtfrog.resulting)[colnames(dtfrog.resulting) %in% cols.old] <- prefix %+% cols.old
  
    
    dtfrog.resulting %>% return
                
}

 
 

