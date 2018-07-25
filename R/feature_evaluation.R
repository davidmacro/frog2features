# --------------------------------------------------------------------------------------#
#                                                                                       #
#  Features :: feature_evaluation.R                                                     #
#                                                                                       #
#  -------------------------------------------------------------------------------------#                                                                               
#                                                                                       #
#  Auteur: D.A. Macro                                                                   #
#           d.a.macro@minvenj.nl // davidmacro@gmail.com                                #
#  -------------------------------------------------------------------------------------# 
 
get_default_selection_options <- function(type){

    opts <- list()

    if(type == "incidence"){
        opts <- list(
            ll = 0.02,
            ul = 0.90
        )
    }

    if(type == "filter"){
        opts <- list(
            controls   = NULL,
            dependents = c("Y"),
            family     = binomial(link="logit"),
            criterium  = "t"
        )
    }

    if(type == "wrapper"){
        opts <- list(
             
        )
    }

    return(opts)
    
}

    
feature_selection_object <- function(selection.type, selection.options = get_default_selection_options(type)){
     

} 

features_to_variances <- function(dtable, 
                                  dictionary,
                                  features = NULL,  
                                  prefix   = NULL){
     
    dtable.long <-  melt(dtable, id.vars = c("docid"), measure.vars =  features) 
        
    dtable.long[, var_value := var(value), by = variable]
  
    dtable.short <- {dtable.long[, list(name = variable, var_value)] %>% unique}[(order(var_value, decreasing = T))]
    
    merge(dtable.short, dictionary, all.x = T, all.y = F, by = "name")
    
}
     
     
    
features_to_z_values <- function(dtable, labels,  features, family = binomial(link = "logit")){
 		  
     result <- lapply(features, function(feature){
          
         result.feature <- lapply(labels, function(lab){
          
             z.value <- 0
            
             tryCatch({
				z.value <- {
					glm(dtable[[lab]] ~ dtable[[feature]], family = family) %>%  
						suppressWarnings %>% summary
				}$coefficients[2,3]  
             })  
            
             z.value
         })  %>% simplify2array
         
         result.feature  
         
     })
       
     retval <- result  %>% simplify2array %>% t
     
     colnames(retval) <- labels 
  
     retval <- cbind(features %>% data.table(name = .), retval %>% data.table)
}  
     
    
feature_to_t_values2 <-function(dtable, 
                              label,    
                              controls   = NULL, 
                              family     = binomial(link = "logit")){ 
  

     
    # -------------------------------------------------------------------------------------------------------- #
    # Fit voor elke afhankelijke variabele een model van de vorm:                                              #
    #                                                                                                          #
    #         f(Dependent) = b0 + b1 * Feature + {b2 ..... bn} * Controls                                      #
    #                                                                                                          #
    #       waarbij:                                                                                           #
    #           -    f een arbitraire link functie is (i.e., logit, probit, etc.)                              #
    #           -   b1 de coefficient is waarin we geinterresseerd zijn                                        #
    #                                                                                                          #
    # -------------------------------------------------------------------------------------------------------- #
 
      ret <- 0
    tryCatch({
        ret <-  {glm(dtable[, get(labels[i])] ~ dtable$value, family = family) %>% 
                        suppressWarnings %>% summary}$coefficients[2,3] 
    }, error = function(x){
          ret <- 0
        })
    return(ret) 
} 
 



features_to_t_values <- function(dtable,
                                 dictionary,
                                 prefix = "f1a_",
                                 labels,
                                 features,  
                                 family = binomial(link = "logit")){
		 
    dtable.long <-  melt(dtable, id.vars = c("docid", labels), 
                                  measure.vars = features) %>% unique %>% FixStringsFactors
      
	t.col.names <- "t_" %+% labels
		 
	dtable.long[ , (t.col.names) := feature_to_t_values(dtable = .SD , labels = labels),  by = c("variable")] 
  	
	dtable.long <- merge(dtable.long, dictionary, all.x = T, all.y = F, by.x = "variable", by.y = "name")
	
	dtable.long[, (labels) := NULL] 
	dtable.long[, docid := NULL] 
	dtable.long[, value := NULL] 
	dtable.long <- unique(dtable.long)
	
	dtable.long[, .(variable, t.col.names) ]
	
}  

#' Calculate test statistics for predictive accuracy of a feature.
#'
#' @param dtable A data.table object with dependents and features. 
#' @param dependents A vector denoting the names of the dependent variable(s). 
#' @param feature A vector denoting the name of the feature colomn.
#' @param controls A character vector denoting the name of the control variables; if specified, tests statistics provide information on the incremental relevance of a feature given the controls.
#' @return a modified data.table object with strings instead of factors. 
#' @examples
#' \dontrun{
#'      x.fixed <- feature_to_t_values(x)
#' \dontrun{
feature_to_t_values<-function(dtable, 
                              labels,    
                              controls   = NULL, 
                              family     = binomial(link = "logit")){ 
  
    # Maak een lege matrix om de uitkomstwaarden op te slaan     
    t.labels <- rep(NA,length(labels)) %>%  matrix(ncol=length(labels))
     
    # -------------------------------------------------------------------------------------------------------- #
    # Fit voor elke afhankelijke variabele een model van de vorm:                                              #
    #                                                                                                          #
    #         f(Dependent) = b0 + b1 * Feature + {b2 ..... bn} * Controls                                      #
    #                                                                                                          #
    #       waarbij:                                                                                           #
    #           -    f een arbitraire link functie is (i.e., logit, probit, etc.)                              #
    #           -   b1 de coefficient is waarin we geinterresseerd zijn                                        #
    #                                                                                                          #
    # -------------------------------------------------------------------------------------------------------- #
 
    
    for(i in 1:length(labels)){  
         tryCatch({
            model.summary <- speedglm(dtable[, get(labels[i])] ~ dtable$value, family = family) %>% suppressWarnings %>%  summary 
            t.labels[,i]  <- model.summary$coefficients[2, 3]     
        }, error = function(x){
             t.labels[,i] < - 0
        })      
    } 
    return(t.labels %>% data.table ) 
} 
     
