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

#' Calculate test statistics for predictive accuracy of a feature.
#'
#' @param dtable A data.table object with dependents and features. 
#' @param dependents A vector denoting the names of the dependent variable(s). 
#' @param feature A vector denoting the name of the feature colomn.
#' @param controls A character vector denoting the name of the control variables; if specified, tests statistics provide information on the incremental relevance of a feature given the controls.
#' @return a modified data.table object with strings instead of factors. 
#' @examples
#' \dontrun{
#'  	x.fixed <- feature_to_t_values(x)
#' \dontrun{
feature_to_t_values<-function(dtable, 
							  dependents, 
							  feature,
							  controls = NULL,
							  family = binomial(link = "logit")){ 
 
	# Maak een lege matrix om de uitkomstwaarden op te slaan	 
    outcomes <- rep(NA,length(dependents) * nrow(dtable)) %>% c %>% matrix(ncol=length(dependents))
	 
    # -------------------------------------------------------------------------------------------------------- # 
	# Fit voor elke afhankelijke variabele een model van de vorm:                                              #
	#                                                                                                          #
	# 		f(Dependent) = b0 + b1 * Feature + {b2 ..... bn} * Controls                                        #
	#	                                                                                                       #
	#       waarbij:                                                                                           #
    #           -	f een arbitraire link functie is (i.e., logit, probit, etc.)                               #
	#           -   b1 de coefficient is waarin we geinterresseerd zijn                                        #
	#                                                                                                          #
	# -------------------------------------------------------------------------------------------------------- #
	
    for(i in 1:length(dependents)){  
	
		if(is.null(controls)){
	
			formula_string  <-  dependents[i]  %+% " ~ " %+% feature  
		
		} else {
			
			formula_string  <-  dependents[i]  %+% " ~ " %+% feature %+% "+" %+% paste0(controls, collapse = " + ") 	
		
		} 
		
		formula <- formula_string %>% as.formula
		
		tryCatch({
            model.summary <- glm(formula, data=dtable, family=family) %>% suppressWarnings %>%  summary 
            outcomes[,i]  <- model.summary$coefficients[feature, 3]     
		}, error = function(x){
		     outcomes[,i] <<- 0
		})      
	} 
    return(outcomes %>% data.table) 
} 
 