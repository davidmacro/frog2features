#' Permute ngrams based on part-of-speech type (majorpos)
#'
#' @param dtfrog 
#' Frogged dataset; must contain the given token columns.
#' 
#' @param tokens 
#' Character vector of source tokens of the ngram. 
#' Typical usage: \cr \code{tokens = c("lemma_1","lemma_2", ...)}
#'          
#' @param summary
#' Defaults to \code{FALSE}; if \code{TRUE}, the action is not performed, but instead only a summary of the intended action is printed.
#'                         
#' @param majorpos_sort_order 
#' Character vector of the desired majorpos tags to select on in order.
#' 
#' @param combinations_to_merge 
#' Either \code{"all"} or a list of vectors of majorpos combinations that 
#' need to be merged together. 
#' 
#' @param combinations_to_exclude
#' Either \code{"none"} or a list of vectors of majorpos combinations that
#' need not to be included in the merge. Often this is useful in combination
#' with \code{combinations_to_merge = "all"}. 
#' 
#' @param non.matched
#' What to do with non-matched ngrams (i.e., ngrams that need no permutation). 
#' The default behavior is to keep them (\code{non.matched = "keep"}). 
#' This allows for multiple ngram permutations to be chained after one another.
#' 
#' @param ties 
#' Not yet implemented; how to handle ties
#' 
#' @param colname_ngram 
#' Name of the new column containing the original ngram.  
#' 
#' @param colname_ngram_permuted 
#' Name of the new column containing the permuted ngram.  
#' Defaults to: \cr \code{colname_ngram_permuted = "ngram_permuted"}. \cr 
#' In-place permutations are allowed via \code{colname_ngram_permuted = "ngram"}
#'                               
#' @usage permute_ngrams_on_majorpos(dtfrog, 
#'    tokens = c("lemma_1", "lemma_2", "lemma_3"), 
#'    majorpos_sort_order     = c("N","WW", "BW"),
#'    combinations_to_merge   = "all",
#'    combinations_to_exclude = "none",
#'    ties = "alphabetical",
#'    colname_ngram_permuted = "ngram_permuted"
#' )
#' @return An altered frogged dataset with the permuted ngram column.
#'
#' @details 
#'  A problem in the construction of \emph{ngrams} is that the same combination of 
#'  tokens can occur in multiple permutations. 
#'  Often, we would wish to count these
#'  permutations as representations of the \emph{same} underlying ngram. 
#'  The \code{permute_ngrams_on_majorpos} function
#'  provides a solution to the special case in which (i) all parts of the ngram are 
#'  of different part-of-speech types, and (ii) a preferred order is given by the end-user. 
#'  
#'  \strong{Examples of the problem} \cr
#'  In the examples, a more detailed description of the problem is provided. In Example 1, 
#'  a Dutch dataset on legal texts is transformed into trigrams to show how pervasive the 
#'  permutation is in real-world corpora. In Example 2, two typical Dutch sentences are 
#'  analyzed to show how the permutation issue arises from Dutch word order 
#'  practices. 
#'  
#'  \strong{A solution for ngrams with unique majorpos components} \cr
#'  The \code{permute_ngrams_on_majorpos()} function solves the permutation issue for ngram with unique majorpos components by
#'  executing the following steps: 
#'  
#'  \enumerate{
#'    \item{Calculate all possible orderings of the major_pos_types \emph{or} by 
#'     start from the list of combinations provided by the user via the \code{combinations_to_merge} option.}
#'    \item{Filter out any unwanted combinations as specified via the \code{combinations_to_exclude} option.}
#'    \item{Create a table that maps all old positions in the ngram to their (permuted) new position; so in a bigram, there can at most be two mappings; in a trigram, there can at most be six, etc.}
#'    \item{Execute the mappings, vectorizing over each mapping type for speed.}
#'  }
#'    
#'    
#' @examples 
#' \dontrun{
#' 
#' # Example 1: Showcase of the permutation issue in a large legal corpus. 
#' # ----------------------------------------------------------------------- 
#' 
#' # This example showcases the ngram permutation issue in an example
#' # based on the Dutch penal code. For educational purposes, this 
#' # dataset is included in the Frog2Features-package. 
#'   
#' # 1.1. Load the example dataset
#' data(wb.sr.frogged) 
#'  
#' # 1.2. Construct lemma trigrams based on (positional) proximity
#' trigrams <- wb.sr.frogged %>% 
#'                apply_rule(default_rule_ngram("lemma.trigram.adjacency"))
#' 
#' # 1.3. Group the trigrams by part-of-speech type
#' trigrams[, .N, by = c('majorpos_1', 
#'                       'majorpos_2',
#'                       'majorpos_3')][order(-N)][1:4] 
#'                        
#' 
#' # The above should give the following output: 
#'                                                                         
#' # ---------------------------------------------------------------------------   
#' #           majorpos_1 majorpos_2 majorpos_3    N 
#' #       1:         VZ        LID          N     2970 
#' #       2:          N         VZ        LID     2677 
#' #       3:        LID          N         VZ     2174 
#' #       4:         VZ          N         VZ     1276  
#' # ---------------------------------------------------------------------------  
#'  
#' # Interpretation                                                               
#' 
#' # The output shows that the four most frequent lemma-trigrams in this data
#' # are in fact combinations of the same three major part-of-speech types. 
#' 
#' # In practice, one would often want to count these ngrams as the same token. 
#' 
#' # Now, consider an example on this very same dataset where this issue arises:     
#'       
#' # 1.4. Filter the trigrams such that their components only contain
#' #      VZ, LID, and N:  
#' 
#' trigrams.filtered <- {
#'    trigrams %>% 
#'        apply_rule(default_rule_filter("exclude.if.majorpos", 
#'           token       = "majorpos_1", 
#'           filter.what = "outside", 
#'           items       = c("VZ","LID","N"))) %>%
#'        apply_rule(default_rule_filter("exclude.if.majorpos", 
#'           token       = "majorpos_2", 
#'           filter.what = "outside", 
#'           items       = c("VZ","LID","N"))) %>%        
#'        apply_rule(default_rule_filter("exclude.if.majorpos", 
#'           token       = "majorpos_3", 
#'           filter.what = "outside", 
#'           items       = c("VZ","LID","N")))   
#' }   
#'    
#' # 1.5. Now zoom in on a specific trigram that often occurs in a legal
#' #      corpus, namely "the execution of (a punishment)". This trigram
#' #      in Dutch is written as "de tenuitvoerlegging van", but this
#' #      trigram can, dependent on context, have multiple permutations.
#' 
#' #      Consider the following example: 
#' 
#' trigrams.filtered[
#'    lemma_1 %in% c("de", "tenuitvoerlegging", "van") &
#'    lemma_2 %in% c("de", "tenuitvoerlegging", "van") & 
#'    lemma_3 %in% c("de", "tenuitvoerlegging", "van"), 
#'    .N, 
#'     by = c("ngram")][order(-N)][1:2]  
#'   
#' # This gives the following output: 
#' 
#'     ngram                        N
#'   1: de - tenuitvoerlegging - van 61
#'   2: tenuitvoerlegging - van - de 55  
#'   
#' # The above ngrams are similar, but depending on context, we may want to 
#' # consider merging these together. 
#'  
#' # Solving the permutation problem for the generic case is difficult, since
#' # the the number of combinations to permute can quickly grow exponentially. 
#'   
#' # The permute_ngrams_on_majorpos() function solves this issue for a specific
#' # case in which:  
#' #    - the parts of the ngram are all of different (major) part of speech codes
#' #    - the user has an a priori preferred ordering.
#' #    - all ngrams take the same token as their basis  
#'     
#' # Example 2: Showcase of the permutation issue in two dutch sentences  
#' # ---------------------------------------------------------------------------  
#' 
#' #   This example shows the permutation issue in two simple Dutch sentences: 
#' 
#' #   i.  De dief vluchtte tevergeefs nadat hij zijn slag geslagen had.
#' #   ii. Wist jij dat de dief tevergeefs vluchtte, nadat hij zijn slag geslagen had?
#' 
#' # Suppose that we want to extract the information on the thief, namely: 
#' #    "dief", "vluchtte", and "tevergeefs".  
#' 
#' # 2.1. Loads the example data        
#' data(vignette.permutation.majorpos)
#' 
#' # 2.2. Create trigram data
#' trigrams <-  vignette.permutation.majorpos  %>% 
#'     apply_rule(default_rule_ngram("lemma.trigram.adjacency"))  
#'  
#' # 2.3. Verify that this example indeed has the two ngrams:     
#' trigrams[lemma_1 %in% c("dief","vluchten","tevergeefs")  &      
#'          lemma_2 %in% c("dief","vluchten","tevergeefs")  &
#'          lemma_3 %in% c("dief","vluchten","tevergeefs"), 
#'          .(docid, sent, .N, ngram, majorpos_1, majorpos_2, majorpos_3), ]
#'       
#' # 2.4. Now use the permute_ngrams_on_majorpos() function: 
#' trigrams.permuted <- trigrams %>% 
#'    permute_ngrams_on_majorpos(majorpos_sort_order = c("N", "WW", "ADJ"))
#'    
#' 
#' # 2.5. Verify that in the new FROG dataset, ngrams are permuted:
#' trigrams.permuted[, .(docid,sent,position,ngram,ngram_permuted)]
#' 
#' 
#' }
#' @export
permute_ngrams_on_majorpos <- function(dtfrog,  
    tokens, 
    summary                 = FALSE,
    majorpos_sort_order,
    combinations_to_merge   = "all",
    combinations_to_exclude = "none",
    ties                    = "alphabetical",
    colname_ngram           = "ngram",
    colname_ngram_permuted  = "ngram_permuted",
    non.matched             = "keep",
    verbose                 = TRUE ){ 
     
    dtfrog <- dtfrog %>% data.table %>% FixStringsFactors
     
    tag_combination <- function(dt, cols, combination, tagname, type = "all", reset=T){
        
        stopifnot(length(cols) == length(combination))
        
        stopifnot(type %in% c("all", "any"))
        
        if(reset){
            dt[, c(tagname) := FALSE ]    
        }
        
        if(type == "all"){
            selector <- {
                cols %+% "==" %+% "\"" %+% combination %+% "\""
            } %>% paste0(collapse = " & ") %>% parse(text= .)     
        } 
        
        if(type == "any"){
            selector <- {
                cols %+% "==" %+% "\"" %+% combination %+% "\""
            } %>% paste0(collapse = " | ") %>% parse(text= .)     
        }  
        
        dt[eval(selector), c(tagname) := TRUE ]
        
        return(dt)
    }
    
    # Columns for convenience
    majorpos_cols  <- "majorpos_"  %+% 1:length(tokens) 
    ngram_cols     <- "ngram_"     %+% 1:length(tokens)
    token_tmp_cols <- "token_tmp_" %+% 1:length(tokens)         
     
    if(is.list(combinations_to_merge)){
        
        # Start with all combinations provided by the user
        all.combinations <- {
            combinations_to_merge %>lapply% 
            data.table  %>lapply% 
            transpose %>% 
            rbindlist    
        } 
        
        names(all.combinations) <- majorpos_cols
        
        all.combinations <- tag_combination(all.combinations,
            cols         = majorpos_cols,
            combination  = majorpos_sort_order,
            tagname      = "valid"    
        )  
        
    } else {
        
        if(combinations_to_merge == "all"){
            # In this variant, all possible combinations of ngrams with unique majorpos codes are calulated.
                
            # Replicate the vector of majorpos_sort_orders x the number of tokens
            reps <- replicate(length(tokens),  majorpos_sort_order, simplify=FALSE)
                
            # Create a full factorial data.table of all combinations
            all.combinations <- { 
                expand.grid(reps) %>% 
                data.table(stringsAsFactors = F) %>% 
                transpose %>% 
                as.list %>lapply% { function(x){ 
                        x.possible <- all(majorpos_sort_order %in% x)
                        x.valid    <- all(x == majorpos_sort_order) 
                        c(x,x.possible, x.valid )  
                    }
                } %>% 
                    transpose %>%
                as.data.table     
            }
            
            # Name all columns
            names(all.combinations) <- c(majorpos_cols, "possible", "valid")
            
            # Select only combinations that are 'possible' (that is, those that have different POS)
            all.combinations <- all.combinations[possible == TRUE, ]
            
            # Remove the 'possible'column
            all.combinations <- all.combinations[, `:=`(possible = NULL)]  
        } 
    }    
    
    if(combinations_to_exclude %>% is.list){  
        
        all.combinations[, exclude := FALSE] 
        
        for(to_exclude in combinations_to_exclude){
            
            all.combinations <- tag_combination(all.combinations,
                cols         = majorpos_cols,
                combination  = to_exclude,
                tagname      = "exclude",
                reset        = FALSE
            )
        }
            
        all.combinations <- all.combinations[exclude == FALSE, ][, exclude := NULL]          
    }  
    
    stopifnot(all.combinations[valid == T,.N == 1])
         
    cat("The following majorpos combinations are considered \r\n") %if% verbose
    print(all.combinations)  %if% verbose   
 
    corr.comb.tmp <- all.combinations[valid == T,][, valid := NULL]  %>% unlist
  
    all.combinations[, valid := NULL]
    
    correct.orders <- corr.comb.tmp %>% names %>% as.list
    names(correct.orders) <- corr.comb.tmp %>% as.character
 
    for(i in 1:length(tokens)){
        all.combinations[, c("ngram_" %+% i) := lapply(get("majorpos_" %+% i) , function(x){
            correct.orders[[x]] %>% 
                stringi::stri_replace_all_fixed("majorpos_", "token_tmp_") %>% simplify2array
        }) ]
         
        all.combinations[, c("ngram_" %+% i) := unlist(get(c("ngram_" %+% i)))]
    }
    
    selectors <- {
        all.combinations[, ..majorpos_cols] %>%    
        transpose %>% 
        as.list %>lapply% function(x){  
            "majorpos_" %+% 1:length(x) %+% "==" %+% "\"" %+% x %+% "\"" 
        } %>% paste0(collapse = " & ")
    } %>% transpose %>% as.data.table
    
    
    selectors[, `:=`(select = V1,V1 = NULL)]
    
    
    assignments <- {
        all.combinations[, ..ngram_cols] %>%    
        transpose %>% 
        as.list %>lapply% function(x){  
            x  %+% "=" %+% tokens  
        } %>% paste0(collapse = ", ")
    } %>% transpose %>% as.data.table
    
    assignments[, `:=`(
        action  = "`:=`(" %+% V1 %+%")", 
        V1 = NULL
    )]
    
    actions <- cbind(selectors, assignments) %>% transpose %>% as.list
  
    for(action in actions){  
        selector.string <- action[[1]] %>% parse(text= .)  
        action.string   <- action[[2]] %>% enquote
          
        dtfrog[ selector.string %>% eval, eval(parse(text=eval(action.string))) ] 
    }  
    
    # Make the new ngram
    dtfrog[[colname_ngram_permuted]] <- do.call(paste, c(dtfrog[,..token_tmp_cols], sep = " - ")) 
     
    all.not.na.expression <- "!(" %+% paste0("is.na(token_tmp_" %+% 1:length(tokens) %+% ")", collapse ="|") %+% ")" %>% parse(text= .) 
    any.na.expression     <-  "(" %+% paste0("is.na(token_tmp_" %+% 1:length(tokens) %+% ")", collapse ="|") %+% ")" %>% parse(text= .) 
    
    if(non.matched == "keep"){   
        dtfrog[any.na.expression %>% eval,c(colname_ngram_permuted) := get(colname_ngram) ]  
    } else { 
        dtfrog <- dtfrog[all.not.na.expression %>% eval, ]
    } 
     
    for(colname in token_tmp_cols){
       setDT(dtfrog)[, (colname) := NULL]
    }
     
    return(dtfrog)
}


 
 
