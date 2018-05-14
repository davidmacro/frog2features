cat.call <- function(caller,description = '', debugmode = TRUE){
    
    if(debugmode){
        cat("\r\n\r\n") 
        cat("---------------------------------------------------------------------------\r\n")
        cat("Method called: ", caller, "\r\n")
        cat("\t\t description ", description, "\r\n")
        cat("---------------------------------------------------------------------------\r\n\r\n")
         
    } 
     
}


cat.short <- function(description = '', debugmode = TRUE){
    
    if(debugmode){ 
        cat("\t\t description ", description, "\r\n") 
    } 
    
}