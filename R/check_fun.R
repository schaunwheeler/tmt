check_fun <- function(x,...){
  !(x %in% aspell(as.factor(x), 
                  control = paste(
                    paste0("--master=", "en_US"),
                    paste0("--sug-mode=", "ultra")
                  ))$Original)
}  
