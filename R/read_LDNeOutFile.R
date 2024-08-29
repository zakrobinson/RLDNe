#'Read LDNe Tabular Output
#'
#'This function parses the tabular output from LDNe
#'
#'@param x An RLDNe object or file path to tabular LDNe output.
#'@param PopDecode Attempt to recode populations to original name. Default=T
#'@return data.frame of parsed LDNe output
#'@export





read_LDNeOutFile <- function(x,PopDecode=T){

  if(class(x)=="RLDNe_data"){
    classy=T
    if(is.null(x$LDNeOutFile)){
      stop("LDNeOutfile is NULL; have you ran create_LDNe_params() ?")
    }else if(!file.exists(x$LDNeOutFile)){
      stop("LDNeOutfile does not exist; have you ran run_LDNe() ?")
      }else{LDNeOutFile=x$LDNeOutFile}
  }else if(is.character(x)&length(x)==1){
    LDNeOutFile=x
  }else{
    stop("x should be a RLDNe_data object or a file path")
  }
lines <- readLines(LDNeOutFile)
strt <- grep("^-------------------------$",lines)+1
stop <- min(grep("^-------------------------------------$" ,lines))-2
text <- paste(lines[strt:stop],collapse = "\n")
res <- read.fwf(file = textConnection(text),widths = c(39,6,8,9,12,10,10,11,10,10,10,10,10))
colnames(res) <- c("Pop","SampSize","CritValue","WeightedHMean","IndepAlleles","r2","Exp_r2_Sample","Ne","low_para","high_para","low_jack","high_jack","Effdf")
res.1 <- res %>% mutate(Pop=gsub("^\\s+\\d{1,2}:|^\\s+$","",Pop)) %>% mutate(Pop=gsub("\\s+","",Pop))

for(i in 1:nrow(res.1)){
  if(!is.na(res.1$SampSize[i])){s=res.1$SampSize[i]}
  res.1$SampSize[i] <- s
  if(res.1$Pop[i]!=""){p=res.1$Pop[i]}
  res.1$Pop[i] <- p
}
if(classy & PopDecode & !is.null(x$Pop_Decode)){
res.1 <- res.1 %>% mutate(PopDecoded=x$Pop_Decode$Pop[match(Pop,x$Pop_Decode$LD_NAME)])
}

return(res.1)

}




