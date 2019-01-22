#'Write LDNe Parameter File
#'
#'This function parses the tabular output from LDNe
#'
#'@param path path to tabular format LDNe output
#'@author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#'@return Path of submitted LDNe output and dataframe of parsed data
#'@export



readLDNe_tab<-function(path){
  x<-readLines(path)
strt<-which(x==paste0(rep("-",25),collapse = ""))+1
stp<-min(which(x==paste0(rep("-",37),collapse = "")))-2
linez<-x[strt:stp]
df<-data.frame(Pop=rep(NA,length(linez)),Pop_ID=rep(NA,length(linez)),Samp_Size=rep(NA,length(linez)),Crit_val=rep(NA,length(linez)),Weighted_H_Mean=rep(NA,length(linez)),Indep_Alleles=rep(NA,length(linez)),r2=rep(NA,length(linez)),Exp_r2_sample=rep(NA,length(linez)),Ne=rep(NA,length(linez)),Ne_para_lower=rep(NA,length(linez)),Ne_para_upper=rep(NA,length(linez)),Ne_jack_lower=rep(NA,length(linez)),Ne_jack_upper=rep(NA,length(linez)),Eff_df=rep(NA,length(linez)))

for( i in 1:length(linez)){

  if(length(grep(linez[i],pattern = ":"))>0) {

   r<-gsub(linez[i],pattern = "^\\s+(\\S{1,3}):(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)$",perl = T,replacement = "\\1,\\2,\\3,\\4,\\5,\\6,\\7,\\8,\\9,")
  r2<-gsub(linez[i],pattern = "^\\s+\\S{1,3}:\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)$",perl = T,replacement = "\\1,\\2,\\3,\\4,\\5")
  z<-paste0(r,r2)
  df[i,]<-strsplit(z,split = ",")[[1]]
  popdat<-df[i,1:3]

    }
  else{

    r<-gsub(linez[i],pattern = "\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(\\S+)$",perl = T,replacement = "\\1,\\2,\\3,\\4,\\5,\\6,\\7,\\8,\\9,")
    r2<-gsub(linez[i],pattern = "\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+(\\S+)\\s+(\\S+)$",perl = T,replacement = "\\1,\\2")
    z<-paste0(r,r2)
    df[i,4:ncol(df)]<-strsplit(z,split = ",")[[1]]
    df[i,1:3]<-popdat
  }
}

out<-list(path,df)
names(out)<-c("LDNEoutfile","DF_LDNe_output")
return(out)

}

