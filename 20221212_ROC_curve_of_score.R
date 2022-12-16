# this script is to draw ROC curve using score based on relative rank about correlation coefficient
# made 2022/12/12

# activate package for drawing ROC curve
library(Epi)

# import list of treiber's physical interaction
# this list is located at "https://github.com/Ryosuke-Hirota/20221122_ROC_curve_of_pvalue_after_cutoff"
setwd("C:/Rdata/20221129_ROC_curve_of_pvalue_after_cutoff_revised")
phy.list <-read.table("list_of_treiber_physical_interaction_with_not_considering_primary_transcript.txt",sep="\t",header = T,stringsAsFactors = F)

# edit list of physical interaction 
phy.list[,5] <-paste0(phy.list[,3],"_vs_",phy.list[,2])
colnames(phy.list)[5] <-"combination"
phy.list <-phy.list[,c(5,4,1)]

# make list of score about relative rank
# these lists are located at ""\\fsw-q02\okamura-lab\Files_related_to_M1_Projects\Hirota\CCLE_plot\20221209_calculate_score_about_relative_rank
score.list <-list.files(path="C:/Rdata/20221209_calculate_score_about_relative_rank",pattern = "table_of_score")
score.list <-score.list[c(1,10,2:9,11:19)]

# set cutoff value
cutoff <-seq(0,900,50)

setwd("C:/Rdata")
dir.create("20221212_ROC_curve_of_score")

for (i in 1:length(score.list)) {
  setwd("C:/Rdata/20221209_calculate_score_about_relative_rank")
  
  # import
  score.df <-read.table(score.list[i],sep="\t",header = T,stringsAsFactors = F)
  
  score.data <-merge(score.df,phy.list,by="combination",all=T)
  score.data <-subset(score.data,!is.na(score.data[,2]))
  
  minus <-score.data[,4]<0 
  score.data[minus,4] <-score.data[minus,4]*-1
  
  score.data[,c(7,8)] <-NA
  colnames(score.data)[7:8] <-c("match","physical_interaction")
  
  score.data[!is.na(score.data[,5]),7] <-"match"
  score.data[is.na(score.data[,5]),7] <-"no_match"
  
  score.data[,5] <-ifelse(is.na(score.data[,5]),0,score.data[,5])
  
  score.data[score.data[,5]>3,8] <-1
  score.data[score.data[,5]<=3,8] <-0
  
  # count number of combinations with/without physical interaction
  # caution : this number is duplicated. If you wanna know non-duplicated number, write additional script.
  count <-as.data.frame(table(score.data[,7],score.data[,8]))
  p <-count[3,3]
  np <-count[1,3]
  
  # draw ROC curve
  setwd("C:/Rdata/20221212_ROC_curve_of_score")
  pdf(paste0("ROC_curve_of_score_cutoff_",cutoff[i],".pdf"))
  ROC(test=score.data$score, stat=score.data$physical_interaction, plot="ROC")
  mtext(text = paste0("physical interaction : ",p," , no physical interaction : ",np))
  dev.off()
}
