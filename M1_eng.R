# Helcom candidate indicator 'Seasonal succession of dominating phytoplankton groups'
# by Joanna Calkiewicz & Janina Kownacka according to the document 'Seasonal succession of dominating phytoplankton groups', 27.03.2015
# jcalkiewicz@mir.gdynia.pl
# National Marine Fisheries Research Institute, Gdynia, Poland
# 2016-02-25 #2018-12-13 modified lines: 32, 49

library (data.table)
library (dplyr)
library (ggplot2)

data_all <- read.csv("example.csv", sep=';', dec=',') #"example.csv" - the name of your file

for (i in 1:length(unique(data_all$taxa))) {
 data_taxa <- filter(data_all, taxa == unique(data_all$taxa)[i]) 
 name_taxa <- unique(data_all$taxa)[i]
 data_taxa<-data.table(data_taxa)
 mo <- c(1:12) # You can choose months
 
 data_taxa <- filter(data_taxa, month %in% mo)
 
 #log witout the average monthly biomass for each year
 data_taxa$lnbm <- log(data_taxa$biomass+1) #biomass on a natural log scale 
 
 #reference years
 years_ref <- c(1984:2006) #You can choose reference years
 years_test <- c(2007:2014) #You can choose test years
 
 data_taxa_ref <- filter(data_taxa, year %in% years_ref) 
 data_taxa_test <- filter(data_taxa, year %in% years_test) 
 
 #reference curve
 data_taxa_ref<- as.data.table(data_taxa_ref)
 data_taxa_ref_Z <-data_taxa_ref[,mean(lnbm), by=c("month")]
 data_taxa_ref_Z$V2 <- (data_taxa_ref_Z$V1-mean(data_taxa_ref$lnbm))/sd(data_taxa_ref$lnbm)
 setnames (data_taxa_ref_Z, c("month", "V1", "V2"), c("month", "mean_ref", "z_score_ref"))
 
 #z-score for testing reference data
 data_taxa_ref_Z_test<-data_taxa_ref[,mean(lnbm), by=c("year", "month")]
 setnames (data_taxa_ref_Z_test,c('year','month','V1'), c('year','month','lnbm'))
 
 data_taxa_ref_Z_test$V1 <- (data_taxa_ref_Z_test$lnbm-mean(data_taxa_ref$lnbm))/sd(data_taxa_ref$lnbm)
 setnames (data_taxa_ref_Z_test, c("year","month", "lnbm", "V1"), c("year","month", "lnbm_ref", "z_score_ref_test"))
 
 #Testing of reference data
 eqr_ref<- merge (data_taxa_ref_Z, data_taxa_ref_Z_test, by="month")
 eqr_ref$score_ref<-c(eqr_ref$z_score_ref_test<=eqr_ref$z_score_ref+0.5 & eqr_ref$z_score_ref_test>=eqr_ref$z_score_ref-0.5)
 
 #z-score for testing the test data
 data_taxa_test<- as.data.table(data_taxa_test)
 data_taxa_test_Z<-data_taxa_test [,mean(lnbm), by=c("year", "month")]
 setnames (data_taxa_test_Z, c('year', 'month','V1'), c('year', 'month','lnbm'))
 
 data_taxa_test_Z$V1 <- (data_taxa_test_Z$lnbm-mean(data_taxa_ref$lnbm))/sd(data_taxa_ref$lnbm)
 setnames (data_taxa_test_Z, c("year","month", "lnbm", "V1"), c("year","month", "lnbm_test", "z_score_test"))
 
 #Testing of test data
 eqr_test <- merge (data_taxa_ref_Z, data_taxa_test_Z, by="month")
 eqr_test$score_test<-c(eqr_test$z_score_test<=eqr_test$z_score_ref+0.5 & eqr_test$z_score_test>=eqr_test$z_score_ref-0.5)

 ############################## 
 #####plot1 - reference data#
 ##############################
rys<-eqr_ref
rys$year <- as.factor(rys$year)
title <- paste(name_taxa, "_", "test_",min(eqr_ref$year), "_", max(eqr_ref$year), "_", "ref_", min(eqr_ref$year), "_", max(eqr_ref$year), ".jpg", sep = "")
main_title <- paste(name_taxa, "test:",min(eqr_ref$year), "-", max(eqr_ref$year), "&", "ref:", min(eqr_ref$year), "-", max(eqr_ref$year), sep = " ")
jpeg (title)
print(ggplot() + 
 geom_point(aes(x=month, y=z_score_ref_test, color=year),rys,size=4) +
 geom_point(aes(month, z_score_ref), rys, size=3) +
 geom_line(aes(month, z_score_ref), rys, size=1, linetype="dashed") +
 geom_line(aes(month, z_score_ref+0.5), rys, linetype="dashed", size=1, color="red") +
 geom_line(aes(month, z_score_ref-0.5), rys, linetype="dashed", size=1, color="red")+
 ggtitle(main_title) + theme(plot.title = element_text(size=10))+
 scale_x_continuous(breaks=seq(min(rys$month), max(rys$month), 1)))
dev.off()
 
##############################
#####plot2 - test data#####
############################## 
 rys<-eqr_test
 rys$year <- as.factor(rys$year)
 
 title <- paste(name_taxa, "_", "test_",min(eqr_test$year), "_", max(eqr_test$year), "_", "ref_", min(eqr_ref$year), "_", max(eqr_ref$year), ".jpg", sep = "")
 main_title <- paste(name_taxa, "test:",min(eqr_test$year), "-", max(eqr_test$year), "&", "ref:", min(eqr_ref$year), "-", max(eqr_ref$year), sep = " ")
 
 
jpeg (title)
 print (ggplot() + 
 geom_point(aes(x=month, y=z_score_test, color=year),rys,size=4) +
 geom_point(aes(month, z_score_ref), rys, size=3) +
 geom_line(aes(month, z_score_ref), rys, size=1, linetype="dashed") +
 geom_line(aes(month, z_score_ref+0.5), rys, linetype="dashed", size=1, color="red") +
 geom_line(aes(month, z_score_ref-0.5), rys, linetype="dashed", size=1, color="red")+
 ggtitle(main_title) + theme(plot.title = element_text(size=10))+
 scale_x_continuous(breaks=seq(min(rys$month), max(rys$month), 1)))
 dev.off()
 
 ##############################
 ##### score file
 ##############################
 
 score <- matrix(data=NA, ncol= 3, nrow= 3)
 colnames (score) <- c("number_of_data", "ref_period", "test_period")
 rownames (score) <- c("all", "true", "score")
 
 (table(eqr_ref$score_ref)) ["TRUE"]
 (table(eqr_test$score_test)) ["TRUE"]
 
 score[1,1] <- dim (eqr_ref)[1]+dim (eqr_test)[1]
 score[1,2] <- dim (eqr_ref)[1]
 score[1,3] <- dim (eqr_test)[1]
 
 score[2,1] <- (table(eqr_ref$score_ref)) ["TRUE"] + (table(eqr_test$score_test)) ["TRUE"]
 score[2,2] <- (table(eqr_ref$score_ref)) ["TRUE"]
 score[2,3] <- (table(eqr_test$score_test)) ["TRUE"]
 
 score[3,1] <- ((table(eqr_ref$score_ref)) ["TRUE"] + (table(eqr_test$score_test)) ["TRUE"])/(dim (eqr_ref)[1]+dim (eqr_test)[1])
 score[3,2] <- (table(eqr_ref$score_ref)) ["TRUE"] / dim (eqr_ref)[1]
 score[3,3] <- (table(eqr_test$score_test)) ["TRUE"] / dim (eqr_test)[1]
 
 
 title <- paste(name_taxa, "_score_", "test_",min(eqr_test$year), "_", max(eqr_test$year), "_", "ref_", min(eqr_ref$year), "_", max(eqr_ref$year), ".csv", sep = "")
 
 write.table(score, file = title, quote = TRUE, sep = ";",
             eol = "\n", na = "NA", dec = ",", row.names = T,col.names = NA)

 
 title1 <- paste(name_taxa, "_", "test_",min(eqr_test$year), "_", max(eqr_test$year), "_", "ref_", min(eqr_ref$year), "_", max(eqr_ref$year), ".csv", sep = "")
 title2 <- paste(name_taxa, "_", "test_",min(eqr_ref$year), "_", max(eqr_ref$year), "_", "ref_", min(eqr_ref$year), "_", max(eqr_ref$year), ".csv", sep = "")
 write.table(eqr_test, file = title1, quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ",", row.names = T,col.names = NA)
 write.table(eqr_ref, file = title2, quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ",", row.names = T,col.names = NA)
 
}

rm(list=ls())







