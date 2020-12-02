## loading qPCR data from positive San Diego Pacific Oysters ##
setwd("~/Documents/UMBC/Course work/R programming")
Pilot_SD <-read.csv("SD_Pilot_qPCR.csv", header=TRUE)
Pilot_SD
head(Pilot_SD)
attach(Pilot_SD)


## Log transform ###

Pilot_SD$LogData <- log10(Pilot_SD$Copies.mg+1)
head(Pilot_SD)


library(Rmisc)
library(ggplot2)


## summarization for error bars ###
Pilot_SD_Sum <- summarySE(Pilot_SD, "LogData", groupvars=c("Group", "Exposure"))
head(Pilot_SD_Sum)

Pilot_SD_Sum_raw <- summarySE(Pilot_SD, "Copies.mg", groupvars=c("Group", "Exposure"))
head(Pilot_SD_Sum_raw)

Pilot_SD_Sum2 <- Pilot_SD_Sum

Pilot_SD_Sum2$Exposure <-factor(Pilot_SD_Sum2$Exposure)

Pilot_SD_Sum2



## bar plot ###
SD_barplot <-  ggplot(Pilot_SD_Sum2, aes(x=Exposure, y=LogData, fill=Group)) +
    geom_bar(position=position_dodge(), stat="identity") +
    scale_fill_manual(values=c("turquoise4", "turquoise3", "turquoise2")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(fill = NA, colour = "gray78", size = 1),
          strip.background = element_rect(color= NA, fill= NA),
          axis.text=element_text(size=10), axis.title=element_text(size=12,face="bold")) +
    labs(y = "Log10 OsHV-1 copies/mg of tissue") +
    scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
    geom_errorbar(aes(ymin=LogData-se, ymax=LogData+se),width=.2, position=position_dodge(.9))

print(SD_barplot)






