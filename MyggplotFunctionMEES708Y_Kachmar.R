### Mariah Kachmar  ###
### MEES708Y Function homework ###

setwd("~/Documents/UMBC/Course work/R programming")
library(ggplot2)

### my ggplot function ###

myggplot <- function(x, timeframe, plotmean, mediancolor, meancolor, quantilecolor, mytitle, myxlab,
                     myylab){
    ## statistics ##
    mean <- apply (x, 1, FUN =mean, na.rm = TRUE)
    median <- apply (x, 1, FUN =median, na.rm = TRUE)
    quantiles <- apply (x, 1, FUN =quantile , probs = c(.25,.75), na.rm = TRUE)
    ### plots ###
    if (plotmean==TRUE) {
        graph<-ggplot(x, mapping=aes(x=timeframe, y= mean))+
            geom_ribbon(mapping=aes(ymin=quantiles[1,], ymax=quantiles[2,], color=quantilecolor),
                alpha= 1, fill= c(quantilecolor))+
            geom_line(mapping= aes(timeframe, median, color= mediancolor),size=1)+
            geom_line(mapping= aes(timeframe,mean, color= meancolor),size=1)+
            geom_point(mapping=aes(timeframe,mean,color="black"),shape=1, size=2.5)+
            ylab(myylab) +
            xlab(myxlab) +
            ggtitle(mytitle)+
            theme_linedraw()+
            scale_color_identity(name="", breaks= c(quantilecolor,meancolor,mediancolor),
                                 labels= c("Q1&Q3", "Average", "Median"),
                                 guide="legend")
        print(graph)
        return(graph)
    }

    if (plotmean==FALSE){
        graph<-ggplot(x, mapping=aes(x=timeframe, y=mean))+
            geom_ribbon(mapping=aes(ymin=quantiles[1,], ymax=quantiles[2,], color=quantilecolor),
                        alpha= 1, fill= c(quantilecolor))+
            geom_line(mapping= aes(timeframe, median, color= mediancolor),size=1)+
            geom_point(mapping=aes(timeframe,mean,color="black"),shape=1, size=2.5)+
            ylab(myylab) +
            xlab(myxlab) +
            ggtitle(mytitle)+
            theme_linedraw()+
            scale_color_identity(name="", breaks= c(quantilecolor,mediancolor),
                                 labels= c("Q1&Q3","Median"),
                                 guide="legend")

        print(graph)
        return(graph)
    }

    }

source('~/Documents/UMBC/Course work/R programming/R programming Part 2/MyggplotFunctionMEES708Y_Kachmar.R')

### Testing the function with the cluster 4 from ADcluster data ###
#load("ADcluster.Rdata")
#names(AD)
#names(cluster)
#cluster

### defining x & timeframe for function ###
#Clusters= names(cluster[cluster == 4])
#Clusters
#x <- AD[,Clusters]
#x

#myggplot(x,timeframe= c(1985:2014), plotmean=TRUE, mediancolor="grey50", meancolor= "black",
         #quantilecolor = "skyblue", mytitle= "clusters", myxlab="Attainment Deficit (%)",
         #myylab= "Year")
#NOTE: works for all clusters when assigning x to 1-4

###Testing with other data##
# Agriculture <-swiss[,2]
# Agriculture
# x<- as.data.frame(Agriculture)
# x
# myggplot(x, timeframe = c(1960:2006), plotmean = TRUE, meancolor ="black",
#          mediancolor = "grey50", quantilecolor = "skyblue", mytitle = "Swiss Agrigulture 1960-2007",
#           myxlab= "year", myylab="Agriculture")
#
