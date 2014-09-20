storm_data<-read.csv("repdata-data-StormData.csv")
names(storm_data)
dim(storm_data)

library(ggplot2)
#Since we don't need the whole dataset, we extract the useful attributes for the first question
use_att<-c("EVTYPE","FATALITIES","INJURIES")
harm_data<-storm_data[use_att]
#Computing a Pareto analysis of the data
#1. aggregating
attach(harm_data)
agg_harm<-aggregate(FATALITIES~EVTYPE,FUN=sum)
detach(harm_data)
#2. sorting
agg_harm<-agg_harm[order(-agg_harm$FATALITIES),]
agg_harm<-agg_harm[which(agg_harm$FATALITIES>0),]
total_fatal<-sum(agg_harm$FATALITIES)
agg_harm$Cumulated<-NA
agg_harm$Cumulated[1]<-agg_harm$FATALITIES[1]/total_fatal
agg_harm$Index<-1
index_80<-NULL
for (i in 2:(as.numeric((length(agg_harm$Cumulated))))){
        agg_harm[i,3]<-agg_harm[i-1,3]+(agg_harm[i,2]/total_fatal)
        agg_harm[i,4]<-i
}
for (i in 2:length(agg_harm$Cumulated)){
        if (agg_harm[i,3]>0.8){
                index_80<-(i)
                break
        }
}

#Creating the Pareto plot
par1<-ggplot(data=agg_harm,aes(Index,Cumulated))
par1<-par1+geom_line(ylab="Cumulated percentage of fatalities",col='red',size=1.1)
par1<-par1+labs(y="Cumulated fraction of fatalities",title="Pareto analysis for fatalities")
par1<-par1+xlim(c(0,NA))+ylim(c(0,1))
par1<-par1+geom_abline(intercept=0,slope=(1/length(agg_harm$Cumulated)))
par1<-par1+geom_abline(intercept=0.8,slope=0,col='orange',size=0.8)
par1<-par1+geom_vline(xintercept=as.numeric(index_80),col='blue',size=0.8)
print(par1)

#Creating the subset containing just the critical event types
agg_crit<-agg_harm[1:index_80,]
par2<-qplot(weight=FATALITIES,reorder(EVTYPE,-FATALITIES),data=agg_crit,geom="bar",ylab="Fatalities",fill=Index)

# Repeating the same process but including fatalities and injuries

#Creating a new attributes, sum of fatalities and injuries
harm_data$Harm<-harm_data$FATALITIES + harm_data$INJURIES

#Computing a Pareto analysis of the data
# aggregating
attach(harm_data)
agg2<-aggregate(Harm~EVTYPE,FUN=sum)
detach(harm_data)
#2. sorting
agg2<-agg2[order(-agg2$Harm),]
agg2<-agg2[which(agg2$Harm>0),]
total_harm<-sum(agg2$Harm)
agg2$Cumulated<-NA
agg2$Cumulated[1]<-agg2$Harm[1]/total_harm
agg2$Index<-1

index_h<-NULL
for (i in 2:(as.numeric((length(agg2$Cumulated))))){
        agg2[i,3]<-agg2[i-1,3]+(agg2[i,2]/total_harm)
        agg2[i,4]<-i
}
for (i in 2:length(agg2$Cumulated)){
        if (agg2[i,3]>0.8){
                index_h<-(i)
                break
        }
}
par3<-ggplot(data=agg2,aes(Index,Cumulated))
par3<-par3+geom_line(ylab="Cumulated percentage of harms",col='red',size=1.1)
par3<-par3+labs(y="Cumulated fraction of harms",title="Pareto analysis for harms")
par3<-par3+xlim(c(0,NA))+ylim(c(0,1))
par3<-par3+geom_abline(intercept=0,slope=(1/length(agg2$Cumulated)))
par3<-par3+geom_abline(intercept=0.8,slope=0,col='orange',size=0.8)
par3<-par3+geom_vline(xintercept=as.numeric(index_h),col='blue',size=0.8)

agg_crit2<-agg2[1:index_h,]
par4<-qplot(weight=Harm,reorder(EVTYPE,-Harm),data=agg_crit2,geom="bar",ylab="Harms",fill=Index)

#Question 2 : Across the United States, which types of events have 
#the greatest economic consequences?


