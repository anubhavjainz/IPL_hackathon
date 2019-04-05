############ Importing the required libraries
library(dplyr)
library(ggplot2) #for the exploratory data analysis
library(plotly)


setwd("D:\\IPL\\train\\train")



ball_by_ball<-read.csv("ball_by_ball_data.csv",header = T,sep = ",",na.strings = "",stringsAsFactors = F)


ball_by_ball['match_year']=NA


for(i in 1:nrow(ball_by_ball)){
  ball_by_ball[i,'match_year']=substr(ball_by_ball[i,'match_id'], 1, 4)
}



batsmen_runs<-ball_by_ball%>%filter(is_super_over==0)%>%group_by(batsman,match_year)%>%summarise(Total_runs=sum(batsman_runs))%>%arrange(match_year,desc(Total_runs))

batsmen_dismissals<-ball_by_ball%>%filter(!is.na(player_dismissed))%>%group_by(player_dismissed,match_year)%>%summarise(Number=n())


t<-head(batsmen_runs%>%filter(match_year==2008),10)
p <- plot_ly(
  x = t$batsman,
  y = t$Total_runs,
  name = "SF Zoo",
  type = "bar"
)
p
