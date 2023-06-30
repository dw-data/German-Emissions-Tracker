#Set work directory
setwd("C:/Users/ajitn/Documents/R")

#Load packages
library(tidyverse)
library(ggplot2)

#Data comes from Umweltbundesamt, published 15.3.2023, available from two separate sheets here: https://www.umweltbundesamt.de/daten/klima/treibhausgas-emissionen-in-deutschland#nationale-und-europaische-klimaziele

#Import target data
data1<-read.csv("germany emissions future.csv") %>% 
  rename(year = Sektor) %>% 
  pivot_longer(!year, names_to="sector") %>% 
  mutate(newcol = rep("target",126)) %>% 
  rename(type = newcol)

#Import historic data
data2<-read.csv("germany emissions past.csv") %>% 
  rename(year = sector) %>% 
  pivot_longer(!year, names_to="sector") %>% 
  mutate(newcol = rep("historic",126)) %>% 
  rename(type = newcol)

#Join historic emissions with targeted emissions
data<-full_join(data1,data2)

#Filter by each sector and plot as line chart
power<-data %>% 
  filter(sector=="power")

power_plot<-ggplot(power[!is.na(power$value),],aes(x=year,y=value,color=type))+
  geom_line()+
  theme_minimal()+
  ylim(0,400)

industry<-data %>% 
  filter(sector=="industry")

industry_plot<-ggplot(industry,aes(x=year,y=value,color=type))+
  geom_line()+
  theme_minimal()+
  ylim(0,200)

buildings<-data %>% 
  filter(sector=="buildings")

buildings_plot<-ggplot(buildings,aes(x=year,y=value,color=type))+
  geom_line()+
  theme_minimal()+
  ylim(0,150)

transport<-data %>% 
  filter(sector=="transport")

transport_plot<-ggplot(transport,aes(x=year,y=value,color=type))+
  geom_line()+
  theme_minimal()+
  ylim(0,200)

agriculture<-data %>% 
  filter(sector=="agriculture")

agriculture_plot<-ggplot(agriculture,aes(x=year,y=value,color=type))+
  geom_line()+
  theme_minimal()+
  ylim(0,80)

waste<-data %>% 
  filter(sector=="waste")

waste_plot<-ggplot(waste,aes(x=year,y=value,color=type))+
  geom_line()+
  theme_minimal()+
  ylim(0,15)

#Create stacked bar plot for gesamt historic data
stacked <- data %>% 
  filter(type=="historic") %>% 
  filter(year=="2022")

stacked_plot <- ggplot(stacked, aes(x=year, y=value, fill=fct_reorder(sector, value)))+
  geom_bar(position="stack", stat="identity")+
  theme_minimal()

#Create stacked bar plot for gesamt 2022 and 2030 data
data3 <- data1 %>% 
  filter(year=="2030")
data4 <- data2 %>% 
  filter(year=="2022")
combined <- full_join(data3,data4)

combined_plot <- ggplot(combined, aes(x=year, y=value, fill=fct_reorder(sector, value)))+
  geom_bar(position="stack", stat="identity")+
  theme_minimal()

#Save as svg files
ggsave("power_plot.svg",power_plot)
ggsave("industry_plot.svg",industry_plot)
ggsave("buildings_plot.svg",buildings_plot)
ggsave("transport_plot.svg",transport_plot)
ggsave("agriculture_plot.svg",agriculture_plot)
ggsave("waste_plot.svg",waste_plot)
ggsave("stacked_plot.svg",stacked_plot)
ggsave("combined_plot.svg",combined_plot)
