rm(list=ls());gc();
library(readxl)
library(httr)
library(tidyverse)
library(ggplot2)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")



lorenz_url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"
if(!file.exists("lorenz_et_al.xls")) {
  GET(lorenz_url, write_disk(tf <- "lorenz_et_al.xls", overwrite=T))  
}
lorenz2011 <- read_excel("lorenz_et_al.xls") %>%
  mutate(
      pre_influence = E1
    , post_influence = E5
    , dataset="lorenz2011"
    , truth=Truth
    , trial= paste0(Information_Condition, Session_Date, dataset)
    , task=Question
    , network= fct_recode(Information_Condition, "Decentralized" = "full", "Solo" = "no", "Decentralized"="aggregated")  
    , communication="Numeric"
    , subject=paste0(dataset,Subject)
  ) %>% 
  subset(network=="Decentralized")

gurcay2015 = read.csv("GURCAY_et_al_newDataApr30.csv") %>%
  mutate(
      trial=paste0("gurcay2015",group)
    , dataset="gurcay2015"
    , task=question.no
    , pre_influence=est1
    , post_influence=est2
    , network= fct_recode(condition, "Solo" = "C", "Decentralized" = "I", "Decentralized"="G")
    , communication="Discussion"
    , truth=true.values
    , subject=paste0(dataset, subject.no)
  ) %>% 
  subset(network=="Decentralized")


becker2017 = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
                 , stringsAsFactors=F) %>%
  mutate(
      trial=paste0("becker2017",group_number)
    , dataset="becker2017"
    , pre_influence=response_1
    , post_influence=response_3
    , communication="Numeric"
    , subject=paste0(dataset, subject_id)
  ) %>% 
  subset(network=="Decentralized")

becker2019 = read.csv(url("https://raw.githubusercontent.com/joshua-a-becker/wisdom-of-partisan-crowds/master/Becker%20Centola%20Porter%20-%20Wisdom%20of%20Partisan%20Crowds%20-%20Supplementary%20Dataset.csv")) %>%
  mutate(
      trial=paste0(set,pair_id,network,experiment,party,"becker2019")
    , dataset="becker2019"
    , pre_influence=response_1
    , post_influence=response_3
    , task=q
    , network= fct_recode(network, "Decentralized" = "Social", "Solo" = "Control")  
    , communication="Numeric"
    , subject=paste0(dataset, user_id)
  ) %>% 
  subset(network=="Decentralized")



cols=c("pre_influence","post_influence","truth","task","trial","network","dataset", "communication", "subject")

d = rbind(
    becker2017[,cols]
  , lorenz2011[,cols]
  , becker2019[,cols]
  , gurcay2015[,cols]
  )



aggreg = d %>% 
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
  group_by(task, trial, network, dataset, communication) %>%
  mutate(
      mu1 = mean(pre_influence)
    , toward_truth = ifelse((pre_influence < mean(pre_influence) & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
  )  %>%
  summarize(
      truth=unique(truth)
    , N = length(pre_influence)

    ## calc mean
    , mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)

    ## cal median
    , med1 = median(pre_influence)
    
    ## error of mean
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth
    , mean_improve = ifelse(change_err_mu<0, "Improve","Worse")
    
    ## organizing stats
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_toward = mean(toward_truth=="Toward")
  
  ) %>%
  mutate(
      prop_toward_round=round(prop_toward,1)
    , improve=(change_err_mu<0)*1
    , majority = ifelse(prop_toward>0.5, "Toward", NA)
    , majority = ifelse(prop_toward<0.5, "Away", majority)
    , majority = ifelse(prop_toward==0.5, "Split", majority)
  ) %>% subset(
    N>4
  )





###
ag_sum = aggreg %>% 
  group_by(prop_toward_round,network,communication) %>%
  summarize(
    N = length(improve)
    ,upper=ifelse(mean(improve)%%1!=0, binom.test(table(improve))$conf.int[2], NA)
    ,lower=ifelse(mean(improve)%%1!=0, binom.test(table(improve))$conf.int[1], NA)
    ,improve = mean(improve)
  )


