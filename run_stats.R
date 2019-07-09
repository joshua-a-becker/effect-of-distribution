source("get_data.R")

ag_numeric = subset(aggreg, communication=="Numeric")
ag_discussion = subset(aggreg, communication=="Discussion")

ag_towards = subset(aggreg, prop_toward>0.5)
ag_away = subset(aggreg, prop_toward<0.5)


### MAIN TEST:  DOES PROP_TOWARD MATTER?

## CLUSTER-ROBUST LOGISTIC REGRESSION
miceadds::glm.cluster(
      formula = improve==1 ~ trial + prop_toward
    , data=ag_discussion
    , cluster=ag_discussion$trial
    , family="binomial" 
    ) %>% summary

miceadds::glm.cluster(
      formula=improve==1 ~ dataset + trial + prop_toward
    , data=ag_numeric
    , cluster = paste0(ag_numeric$trial, ag_numeric$dataset)
    , family="binomial"
    ) %>% summary 

miceadds::glm.cluster(
  formula = improve==1 ~ trial + prop_toward
  , data=ag_numeric %>% subset(dataset=="becker2017")
  , cluster = (ag_numeric %>% subset(dataset=="becker2017"))$trial
  , family="binomial" 
) %>% summary

miceadds::glm.cluster(
  formula = improve==1 ~ trial + prop_toward
  , data=ag_numeric %>% subset(dataset=="becker2019")
  , cluster = (ag_numeric %>% subset(dataset=="becker2019"))$trial
  , family="binomial" 
) %>% summary

miceadds::glm.cluster(
  formula = improve==1 ~ trial + prop_toward
  , data=ag_numeric %>% subset(dataset=="lorenz2011")
  , cluster = (ag_numeric %>% subset(dataset=="lorenz2011"))$trial
  , family="binomial" 
) %>% summary

### DOES NUMERIC IMPROVE OVERALL?
table(ag_numeric$improve) %>%
  prop.test()


### DOES NUMERIC IMPROVE WHEN PHI>0.5?
with(ag_numeric %>% subset(prop_toward>0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test
  prop.table


### DOES NUMERIC IMPROVE WHEN PHI<0.5?
with(ag_numeric %>% subset(prop_toward<0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test
  prop.table


### DOES DISCUSSION IMPROVE WHEN PHI>0.5?
with(ag_discussion %>% subset(prop_toward>0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test
  prop.table
  

### DOES DISCUSSION IMPROVE WHEN PHI<0.5?
with(ag_discussion %>% subset(prop_toward<0.5), 
     table(ifelse(improve==1,"Good","Bad"))
) %>% 
  prop.test
  prop.table

  
### ARE THE TWO SETS OF TRIALS SIGNIFICANTLY DIFFERENT?
with(ag_discussion %>% subset(prop_toward!=0.5), 
     table(ifelse(improve==1,"Good","Bad"),  ifelse(prop_toward<0.5,"Minority","Majority"))
) %>%
  chisq.test
  prop.table(., margin=2)

### ARE THE TWO COMMUNICATION FORMATS DIFFERENT?
### WHEN MAJORITY IS AWAY?
with(aggreg %>% subset(prop_toward<0.5 & network=="Decentralized"), 
     table(ifelse(improve==1,"Good","Bad"), communication)
) %>% 
  chisq.test
  prop.table(., margin=2)


### ARE THE TWO COMMUNICATION FORMATS DIFFERENT?
### WHEN MAJORITY IS TOWARDS?
with(aggreg %>% subset(prop_toward>0.5 & network=="Decentralized"), 
     table(ifelse(improve==1,"Good","Bad"), communication)
) %>% 
  #chisq.test
  prop.test
  prop.table(., margin=2)



aggreg$majority = aggreg$prop_toward>0.5
d_sum = summarySE(aggreg %>% subset(network=="Decentralized" & prop_toward!=0.5)
                  , measurevar="improve"
                  , groupvars=c("communication","majority")
                  
                  )
ggplot(d_sum, aes(x=communication, y=improve,color=majority)) +
  geom_point(position=position_dodge(width=0.75))+
  geom_errorbar(aes(ymin=improve-ci, ymax=improve+ci), position=position_dodge(0.75), width=0)+
  geom_hline(yintercept=0.5, linetype="dashed") +
  beckertheme



ggsave("comparison.png", width=3.5, height=3)




