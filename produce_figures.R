source("get_data.R")

pct_labels = function(x) { paste0(x*100,"%")}


### FIGURE 1B
ggplot(aggreg, aes(x=prop_toward)) + 
  geom_histogram() + beckertheme +
  labs(x="", y="") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  scale_x_continuous(labels=pct_labels, lim=c(0,1))
ggsave("Fig 1B - Histogram of Phi.png", width=3.5, height=3)



### FIGURE 1C
adjust=c(rep(0.07,7), -0.05, rep(0.07, 1))
ggplot(ag_sum %>% subset(communication=="Discussion"), 
       aes(x=prop_toward_round, y=improve)) +
  geom_errorbar(aes(ymin=1-lower, ymax=1-upper), color="red", alpha=0.2, size=6, width=0)+
  geom_point(size=3)+
  geom_hline(yintercept=0.5, linetype="dashed") + 
  geom_vline(xintercept=0.5, linetype="dashed") +
  geom_label(aes(label=paste0(N), y=improve+adjust), size=3, label.padding=unit(0.15,"lines"))+
  scale_x_continuous(labels=pct_labels, lim=c(0,1))+ 
  scale_y_continuous(expand = c(0,0), lim=c(0,1), labels=pct_labels)+
  beckertheme +
  labs( y=""
       ,x="")
ggsave("Fig 1C - Effect of Phi - Unstructured Discussion.png", width=3.3, height=3)


### FIGURE 1D
ggplot(ag_sum %>% subset(communication=="Numeric"), 
       aes(x=prop_toward_round, y=improve)) +
  geom_errorbar(aes(ymin=1-lower, ymax=1-upper), color="red", alpha=0.2, size=6, width=0)+
  geom_point(size=3)+
  geom_hline(yintercept=0.5, linetype="dashed") + 
  geom_vline(xintercept=0.5, linetype="dashed") +
  geom_label(aes(label=paste0(N), y=improve+0.06), size=3, label.padding=unit(0.15,"lines"))+
  scale_x_continuous(labels=pct_labels, lim=c(0,1))+ 
  scale_y_continuous(expand = c(0,0), lim=c(0,1), labels=pct_labels)+
  #geom_errorbar(aes(ymin=improve-ci, ymax=improve+ci), width=0.02)+
  beckertheme +
  labs( y=""
        ,x="")
ggsave("Fig 1D - Effect of Phi - Numeric.png", width=3.3, height=3)



### PREP FOR FIGS 1E and 1F
d_sum = summarySE(aggreg %>% subset(network=="Decentralized")
                  , measurevar="improve"
                  , groupvars=c("communication","majority")
                  ) %>%
  subset(majority!="Split")

adjust =((d_sum %>% subset(majority!="Split" & communication=="Discussion"))$ci+0.05) * c(-1,1)
d_sum$majority = factor(d_sum$majority)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


### FIG 1E
ggplot(d_sum %>% subset(majority!="Split" & communication=="Discussion")
       , aes(x=majority, y=improve,color=majority)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_point(position=position_dodge(0.5), size=4) +
  geom_errorbar(aes(ymin=improve-ci, ymax=improve+ci)
                , size=1.15, width=0, position=position_dodge(0.5))+
  geom_label(aes(label=paste0(N), y=improve+adjust), size=3.5, position=position_dodge(0.5), label.padding=unit(0.15,"lines"))+
  beckertheme +
  scale_y_continuous(expand = c(0,0), lim=c(0,1), labels=pct_labels)+
  guides(color=F)+
  labs(x="", y="", color="")
ggsave("comparison_discussion.png", width=2, height=3)


### FIG 1F
adjust =((d_sum %>% subset(majority!="Split" & communication=="Numeric"))$ci+0.05)
ggplot(d_sum %>% subset(majority!="Split" & communication=="Numeric")
       , aes(x=majority, y=improve,color=majority)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  geom_point(position=position_dodge(0.5), size=4) +
  geom_errorbar(aes(ymin=improve-ci, ymax=improve+ci)
                , size=1.15, width=0, position=position_dodge(0.5))+
  geom_label(aes(label=paste0(N), y=improve+adjust), size=3.5, position=position_dodge(0.5), label.padding=unit(0.15,"lines"))+
  beckertheme +
  scale_y_continuous(expand = c(0,0), lim=c(0,1), labels=pct_labels)+
  guides(color=F)+
  labs(x="", y="", color="")
ggsave("comparison_delphi.png", width=2, height=3)

