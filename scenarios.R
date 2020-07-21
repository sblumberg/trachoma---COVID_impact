library(gridExtra)
library(tidyverse)
library(latex2exp)
rm(list = ls())
setwd("/Users/sblumberg/Google Drive/Research/Papers/Trachoma papers/COVID delay/Figures/")

gen_time_series <- function(R = 1.625, gen_time = 0.5, init_val = .3, time_step = .001, tot_time = 4, treatment_efficacy = 0.7, treatment_times = c(0.5,1.5,2.5,3.5), scenario = "test", time_offset = 0.025) {
  
  current_time <-  tibble(time = 0, inf = init_val) #stores results for current generation
  time_series <- current_time #Will store all data
  treatment_index <- round(treatment_times / time_step)
  time_index <- 0
  
  while (time_index <= tot_time/time_step) {
    last_time <- current_time
    new_inf <- R*(time_step/gen_time) * last_time$inf * (1-last_time$inf)
    inf <- last_time$inf + new_inf - last_time$inf * (time_step/gen_time)
    time_index <- time_index + 1
    if (time_index %in% treatment_index) {
      inf <- inf * (1-treatment_efficacy)
    }
    current_time <- tibble(time = time_index*time_step, inf = inf)
    time_series <- bind_rows(time_series,current_time)
  }
  time_series$time <- time_series$time + time_offset
  time_series %>% mutate(Scenario = scenario)
}

# Hypoendemic
ts_hypo_ap <- gen_time_series(R = 0.95, treatment_times = c(0.5,1.5,2.5,3.5), scenario = "MDA as planned", time_offset = 0.025)
ts_hypo_ca <- gen_time_series(R = 0.95, treatment_times = c(0.5,2.5,3,3.5), scenario = "MDA catch-up", time_offset = 0.0)
ts_hypo_d <- gen_time_series(R = 0.95, treatment_times = c(0.5,2.5,3.5), scenario = "MDA disrupted", time_offset = 0.05)
ts_hypo <- bind_rows(ts_hypo_ap, ts_hypo_ca, ts_hypo_d) %>% mutate(class = "Hypoendemic")

# Mesoendemic
ts_meso_ap <- gen_time_series(R = 1.5, init_val = 0.3, treatment_times = c(0.5,1.5,2.5,3.5), scenario = "MDA as planned", time_offset = 0.025)
ts_meso_ca <- gen_time_series(R = 1.5, init_val = 0.3, treatment_times = c(0.5,2.5,3,3.5), scenario = "MDA catch-up", time_offset = 0.0)
ts_meso_d <- gen_time_series(R = 1.5, init_val = 0.3, treatment_times = c(0.5,2.5,3.5), scenario = "MDA disrupted", time_offset = 0.05)
ts_meso <- bind_rows(ts_meso_ap, ts_meso_ca, ts_meso_d) %>% mutate(class = "Mesoendemic")

# Hyperendemic
ts_hyper_ap <- gen_time_series(R = 2.5, init_val = 0.3, treatment_times = c(0.5,1.5,2.5,3.5), scenario = "MDA as planned", time_offset = 0.025)
ts_hyper_ca <- gen_time_series(R = 2.5, init_val = 0.3, treatment_times = c(0.5,2.5,3,3.5), scenario = "MDA catch-up", time_offset = 0.0)
ts_hyper_d <- gen_time_series(R = 2.5, init_val = 0.3, treatment_times = c(0.5,2.5,3.5), scenario = "MDA disrupted", time_offset = 0.05)
ts_hyper <- bind_rows(ts_hyper_ap, ts_hyper_ca, ts_hyper_d) %>% mutate(class = "Hyperendemic")

ts_all <- bind_rows(ts_hypo,ts_meso,ts_hyper)
ts_all$class <- factor(ts_all$class,c("Hypoendemic","Mesoendemic","Hyperendemic"))

ggplot(ts_all) +
  geom_hline(yintercept=0.05, col = "grey") +
  geom_line(aes(x=time, y = inf, col = Scenario)) +
  xlab("Time (years)") + ylab("Infection prevalence") + theme(text = element_text(size=20)) +
  facet_wrap(~class,ncol = 1, as.table = TRUE)
ggsave("scenarios071520.jpg")


ggplot(ts_meso %>% filter(Scenario != "MDA catch-up")) +
  geom_hline(yintercept=0.05, col = "grey") +
  geom_line(aes(x=time, y = inf, col = Scenario)) +
  annotate("text", x = 2, y = 0.093, label = "Program\n Delay", size = 6) +
  geom_segment(aes(x = 1.51, xend = 2.51, y = 0.08, yend = 0.08), arrow = arrow(ends = "both")) +
  annotate("text", x = 2.5, y = 0.158, label = "Control\n Delay", size = 6) +
  geom_segment(aes(x = 1.51, xend = 3.52, y = 0.145, yend = 0.145), arrow = arrow(ends = "both")) +
  annotate("text", x = 0.85, y = 0.275, label = "MDA", size = 6) +
  geom_segment(aes(x = 0.85, xend = 0.52, y = 0.269, yend = 0.25), arrow = arrow(ends = "last")) +
  annotate("text", x = 0.45, y = 0.075, label = "Control\n threshold", size = 6) +
  geom_segment(aes(x = 0.4, xend = 0.6, y = 0.0625, yend = 0.05), arrow = arrow(ends = "last")) +
  xlab("Time (years)") + ylab("Infection prevalence") + theme(text = element_text(size=20))
ggsave("schematic071620B.jpg")


##########################
# Stand alone figure
##########################
orig.clrz <- c("#FF2222","#DD2255","#BB2288","#8822BB","#5522DD","#2222FF")
plot(c(0,2),c(0,3),xlab=TeX("$R_0$"),ylab="Program delay (years)",type="n",xaxs="i",yaxs="i",yaxt="n")
axis(side=2,at=c(0,1,2,3),labels=c(0,1,2,3))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "#FF2222")
kk <- seq(0,5,by=1)
r.values <- seq(0,3,by=1/32)
theta <- log(0.3)
gamma <- 2
fn <- function(kk,rr) {(kk*( (rr-1)*gamma ) + kk*theta)/theta}
rbottom <- 1-theta/gamma
rtop <- 1+(3-kk[6])*theta/(kk[6]*gamma)
#polygon(c(rbottom,rtop,0,0), c(0,3,3,0), col="#DD2255",border=NA)
rtop <- 1+(3-kk[5])*theta/(kk[5]*gamma)
polygon(c(rbottom,rtop,0,0), c(0,3,3,0), col="#D0225E",border=NA)
rtop <- 1+(3-kk[4])*theta/(kk[4]*gamma)
polygon(c(rbottom,rtop,0,0), c(0,3,3,0), col="#992299",border=NA)
rtop <- 1+(3-kk[3])*theta/(kk[3]*gamma)
polygon(c(rbottom,rtop,0,0), c(0,3,3,0), col="#5522CC",border=NA)
rtop <- 1+(3-kk[2])*theta/(kk[2]*gamma)
polygon(c(rbottom,rtop,0,0), c(0,3,3,0), col="#2222FF",border=NA)

dd <- 0.925
text(1.6,2.5,labels="Hyperendemic",col="white",cex=1.4)
text(1.0,2.5-dd,labels="Mesoendemic",col="white",cex=1.4)
text(0.4,2.5-2*dd,labels="Hypoendemic",col="white",cex=1.4)
text(1.45,2.89,labels="> 4 years delay",col="white")
text(1.1,2.89,labels="3-4",col="white")
text(0.88,2.89,labels="2-3 years",col="white")
text(0.28,2.89,labels="1-2 years",col="white")
text(0.28,2.75,labels="control delay",col="white")
text(0.19,1.99,labels="<1 year",col="white")
text(0.19,1.85,labels="control delay",col="white")

