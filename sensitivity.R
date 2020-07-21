# Control delay = program delay * Theta / (Theta - (R_0-1)*gamma*time_between_treatment)
# Gamma = Rate of infection --> recovered
# Theta = - log (1- coverage*efficacy)
# Time will be measured in years
library(latex2exp)
library(tidyverse)


det_control_delay <- function(R0_arr = seq(0, 2, 0.0025), ce = 0.7, infect_dur = 0.5, program_delay = 1, treatment_gap = 1) {
  theta <- - log(1 - ce)
  control_delay <- program_delay * theta / (theta - (R0_arr - 1) * treatment_gap / infect_dur)
  tibble(R0 = R0_arr, control_delay = control_delay)
}

pd_array <- tibble(program_delay = seq(0,2,.0025))
pd_res <- pd_array %>% group_by(program_delay) %>% do({
  res <- det_control_delay(program_delay = .$program_delay)
#  res <- det_control_delay(program_delay = .$program_delay,R0_arr = seq(0, 2, 0.025))
  res
})
pd_res <- pd_res %>% filter (control_delay >0) %>% mutate(control_delay = ifelse(control_delay < 10, control_delay, 9.99))


p_pd <- ggplot (pd_res) +
  geom_tile(aes(x=R0, y = program_delay, fill= control_delay)) +
  scale_fill_distiller(palette = "Spectral", direction = -1, name = "Control delay") +
  geom_vline(xintercept = 1) +   geom_vline(xintercept = -log(0.3)/2+1) +
  annotate("text", x = 0.5, y = 0.5, label = "Hypoendemic", size = 4) +
  annotate("text", x = 1.3, y = 1, label = "Meso-\nendemic", size = 4) +
  annotate("text", x = 2.0, y = 1.5, label = "Hyper-\nendemic", size = 4) +
  xlab(TeX("$R_0$")) + ylab("Program delay (years)") + expand_limits(x = c(0, 2.3))
(p_pd)
ggsave("program_delayy071420.jpg",plot = p_pd)

##########################
  
id_array <- tibble(infect_dur_week = seq(10,50,.05))
#id_array <- tibble(infect_dur_week = seq(10,50,.5))
id_res <- id_array %>% group_by(infect_dur_week) %>% do({
  res <- det_control_delay(infect_dur = .$infect_dur_week*7/365.25)
  res
})
id_res <- id_res %>% filter (control_delay >0 & control_delay < 4.01)

p_id <- ggplot (id_res) +
  geom_tile(aes(x=R0, y = infect_dur_week, fill= control_delay)) +
  scale_fill_distiller(palette = "Spectral", direction = -1, name = "Control delay") + 
  geom_vline(xintercept = 1) +
  annotate("text", x = 0.5, y = 30, label = "Hypoendemic", size = 4) +
  annotate("text", x = 1.3, y = 40, label = "Meso-\nendemic", size = 4) +
  annotate("text", x = 1.7, y = 20, label = "Hyper-\nendemic", size = 4) +
  xlab(TeX("$R_0$")) + ylab("Infection duration (weeks)") + ggtitle("Sensitivity analysis for infection duration")
(p_id)

##########################

ce_array <- tibble(ce = seq(0.1,0.9, .001))
ce_res <- ce_array %>% group_by(ce) %>% do({
  res <- det_control_delay(ce = .$ce)
  res
})
ce_res <- ce_res %>% filter (control_delay >0 & control_delay < 4.01)

p_ce <- ggplot (ce_res) +
  geom_tile(aes(x=R0, y = ce, fill= control_delay)) +
  scale_fill_distiller(palette = "Spectral", direction = -1, name = "Control delay") + 
  geom_vline(xintercept = 1) +
  annotate("text", x = 0.5, y = .58, label = "Hypoendemic", size = 4) +
  annotate("text", x = 1.27, y = .79, label = "Meso-\nendemic", size = 4) +
  annotate("text", x = 1.55, y = .37, label = "Hyper-\nendemic", size = 4) +
  xlab(TeX("$R_0$")) + ylab("Overall MDA efficacy") + ggtitle("Sensitivity analysis for MDA efficacy")
(p_ce)

p_sens <-grid.arrange(p_id, p_ce)
ggsave("sensitivity071420.jpg",plot = p_sens)
