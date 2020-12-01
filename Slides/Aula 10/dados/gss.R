
# load in the dataset
data(gss)

# take a glimpse at it
str(gss)

# teste de independência
F_hat <- gss %>% 
  specify(age ~ partyid) %>%
  calculate(stat = "F")

null_distn <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = F_hat, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = F_hat, direction = "greater")