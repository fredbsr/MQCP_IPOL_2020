
library(tidyverse)
library(moderndive)

tactile_prop_red


ggplot(tactile_prop_red, aes(x = prop_red)) +
   geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
   labs(x = "% de bolas vermelhas entre as 50 sorteadas",
        title = "Distribution of 33 proportions red")

bowl


pa_virtual <- bowl %>% 
  rep_sample_n(size = 50)
pa_virtual

pa_virtual %>% 
  mutate(is_red = (color == "red"))

pa_virtual %>% 
  mutate(is_red = (color == "red")) %>% 
  summarize(num_red = sum(is_red))

pa_virtual %>% 
  mutate(is_red = color == "red") %>% 
  summarize(num_red = sum(is_red)) %>% 
  mutate(prop_red = num_red / 50)



pa_virtual %>% 
  summarize(num_red = sum(color == "red")) %>% 
  mutate(prop_red = num_red / 50)


amostra_virtual <- bowl %>% 
  rep_sample_n(size = 50, reps = 33)
amostra_virtual



virtual_prop_red <- amostra_virtual %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 50)
virtual_prop_red



ggplot(virtual_prop_red, aes(x = prop_red)) +
   geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
   labs(x = "% de bolas vermelhas entre as 50 sorteadas",
        title = "Distribution of 33 proportions red")


amostra_virtual <- bowl %>% 
  rep_sample_n(size = 50, reps = 1000)
amostra_virtual


virtual_prop_red <- amostra_virtual %>% 
  group_by(replicate) %>% 
  summarize(red = sum(color == "red")) %>% 
  mutate(prop_red = red / 50)
virtual_prop_red


ggplot(virtual_prop_red, aes(x = prop_red)) +
   geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
   labs(x = "% de bolas vermelhas entre as 50 sorteadas",
        title = "Distribution of 1000 proportions red")




 # Segment 1: sample size = 25 ------------------------------
 # 1.a) Virtually use shovel 1000 times
amostra_virtual_25 <- bowl %>%
   rep_sample_n(size = 25, reps = 1000)

 # 1.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_25 <- amostra_virtual_25 %>%
   group_by(replicate) %>%
   summarize(red = sum(color == "red")) %>%
   mutate(prop_red = red / 25)

 # 1.c) Plot distribution via a histogram
ggplot(virtual_prop_red_25, aes(x = prop_red)) +
   geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
   labs(x = "Proportion of 25 balls that were red", title = "25")


 # Segment 2: sample size = 50 ------------------------------
 # 2.a) Virtually use shovel 1000 times
amostra_virtual_50 <- bowl %>%
   rep_sample_n(size = 50, reps = 1000)

 # 2.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_50 <- amostra_virtual_50 %>%
   group_by(replicate) %>%
   summarize(red = sum(color == "red")) %>%
   mutate(prop_red = red / 50)

 # 2.c) Plot distribution via a histogram
ggplot(virtual_prop_red_50, aes(x = prop_red)) +
   geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
   labs(x = "% de bolas vermelhas entre as 50 sorteadas", title = "50")


 # Segment 3: sample size = 100 ------------------------------
 # 3.a) Virtually using shovel with 100 slots 1000 times
amostra_virtual_100 <- bowl %>%
 rep_sample_n(size = 100, reps = 1000)

 # 3.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_100 <- amostra_virtual_100 %>%
   group_by(replicate) %>%
   summarize(red = sum(color == "red")) %>%
   mutate(prop_red = red / 100)

 # 3.c) Plot distribution via a histogram
ggplot(virtual_prop_red_100, aes(x = prop_red)) +
   geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
   labs(x = "Proportion of 100 balls that were red", title = "100")




 ---- eval=FALSE--------------------------------------------------------------
 # n = 25
virtual_prop_red_25 %>%
   summarize(sd = sd(prop_red))

 # n = 50
virtual_prop_red_50 %>%
   summarize(sd = sd(prop_red))

 # n = 100
virtual_prop_red_100 %>%
   summarize(sd = sd(prop_red))
























 -----------------------------------------------------------------------------
bowl %>% 
  summarize(sum_red = sum(color == "red"), 
            sum_not_red = sum(color != "red"))

