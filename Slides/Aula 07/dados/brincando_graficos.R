library(rvest)
library(tidyverse)
library(googlesheets4)
library(purrr)
library(magrittr)

url_dados <- "https://docs.google.com/spreadsheets/d/1lVBlMkzn9npvJKdlIAWzKx2qXvos00t3fTno20L-7J0/edit?usp=sharing"
estados <- googlesheets4::read_sheet(url_dados,sheet = 1) %>%
  dplyr::rename(url_lavareda=Link) %>%
  mutate(xpath=paste0('//*[@id="post-',xpath,'"]/div/div/figure/table'))

# Function
pesquisa <- function(url_lavareda,xpath,UF){
  df <- url_lavareda %>%
    read_html() %>%
    html_node(xpath = xpath) %>%
    html_table(fill = TRUE,header=T) %>%
    janitor::clean_names() %>%
    mutate(UF=UF)
}

## Buscando todos os votos disponíveis
dados <- 
  pmap_dfr(list(
    url_lavareda = estados$url_lavareda,
    xpath = estados$xpath,
    UF=estados$UF),
#    .id = "DF",
#    .id = list(estados$UF),
    pesquisa) %>% dplyr::select(-x)


#levels(factor(df$pesquisa_das_pesquisas))

df <-dados %>%
  left_join(estados %>% dplyr::select(-xpath,-url_lavareda)) %>%
  dplyr::filter(candidato_partido!="") %>%
  mutate(candidato_partido = gsub(" \\| ","_",candidato_partido),
         candidato_partido = gsub("\\| ","_",candidato_partido),
         candidato_partido = gsub(" \\|","_",candidato_partido),
         #candidato_partido = gsub("\\|","_",candidato_partido),
         pesquisa_das_pesquisas=gsub("–","0%",pesquisa_das_pesquisas),
         perc=as.numeric(str_remove(pesquisa_das_pesquisas,"%"))) %>% 
  separate(candidato_partido,into = c("candidato","partido"),sep = "_")

df_plot <- df %>%
  dplyr::filter(candidato %in% c("BRANCO/NULO","NS/NR"))

plot <- df_plot %>%
  ggplot(aes(x=perc,y=paste0(Capital,"/",UF),fill=candidato)) +
  geom_bar(stat = "identity",
           position = "dodge",
           alpha=.7,
           width = .85) +
  geom_text(aes(label=paste0(perc,"%"),color=candidato),
            position=position_dodge(.85),
            hjust=0,
            size=3) +
  facet_grid(rows =vars(`Região`),scales = "free_y",space="free",switch = "y") +
  labs(fill="",x="",y="") +
  guides(color="none") +
  theme_minimal() +
  theme(legend.position = "top")

plot
