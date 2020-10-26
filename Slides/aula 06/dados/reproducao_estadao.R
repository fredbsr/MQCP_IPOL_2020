lista.de.pacotes = c("tidyverse","haven","lubridate","janitor","readxl",
                     "stringr","textclean","repmis","pander","qualtRics","sjlabelled",
                     "tidymodels","jtools","estimatr",
                     "ordinal","srvyr","ggstatsplot",
                     "flextable","compareGroups","skimr",
                     "magrittr","ggridges","ggpubr",
                     "ggstance","ggdark","gghighlight",
                     "hrbrthemes","viridis","extrafont",
                     "cregg","conjoint","cjoint",
                     "dabestr","patchwork") 

novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
#rm(lista.de.pacotes,novos.pacotes)
rm(list = ls())
gc()


# Cores e temas
loadfonts()

cores = rev(ggsci::pal_uchicago("dark")(5))

fonte_t = "Times New Roman"

theme_ipsum_mod <- theme_ipsum() +
  theme(panel.spacing=grid::unit(.25, "lines"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))


# Definiindo o diretorio de trabalho como do arquivo local
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Figura 1 ----

#write_rds(df_join,"df_join.rds")
df_join <- read_rds("df_join.rds")

p1 <- df_join %>% 
  mutate(isola_dummy=factor(case_when(
    isolamento >3 ~ 3,
    isolamento == 3 ~ 2,
    isolamento <3 ~ 1),
    levels = c(1,2,3),
    labels=c("Discorda","Indiferente","Concorda"),
    ordered = T)) %>%
  group_by(Rnd,ideologia_cat,isola_dummy) %>%
  summarise(n=n()) %>%
  group_by(Rnd,ideologia_cat) %>%
  mutate(prop=prop.table(n))

# Salvando tabela
#write_csv2(p1,"fig1_painel1.csv")

p1 %<>% 
  arrange(Rnd,ideologia_cat,desc(isola_dummy)) %>%
  mutate(lab_pos = cumsum(prop) - 0.5 * prop) %>%
  ggplot(aes(x=prop,fill=isola_dummy,y=Rnd)) +
  geom_bar(alpha=.75,stat = "identity",position = "stack") +
  geom_text(data=. %>% dplyr::filter(prop>0.1),
            aes(x=lab_pos,label=paste0(round(100*prop,0),"%")),
            size=3,fontface="bold",color="white") +
  scale_fill_manual(values = c("indianred3","grey50","dodgerblue4")) +
  scale_x_continuous(labels=scales::percent) +
  coord_cartesian(clip = "off",
                  expand = T) + 
  labs(x="",y="",fill="",title="Política de isolamento") +
  theme_ipsum_mod +
  #  theme_minimal() +
  facet_wrap(~ideologia_cat,ncol = 1) +
  theme(legend.position = "bottom",
        legend.key.height =unit(.75,"line"),
        panel.spacing=grid::unit(.25, "lines"))


p2 <- df_join %>% 
  mutate(isola_dummy=factor(case_when(
    bolsonaro >3 ~ 3,
    bolsonaro == 3 ~ 2,
    bolsonaro <3 ~ 1),
    levels = c(1,2,3),
    labels=c("Negativa","Neutra","Positiva"),
    ordered = T)) %>%
  group_by(Rnd,ideologia_cat,isola_dummy) %>%
  summarise(n=n()) %>%
  group_by(Rnd,ideologia_cat) %>%
  mutate(prop=prop.table(n)) 

# Salvando tabela
#write_csv2(p2,"fig1_painel2.csv")

p2 %<>% 
  arrange(Rnd,ideologia_cat,desc(isola_dummy)) %>%
  mutate(lab_pos = cumsum(prop) - 0.5 * prop) %>%
  ggplot(aes(x=prop,fill=isola_dummy,y=Rnd)) +
  geom_bar(alpha=.75,stat = "identity",position = "stack") +
  geom_text(data=. %>% dplyr::filter(prop>0.1),
            aes(x=lab_pos,label=paste0(round(100*prop,0),"%")),
            size=3,fontface="bold",color="white") +
  scale_fill_manual(values = c("indianred3","grey50","dodgerblue4")) +
  scale_x_continuous(labels=scales::percent) +
  coord_cartesian(clip = "off",
                  expand = T) + 
  labs(x="",y="",fill="",title="Avaliação de Bolsonaro") +
  theme_ipsum_mod +
  #  theme_minimal() +
  facet_wrap(~ideologia_cat,ncol = 1) +
  theme(legend.position = "bottom",
        legend.key.height =unit(.75,"line"),
        panel.spacing=grid::unit(.25, "lines"),
        axis.text.y = element_blank())


p3 <- df_join %>% 
  mutate(isola_dummy=factor(case_when(
    governadores >3 ~ 3,
    governadores == 3 ~ 2,
    governadores <3 ~ 1),
    levels = c(1,2,3),
    labels=c("Negativa","Neutra","Positiva"),
    ordered = T)) %>%
  group_by(Rnd,ideologia_cat,isola_dummy) %>%
  summarise(n=n()) %>%
  group_by(Rnd,ideologia_cat) %>%
  mutate(prop=prop.table(n)) 

# Salvando tabela
#write_csv2(p3,"fig1_painel3.csv")

p3 %<>% 
  arrange(Rnd,ideologia_cat,desc(isola_dummy)) %>%
  mutate(lab_pos = cumsum(prop) - 0.5 * prop) %>%
  ggplot(aes(x=prop,fill=isola_dummy,y=Rnd)) +
  geom_bar(alpha=.75,stat = "identity",position = "stack") +
  geom_text(data=. %>% dplyr::filter(prop>0.1),
            aes(x=lab_pos,label=paste0(round(100*prop,0),"%")),
            size=3,fontface="bold",color="white") +
  scale_fill_manual(values = c("indianred3","grey50","dodgerblue4")) +
  scale_x_continuous(labels=scales::percent) +
  coord_cartesian(clip = "off",
                  expand = T) + 
  labs(x="",y="",fill="",title="Avaliação dos Governadores") +
  theme_ipsum_mod +
  #  theme_minimal() +
  facet_wrap(~ideologia_cat,ncol = 1) +
  theme(legend.position = "bottom",
        legend.key.height =unit(.75,"line"),
        panel.spacing=grid::unit(.25, "lines"),
        axis.text.y = element_blank())

# Entra fig1
p1 + p2 + p3

# Figura 2 ----

#write_rds(importancia3,"importancia3.rds")
importancia3 <- read_rds("importancia3.rds")
# Salvando tabela
#write_csv2(importancia3,"fig2_painel2_barras.csv")


barras_importancia <-
  importancia3 %>%
  ggplot(aes(x=dim,y=value,fill=name,color=name)) +
  geom_bar(color=NA,alpha=.75,stat = "identity") +
  geom_text(fontface="bold",size=4,vjust=-.5,aes(label=paste0(round(value,1),"%"))) +
  facet_grid(name~dim,scales = "free",switch = "x",
             labeller = labeller(name = c("Bolsonaro_bem"="Avaliam BEM",
                                          "Bolsonaro_mal"="Avaliam MAL"))) +
  labs(x="",y="") +
  coord_cartesian(clip = "on",ylim = c(0,40),expand = F) + 
  scale_fill_manual(values = c("dodgerblue4","indianred4")) +
  scale_color_manual(values = c("dodgerblue4","indianred4")) +
  theme_ipsum_mod +
  theme(legend.position = "none",
        strip.text.y = element_text(size=12,face = "bold"),
        axis.text = element_blank(),
        axis.text.x = element_blank())


#write_rds(m1m2,"m1m2.rds")
m1m2 <- read_rds("m1m2.rds")

# Salvando tabela
#write_csv2(m1m2,"fig2_painel1_pontos.csv")

valencias_conjoint <-
  m1m2 %>%
  ggplot(aes(x=name,y=value,color=Perfil)) +
  geom_jitter(alpha=.025) +
  stat_summary(fun = mean, 
               geom = "point") + 
  
  stat_summary(fun.data = mean_cl_boot,  
               geom = "errorbar") +
  geom_hline(yintercept = 0,linetype=2) +
  geom_text(aes(y=valor_esq,label=valencia_esq),
            color="black",
            size=3,check_overlap = T) +
  geom_text(aes(y=valor_dir,label=valencia_dir),
            color="black",
            size=3,check_overlap = T) +
  facet_grid(Perfil~dim,scales = "free",switch = "x") +
  labs(x="",y="") +
  scale_fill_manual(values = c("dodgerblue4","indianred4")) +
  scale_color_manual(values = c("dodgerblue4","indianred4")) +
  coord_cartesian(clip = "on",ylim = c(-2.5,2.5),expand = F) + 
  theme_ipsum_mod +
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        #axis.text.x = element_text(angle = 45)
        axis.text.x = element_blank()) 
# Entra fig2
valencias_conjoint + 
  barras_importancia + 
  patchwork::plot_layout(widths = c(2.5,1)) 

# Figura 3 ----

#write_rds(preferences_cargas,"preferences_cargas.rds")
preferences_cargas <- read_rds("preferences_cargas.rds")

# mapa hex
mapa_hex <- preferences_cargas %>%
  dplyr::filter(ideologia>3) %>%
  
  left_join(
    preferences_cargas %>%
      dplyr::filter(ideologia>3) %>%
      group_by(voto_bolsonaro_grupo) %>%
      summarise(n=n()) %>% 
      mutate(prop=paste0("(",round(100*prop.table(n),0),"%)"))
  ) %>%
  
  mutate(voto_bolsonaro=forcats::fct_recode(factor(voto_bolsonaro_grupo),
                                            "Não votaria de jeito nenhum em Bolsonaro"="Anti-Bolsonaro",
                                            "Só votaria em Bolsonaro se o opositor fosse de esquerda ou do PT"="Anti-esquerda",
                                            "Votará em Bolsonaro provavelmente ou com certeza"="Pro-Bolsonaro")) %>%
  dplyr::select(Conservador,Liberal,voto_bolsonaro,prop)

# Salvando tabela
#write_csv2(mapa_hex,"fig3.csv")

mapa_hex %<>%
  mutate(voto_bolsonaro=str_wrap(paste(voto_bolsonaro,prop),35)) %>%
  ggplot(aes(x=Conservador,y=Liberal,
             color=voto_bolsonaro,
             fill=voto_bolsonaro,
             group=voto_bolsonaro)) +
  
  geom_hex(alpha=.75,color=NA,bins=55) +
  #geom_jitter(alpha=.75,size=.75) +
  stat_ellipse(type = "norm",size=.75,linetype = 2) +
  # stat_ellipse(type = "euclid",level=2,size=1.5,linetype = 2) +
  gghighlight::gghighlight(label_key = NULL,
                           unhighlighted_params = list(fill='grey75',
                                                       color=NA,
                                                       alpha=.5)
  ) + 
  
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(-5,-2.5,0,2.5,5),
                     limits = c(-6,6),
                     labels = c("progressista","","","","conservador")) +
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5),
                     limits = c(-6,6),
                     labels = c("desenvolvimentista","","","","liberal")) +
  labs(y="Preferência de Política Econômica",
       x="Preferência de visão de mundo",
       color="",
       fill=""#,
       #   title = "Expectativas de voto por perfil ideológico dos eleitores da direita e\ncentro-direita para Bolsonaro em 2022"
       
  ) +
  theme_ipsum() +
  
  facet_wrap(~voto_bolsonaro,ncol = 3) +
  theme(legend.position = "none",
        legend.text = element_text(size=10),
        axis.text.y = element_text(angle=90,hjust = 0),
        axis.title = element_text(size=10)
  )

mapa_hex

# Figura 4 ----
#write_rds(reg.df,"reg_df.rds")
reg.df <- read_rds("reg_df.rds")

flog = voto_dummy ~ 
  covid_n*Conservador +
  Liberal +
  Antipartido +
  Corrupcao +
  covid_n*prejuizo_d #+ evangelico

reg.log <- glm(family = "binomial",
               formula = flog, 
               data=reg.df)

pred_covid <- jtools::effect_plot(model=reg.log,pred=covid_n,interval = T) +
  scale_x_continuous("Exposição à covid-19",
                     breaks = c(1,2,3,4),
                     labels = c("Livre","Leve","Grave","Morte")) +
  scale_y_continuous(labels = scales::percent,
                     "Probabilidade de voto",
                     limits = c(0,1),
                     breaks = c(0,.05,.1,.15,.20,.25,.30,.35,.5,.75,1))  + 
  ggtitle("Efeitos marginais") +
  theme_ipsum_mod +
  theme(panel.spacing=grid::unit(.5, "lines"),
        plot.margin = ggplot2::margin(4, 4, 4, 4))

pred_conserva<- jtools::effect_plot(model=reg.log,pred=Conservador,interval = T) +
  scale_x_continuous("Conservadorismo",
                     breaks = c(-2.5,0,2.5,5),
                     labels = c("Progressista","","Conservador","")) +
  scale_y_continuous(labels = scales::percent,
                     "Probabilidade de voto",
                     limits = c(0,1)) +
  theme_ipsum_mod +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.spacing=grid::unit(.5, "lines"),
        plot.margin = ggplot2::margin(4, 4, 4, 4))


plot_reg <- jtools::plot_summs(reg.log,exp = T,
                               coefs=c( "Exposição à covid" = "covid_n",
                                        "Prejuízo econômico" = "prejuizo_d1",
                                        "Conservadorismo" = "Conservador",
                                        "Liberalismo" = "Liberal",
                                        "Antipartidarismo" = "Antipartido",
                                        "Pauta anticorrupção" = "Corrupcao"#,"evangelico","covid_n:Conservador","covid_n:prejuizo_d1"
                               )
) +
  scale_y_discrete(position = "left","") + 
  scale_x_continuous("100*(exp(b)-1)",
                     breaks = c(.75,1,1.25,1.75),
                     labels = c("-25%","0","+25%","+75%")) +
  ggtitle("Modelo de regressão") +
  theme_ipsum_mod +
  theme(panel.spacing=grid::unit(.5, "lines"),
        plot.margin = ggplot2::margin(4, 4, 4, 4))

# Entra 
plot_reg + pred_covid + pred_conserva + plot_layout(widths = c(2,1,1))

## Lixo ----
mapa_hex_point <- mapa_hex %>%
  group_by(voto_bolsonaro,prop) %>%
  summarise(Liberal=Hmisc::smean.cl.boot(Liberal),
            Conservador=Hmisc::smean.cl.normal(Conservador) ) %>%
  mutate(lab=c("mean","low","high")) %>%
  pivot_longer(3:4) %>%
  unite("lab_name",lab:name) %>%
  pivot_wider(names_from = lab_name,
              values_from = value)


mapa_hex_point %>%
  mutate(voto_bolsonaro=str_wrap(paste(voto_bolsonaro,prop,"\n\n"),35)) %>%
  ggplot(aes(color=voto_bolsonaro,
             fill=voto_bolsonaro,
             group=voto_bolsonaro)) +
  geom_point(aes(y=mean_Liberal,x=mean_Conservador),
             size=3.5) +
  geom_linerange(#color="black",
    aes(y=mean_Liberal,x=mean_Conservador,
        ymin=low_Liberal,ymax=high_Liberal),
    size=1.5) +
  geom_errorbarh(#color="black",
    aes(xmin=low_Conservador,xmax=high_Conservador,
        y=mean_Liberal,x=mean_Conservador),
    orientation = "x",width=0,
    size=1.5) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  annotate("text",label="+ conservador >>",y=0,x=1.5,
           hjust=1,vjust=-.5) +
  annotate("text",label="+ liberal >>",x=0,y=1.2,
           vjust=-.5,hjust=1,angle=90) +
  annotate("text",label="+ liberal >>",x=0,y=1.2,
           vjust=-.5,hjust=1,angle=90) +
  labs(y="",
       x="",
       color="",
       fill="",
       title = "Expectativas de voto por perfil ideológico dos eleitores da direita e\ncentro-direita para Bolsonaro em 2022"
       
  ) +
  theme_ipsum() +
  theme(legend.position = "none",
        legend.text = element_text(size=10),
        axis.text.y = element_text(angle=90,hjust = 0),
        axis.title = element_text(size=10),
        legend.spacing.y = unit(2, 'cm'),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm")
  )

mapa_hex_point %>%
  mutate(voto_bolsonaro=str_wrap(paste(voto_bolsonaro,prop),18)) %>%
  ggplot(aes(color=voto_bolsonaro,
             fill=voto_bolsonaro,
             group=voto_bolsonaro)) +
  geom_point(aes(y=mean_Liberal,x=mean_Conservador),
             size=3.5) +
  geom_linerange(#color="black",
    aes(y=mean_Liberal,x=mean_Conservador,
        ymin=low_Liberal,ymax=high_Liberal),
    size=1.5) +
  geom_errorbarh(#color="black",
    aes(xmin=low_Conservador,xmax=high_Conservador,
        y=mean_Liberal,x=mean_Conservador),
    orientation = "x",width=0,
    size=1.5) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  annotate("text",label="+ conservador >>",y=0,x=1.5,
           hjust=1,vjust=-.5) +
  annotate("text",label="+ liberal >>",x=0,y=1.2,
           vjust=-.5,hjust=1,angle=90) +
  directlabels::geom_dl(aes(y=mean_Liberal,x=mean_Conservador,label = voto_bolsonaro), 
                        method = list("top.points",vjust=2,cex=.75),
                        color="black") +
  # scale_x_continuous(breaks = c(-.5,0,1.75),
  #                    limits = c(-.5,1.75),
  #                    labels = c("+progr.","","+cons.")) +
  # scale_y_continuous(breaks = c(-.25,0,1.25),
  #                    limits = c(-.25,1.25),
  #                    labels = c("+des.","","+lib.")) +
  
  labs(y="",
       x="",
       color="",
       fill="",
       title = "Expectativas de voto por perfil ideológico dos eleitores da direita e\ncentro-direita para Bolsonaro em 2022"
       
  ) +
  theme_ipsum() +
  theme(legend.position = "none",
        legend.text = element_text(size=8),
        axis.text.y = element_text(angle=90,hjust = 0),
        axis.title = element_text(size=10),
        legend.spacing.y = unit(2, 'cm'),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm")
  )


## Carlos estadao
preferences_cargas %>%
  dplyr::filter(ideologia>3) %>%
  mutate(Conservador=factor(Conservador>0)) %>%
  tabyl(voto_bolsonaro_grupo,Conservador,demog_tp) %>% 
  janitor::adorn_percentages("col") %>%
  #  janitor::adorn_pct_formatting() %>%
  data.frame() %>%
  pivot_longer(c(2:3,5:6)) %>%
  mutate(name=mgsub(name,c("X1.","X0."),c("TeaParty_","Não Tea Party_"))) %>%
  separate(name,into = c("Evangelico","name"),sep = "_",extra = "merge") %>%
  dplyr::select(-X0.voto_bolsonaro_grupo) %>% rename(voto_bolsonaro=X1.voto_bolsonaro_grupo) %>%
  #  dplyr::filter(name %in% c("Identitário.Conservador","Pragmático.Conservador")) %>%
  ggplot(aes(x=voto_bolsonaro,y=round(100*value,1))) +
  geom_bar(color=NA,alpha=.75,stat = "identity") +
  geom_text(fontface="bold",vjust=-.5,aes(label=paste0(round(value*100,1),"%"))) +
  facet_grid(name~Evangelico) +
  labs(x="",y="") +
  coord_cartesian(clip = "off",
                  #ylim = c(0,110),
                  expand = T) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank())


preferences_cargas %>% 
  dplyr::filter(ideologia>3) %>%
  mutate(Conservador=factor(Conservador>0,labels = c("Progressista","Conservador")),
         tea_true = ifelse(demog_tp==1&Conservador=="Conservador",1,0)) %>%
  tabyl(voto_bolsonaro_grupo,tea_true) %>% 
  janitor::adorn_percentages("col") %>%
  #  janitor::adorn_pct_formatting() %>%
  data.frame() %>%
  pivot_longer(2:3) %>%
  mutate(name=mgsub(name,c("X1","X0"),c("TeaParty","Não Tea Party")))  %>%

  ggplot(aes(x=voto_bolsonaro_grupo,y=round(100*value,1))) +
  geom_bar(color=NA,alpha=.75,stat = "identity") +
  geom_text(fontface="bold",vjust=-.5,aes(label=paste0(round(value*100,1),"%"))) +
  facet_grid(.~name) +
  labs(x="",y="") +
  coord_cartesian(clip = "off",
                  #ylim = c(0,110),
                  expand = T) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank())



preferences_cargas %>% 
  dplyr::filter(ideologia>3) %>%
  mutate(Conserva=factor(Conservador>0,labels = c("Progressista","Conservador")),
         tea_true = ifelse(demog_tp==1&Conserva=="Conservador","Tea party","Não Tea Party")) %>%
  mutate(tea_true=evangelico) %>%
  dplyr::filter(ideologia>3) %>%
  ggplot(aes(x=Conservador,y=Liberal,
             color=tea_true,
             fill=tea_true,
             group=tea_true)) +
  
  geom_hex(alpha=.75,color=NA,bins=55) +
  #geom_jitter(alpha=.75,size=.75) +
  stat_ellipse(type = "norm",size=.75,linetype = 2) +
  # stat_ellipse(type = "euclid",level=2,size=1.5,linetype = 2) +
  gghighlight::gghighlight(label_key = NULL,
                           unhighlighted_params = list(fill='grey75',
                                                       color=NA,
                                                       alpha=.5)
  ) + 
  
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(-5,-2.5,0,2.5,5),
                     limits = c(-6,6),
                     labels = c("progressista","","","","conservador")) +
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5),
                     limits = c(-6,6),
                     labels = c("desenvolvimentista","","","","liberal")) +
  labs(y="Preferência de Política Econômica",
       x="Preferência de visão de mundo",
       color="",
       fill=""#,
       #   title = "Expectativas de voto por perfil ideológico dos eleitores da direita e\ncentro-direita para Bolsonaro em 2022"
       
  ) +
  theme_ipsum() +
  
  facet_wrap(~tea_true,ncol = 2) +
  theme(legend.position = "none",
        legend.text = element_text(size=10),
        axis.text.y = element_text(angle=90,hjust = 0),
        axis.title = element_text(size=10)
  )
