library(tidyverse)
library(magrittr)
dfe <- readxl::read_excel("b_exerc_v1.xlsx")
dfe %<>%
  mutate(id = factor(id),
         interess = factor(interess,
                           levels=c(2,3),
                           labels=c("Secundário","Principal"),
                           ordered = T),
         tempocup = factor(tempocup,
                           levels = c(1:5),
                           labels=c("não tem",
                                    "até 2h",
                                    "de 2h a 4h",
                                    "de 4h a 6h",
                                    "+ de 6h"),
                           ordered=T),
         estcivil = factor(estcivil,
                           levels = c(1,2),
                           labels = c("Casado","Solteiro")),
         
         escola = factor(escola, 
                         levels = c(1:4),
                         labels = c("Tudo privada",
                            "Maior parte privada",
                            "Maior parte pública",
                            "Tudo pública"),
                         ordered=T)
  )
readr::write_rds(dfe,"dfe.rds")
