#finalDataset

##Alterar a designaçao do metier
##ver que colunas ficam e como ficam
## Adicionar coluna extra




###########ANALISE

##### SEM NA's

######## Fish by metier general
ds_fish_metier = ds %>% group_by(metier, classificacao) %>%
                        summarise(peso_total= sum(peso),
                                  .groups = 'drop')

library(dplyr)
ds_max_metier =ds_fish_metier %>% group_by(metier) %>% slice_max(peso_total)


######### Fish by metier general by year
ds_80_21_fish_metier = ds %>% group_by(metier,ano,classificacao) %>%
  summarise(peso_total= sum(peso),
            .groups = 'drop')

ds_80_21_max_fish =ds_80_21_fish_metier %>% group_by(metier,ano) %>% slice_max(peso_total)

library(ggplot2)
library(gcookbook) # Load gcookbook for the cabbage_exp data set

for(classe in levels(ds_80_21_final$metier)){
  title=paste("Classificação de peixe mais apanhado (em relação ao peso)por",classe)
  gg<-ggplot(ds_80_21_max_fish[ds_80_21_max_fish$metier==classe,], aes(x = ano, y = peso_total, fill = classificacao)) +
      geom_col()+labs(title=title, x ="Ano", y = "Peso total (kg)")
  print(gg)
  ggsave(paste("plots/FinalDataSet/",paste(title,".png",sep="")),width = 1800,height = 750,units = "px")
}

###########

## frequencia 
ds_80_21_fish_metier = descRandom_80_21 %>% group_by(metier,ano,classificacao) %>%
  summarise(peso_total= mean(peso),
            .groups = 'drop')

ds_80_21_max_fish =ds_80_21_fish_metier %>% group_by(metier,ano) %>% slice_max(peso_total)


ggplot(as.data.frame(UCBAdmissions),
       aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = classificacao), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")
  
