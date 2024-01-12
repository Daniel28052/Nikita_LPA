library(haven)
library(tidyverse)
library(semPlot)
library(tidyLPA)
library(ggplot2)

setwd("C:/Users/danie/Desktop/Honours Super/Nikita")

CAB <- 
  read_sav("C:/Users/danie/Desktop/Honours Super/Nikita/Totals_including3rdWave.sav")

#CAB_m <-  CAB %>%     # males
 # filter(Gender == 1) 
#CAB_f <- CAB %>%      # females
 # filter(Gender == 0)

Nikita <- CAB %>%
  select(2:3, 20:22,44:55)

names(Nikita)

################################################################################
##################### Models cheat sheet #######################################
################################################################################
#Model 1 is Equal variances and covariances fixed to 0 (CIDP) 
#Model 2 is Varying variances and covariances fixed to 0 (CVDP) 
#Model 3 is Equal variances and equal covariances (CIRP) 
#Model 4 and 5 are not able to fit Mclust 
#Model 6 is Varying variances and Varying covariances (CVUP) 
################################################################################
################################################################################

## Initial model fit
set.seed(123)
Models <- Nikita%>%
  select("SIMS_IMot_1","SIMS_IReg_1","SIMS_EReg_1","SIMS_Amot_1")%>%
  single_imputation()%>%
  estimate_profiles(2:4, variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("zero", "zero", "equal", "varying"))%>%
  compare_solutions(statistics = c("AIC","BIC","AWE", "CLC", "KIC"))

Models

## Filtering model
set.seed(125)
CIRP2 <- Nikita%>%
  select("SIMS_IMot_1","SIMS_IReg_1","SIMS_EReg_1","SIMS_Amot_1")%>%
  single_imputation()%>%
  estimate_profiles(2, variances="equal",covariances="equal")

set.seed(126)
CIRP3 <- Nikita%>%
  select("SIMS_IMot_1","SIMS_IReg_1","SIMS_EReg_1","SIMS_Amot_1")%>%
  single_imputation()%>%
  estimate_profiles(3, variances="equal",covariances="equal")

set.seed(127)
CIRP4 <- Nikita%>%
  select("SIMS_IMot_1","SIMS_IReg_1","SIMS_EReg_1","SIMS_Amot_1")%>%
  single_imputation()%>%
  estimate_profiles(4, variances="equal",covariances="equal")

## Plotting AIC/BIC/Entropy
fit <- as_tibble(rbind(CIRP2[["model_3_class_2"]][["fit"]],CIRP3[["model_3_class_3"]][["fit"]],
                       CIRP4[["model_3_class_4"]][["fit"]])) %>%
  select(Model,Classes,LogLik,AIC,BIC,Entropy,n_min,BLRT_p)
fit

aic2 <- as.numeric(fit %>% select(AIC)%>% slice(1))
aic3 <- as.numeric(fit %>% select(AIC)%>% slice(2))
aic4 <- as.numeric(fit %>% select(AIC)%>% slice(3))
bic2 <- as.numeric(fit %>% select(BIC)%>% slice(1))
bic3 <- as.numeric(fit %>% select(BIC)%>% slice(2))
bic4 <- as.numeric(fit %>% select(BIC)%>% slice(3))
e2 <- as.numeric(fit %>% select(Entropy)%>% slice(1))
e3 <- as.numeric(fit %>% select(Entropy)%>% slice(2))
e4 <- as.numeric(fit %>% select(Entropy)%>% slice(3))

## Plot best model
`Number of Profiles` <- as.factor(c(2,3,4))
AIC <- c(aic2,aic3,aic4)
BIC <- c(bic2,bic3,bic4)
Entropy <- c(e2,e3,e4)
Model_metrics <- data.frame(`Number of Profiles`,AIC,BIC,Entropy)

scl = 30000
Model_metrics %>%
  ggplot(aes(x = `Number of Profiles`)) + 
  geom_line(aes(y = AIC, colour = "AIC"), size = 1.5, group=1) +
  geom_line(aes(y = BIC, colour = "BIC"), size = 1.5, group=1) +
  geom_line(aes(y = Entropy*scl, colour = "Entropy"), size = 1.5,group=1) +
  scale_y_continuous(sec.axis = sec_axis(~. /scl , name = "Entropy")) +
  labs(colour = "Index", y = "AIC / BIC") + 
  theme(legend.position="bottom")

#ggsave("Figure_1.tiff", tt, width = 6, height = 6, dpi=300)

################################################################################
########################## Latent profiles #####################################
################################################################################
#write_sav(get_data(CIRP2), "LPA_with_participant_class.sav")
data2 <- read_sav("LPA_with_participant_class.sav")

names(data2)

data2%>%
  group_by(Class)%>%
  count(Class)%>%
  mutate(Perc = (n/276)*100)

## Std table
data2%>%
  select(Class,ZSIMS_IMot_1,ZSIMS_IReg_1,ZSIMS_EReg_1,ZSIMS_Amot_1) %>%
  group_by(Class) %>%
  summarise(ZSIMS_IMot_1=mean(ZSIMS_IMot_1),
            ZSIMS_IReg_1=mean(ZSIMS_IReg_1),
            ZSIMS_EReg_1=mean(ZSIMS_EReg_1),
            ZSIMS_Amot_1=mean(ZSIMS_Amot_1)) %>%
  na.omit()

## Raw scores table
data2%>%
  select(Class,SIMS_IMot_1,SIMS_IReg_1,SIMS_EReg_1,SIMS_Amot_1) %>%
  group_by(Class) %>%
  summarise(SIMS_IMot_1=mean(SIMS_IMot_1),
            SIMS_IReg_1=mean(SIMS_IReg_1),
            SIMS_EReg_1=mean(SIMS_EReg_1),
            SIMS_Amot_1=mean(SIMS_Amot_1)) %>%
  na.omit()


LPA_data1 <- data2%>%
  select(Class,ZSIMS_IMot_1,ZSIMS_IReg_1,ZSIMS_EReg_1,ZSIMS_Amot_1) %>%
  group_by(Class) %>%
  summarise(ZSIMS_IMot_1=mean(ZSIMS_IMot_1),
            ZSIMS_IReg_1=mean(ZSIMS_IReg_1),
            ZSIMS_EReg_1=mean(ZSIMS_EReg_1),
            ZSIMS_Amot_1=mean(ZSIMS_Amot_1))%>%
  pivot_longer(cols=c(ZSIMS_IMot_1,ZSIMS_IReg_1,ZSIMS_EReg_1,ZSIMS_Amot_1), 
               names_to="Model_Indicators",
               values_to="Z_Scores")

LPA_data1$Class <- as.factor(LPA_data1$Class)
levels(LPA_data1$Class) <- c("Low-Risk Motivation","High-Risk Motivation")
LPA_data1$Model_Indicators <- as.factor(LPA_data1$Model_Indicators)
LPA_data1$Model_Indicators <- fct_relevel(LPA_data1$Model_Indicators,
                                          "ZSIMS_Amot_1","ZSIMS_EReg_1","ZSIMS_IReg_1","ZSIMS_IMot_1")


tt <- LPA_data1 %>%
  ggplot(aes(x=Model_Indicators, y=Z_Scores, group=Class, color=Class)) + 
  geom_point(size = 1.5) + geom_line(linewidth = 1.5) +
  labs(x= "Model Indicators", y = "Z scores", color = "Profiles") + 
  theme(axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1.0),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"), 
        legend.position="bottom") +
  scale_y_continuous(breaks=seq(-2.0, 2.0, by = 0.5)) +
  scale_x_discrete(labels=c("ZSIMS_IMot_1"="Intrinsic motivation",
                            "ZSIMS_IReg_1"="Identified regulation",
                            "ZSIMS_EReg_1"="External regulation",
                            "ZSIMS_Amot_1"="Amotivation"))

#ggsave("Figure_2.tiff", tt, width = 6, height = 6, dpi=300)

################################################################################
######## Plotting rep measures anova separated by Latent profiles ##############
################################################################################
data3 <- read_sav("Nikita_dataset.sav")

data3$Profile <- as_factor(data3$Profile)
levels(data3$Profile) <- c("Low-Risk Motivation","High-Risk Motivation")

b1 <-  data3 %>%
  select(Profile,T_Bsmas_1,T_Bsmas_2,T_Bsmas_3)%>%
  group_by(Profile) %>% 
  summarise_all(list(mean, sd)) %>%
  rename(Bsmas1_mean = T_Bsmas_1_fn1, Bsmas2_mean = T_Bsmas_2_fn1,
         Bsmas3_mean = T_Bsmas_3_fn1, Bsmas1_sd = T_Bsmas_1_fn2 ,
         Bsmas2_sd = T_Bsmas_2_fn2 ,Bsmas3_sd = T_Bsmas_3_fn2 ) %>%
  pivot_longer(!Profile, names_to = c("wave", "fun"), 
               names_sep = "_") %>%
  pivot_wider(names_from = fun, values_from = value)
  


b2 <- b1 %>% 
  ggplot(aes(x=wave, y=mean, group=Profile, color=Profile)) +
  geom_point(size = 1.5) + geom_line(linewidth = 1.5) + 
  labs(x= "", y = "Problematic Social media use", color = "Profiles") +
  theme(axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"), 
        legend.position="bottom") +
  scale_colour_manual(values=c("#999999","#E69F00"))+
  scale_x_discrete(labels=c("Bsmas1"="Jan 2021",
                            "Bsmas2"="Jan 2022",
                            "Bsmas3"="Jan 2023"))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2)

ggsave("Figure_3.tiff", b2, width = 6, height = 6, dpi=300)





