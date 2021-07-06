library(ggplot2)
library(ggpubr)
library(tidyverse)
library(MESS)

# Information Curve
df <- readxl::read_xlsx("information.xlsx", sheet = 1)
auc(df$t,df$LF, type = "spline")
(auc(df$t,df$LF, type = "spline")-auc(df$t,df$SF, type = "spline"))/auc(df$t,df$LF, type = "spline")
a <- df %>%
  ggplot() +
  geom_line(aes(theta,LF,linetype="RFUTN-PRO Long-form (15 Items)")) +
  geom_line(aes(theta,SF,linetype="RFUTN-PRO Short-form (10 Items)")) +
  xlab("Ability Score") + ylab("Information") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = c(0.32, 0.8), 
        legend.key = element_blank(), legend.text = element_text(size = 9),
        legend.box.background = element_rect(colour = "black")) 
  #scale_linetype_manual(name="Curve",values = c("Index Long-form"="solid", "Index Short-form"="dashed"))
df <- readxl::read_xlsx("information.xlsx", sheet = 2)
b <- df %>%
  ggplot() +
  geom_line(aes(theta,LF,linetype="AC-PRO Long-form (17 Items)")) +
  geom_line(aes(theta,SF,linetype="AC-PRO Short-form (10 Items)")) +
  xlab("Ability Score") + ylab("Information") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = c(0.32, 0.8), 
        legend.key = element_blank(), legend.text = element_text(size = 9),
        legend.box.background = element_rect(colour = "black")) 
df <- readxl::read_xlsx("information.xlsx", sheet = 3)
c <- df %>%
  ggplot() +
  geom_line(aes(theta,LF,linetype="RFUTN-CRO Long-form (19 Items)")) +
  geom_line(aes(theta,SF,linetype="RFUTN-CRO Short-form (10 Items)")) +
  xlab("Ability Score") + ylab("Information") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = c(0.32, 0.8), 
        legend.key = element_blank(), legend.text = element_text(size = 9), 
        legend.box.background = element_rect(colour = "black")) 
df <- readxl::read_xlsx("information.xlsx", sheet = 4)
d <- df %>%
  ggplot() +
  geom_line(aes(theta,LF,linetype="AC-CRO Long-form (12 Items)")) +
  geom_line(aes(theta,SF,linetype="AC-CRO Short-form (10 Items)")) +
  xlab("Ability Score") + ylab("Information") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = c(0.32, 0.8), 
        legend.key = element_blank(), legend.text = element_text(size = 9),
        legend.box.background = element_rect(colour = "black")) 
ggarrange(d, c, b, a + rremove("x.text"), 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
AUC(t,LF)-AUC(t,SF)

# Scoring
df1 <- readxl::read_xlsx("Summary_SF_highlighted.xlsx", sheet = 14)
df1$outcome <- ifelse(df$E_D == 0,0,1)
df1 %>% 
  select("Sum","outcome") %>%
  mutate(sumInt = round(Sum)) %>%
  arrange(sumInt) %>%
  View()
df2 <- df1 %>% 
  select("Sum","T_Score","SE", "outcome") %>%
  group_by(factor(round(Sum))) %>%
  summarize(meanT = mean(T_Score), meanSE = mean(SE), nn = n(), prop_D = mean(outcome))

df2 <- readxl::read_xlsx("Scoring.xlsx", sheet = 9)
colnames(df2) <- c("raw","meanT", "meanSE","n%","percent","N","7")
for(i in 1:nrow(df2)){
  meanTT = df2$meanT[i:nrow(df2)]
  df2$meanTSorted[i] = min(meanTT)
  df2$meanSESorted[i] = df2$meanSE[df2$meanT == min(meanTT)]
}
writexl::write_xlsx(df2, "~/Documents/GSR/PROMIS/Current Version/sortedtscores.xlsx")

#Summary Statistics
df <- read_csv("Final_Data_demo.csv")
df <- df[df$P_age_g != "g1:2-7",]
df1 <- df[,c("P_age_g","P_AGE","C_RACE","C_GENDER","C_AGE","C_LANG","P_GEN_CHL","P_CHILDRAC","P_GENDER","agegroup_P","P_LANG","P_RACE","P_CHILDRAC","C_N_LIV","P_INSUR","P_FAM_EMP","P_EUC","P_MARRSTAT","E_D","E_refer2")]
df1$C_GENDER <- ifelse(df1$C_GENDER == 1, "Male", "Female")
df %>%
  group_by(P_MARRSTAT) %>%
  summarise(n())
prop.table(table(df$P_MARRSTAT))

# Data for ML
df <- read_csv("data_CHL_IRT_head.csv")
df1 <- df[,c('P_WORRY', 'P_SOCNET1B', 'P_SOCCOM2', 'P_SH1',
'P_SELFEFF7', 'P_SELFEFF1', 'P_SCHMISS', 'P_OHEFFECT',
'P_COLOR_BAD', 'P_CHOPF', 'P_CHOPE', 'P_CHOPC', 'P_CHOPA', 'P_AFRAID',
'P_ACCESS6')]
df1 <- df[,c('P_WORRY', 'P_SOCNET1E', 'P_SOCNET1B', 
'P_SH1', 'P_SELFEFF2', 'P_SELFEFF', 'P_SCHMISS',
'P_OHEFFECT', 'P_CHOPG', 'P_CHOPF', 'P_CHOPE', 'P_CHOPD', 'P_CHOPC', 'P_CHOPB', 
'P_CHOPA', 'P_AFRAID', 'P_ACCESS1')]
df1 <- df[,c('C_SWALLOW', 'C_SOCNETE', 'C_PAIN','C_DEVELOP1',
'C_CHOPK','C_CHOPJ', 'C_CHOPI', 'C_CHOPG', 'C_CHOPF', 'C_BRUSH', 'C_ANXIET2A', 'C_AFRAID')]
df1 <- df[,c('C_SWALLOW', 'C_SOCNET1F', 'C_SOCNET1D', 
'C_SOCCOMP','C_SELFEFF1', 'C_SCHMISS', 'C_PAIN', 'C_CHOPK', 'C_CHOPJ',
'C_CHOPI', 'C_CHOPH', 'C_CHOPG', 'C_CHOPF','C_CHOPE', 'C_CHOPB', 
'C_CHOPA', 'C_BRUSH', 'C_BREATH1', 'C_AFRAID')]

write.csv(df1, "~/Documents/GSR/PROMIS/fffff.csv")

