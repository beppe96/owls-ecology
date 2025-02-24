rm(list=ls())

setwd("C:/Users/2794008O/OneDrive - University of Glasgow/Desktop/Data analysis/data/PhD/Aim 2")


library(lme4)
library(dplyr)
library(ggplot2)
library(stats)
library(car)
library(DHARMa)
library(stringr)
library(ggeffects)
library(corrplot)
library(psych)
library(writexl)
library(interactions)
library(MuMIn)


data <- read.csv("dataset tawny owl.csv")

str(data)

glimpse(data)

summary(data)



# land cover variation within 500 m radius over time per each country for Appendix ms
scotland <- filter(data, country == "Scotland")
scotland2 <- filter(scotland, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.scotland <- scotland2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.scotland$woodland.tot)
sd(woodland.scotland$woodland.tot)
initial_value_scotland_woodland <- woodland.scotland$woodland.tot[woodland.scotland$year == 2017]
final_value_scotland_woodland <- woodland.scotland$woodland.tot[woodland.scotland$year == 2023]
rate_of_change_scotland_woodland <- (final_value_scotland_woodland - initial_value_scotland_woodland) / (2023 - 2017)*100
print(rate_of_change_scotland_woodland)

urban.scotland <- scotland2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.scotland$urban.tot)
sd(urban.scotland$urban.tot)
initial_value_scotland_urban <- urban.scotland$urban.tot[urban.scotland$year == 2017]
final_value_scotland_urban <- urban.scotland$urban.tot[urban.scotland$year == 2023]
rate_of_change_scotland_urban <- (final_value_scotland_urban - initial_value_scotland_urban) / (2023 - 2017)*100
print(rate_of_change_scotland_urban)

openland.scotland <- scotland2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.scotland$openland.tot)
sd(openland.scotland$openland.tot)
initial_value_scotland_openland <- openland.scotland$openland.tot[openland.scotland$year == 2017]
final_value_scotland_openland <- openland.scotland$openland.tot[openland.scotland$year == 2023]
rate_of_change_scotland_openland <- (final_value_scotland_openland - initial_value_scotland_openland) / (2023 - 2017)*100
print(rate_of_change_scotland_openland)

norway <- filter(data, country == "Norway")
norway2 <- filter(norway, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.norway <- norway2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.norway$woodland.tot)
sd(woodland.norway$woodland.tot)
initial_value_norway_woodland <- woodland.norway$woodland.tot[woodland.norway$year == 2017]
final_value_norway_woodland <- woodland.norway$woodland.tot[woodland.norway$year == 2023]
rate_of_change_norway_woodland <- (final_value_norway_woodland - initial_value_norway_woodland) / (2023 - 2017)*100
print(rate_of_change_norway_woodland)

urban.norway<- norway2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.norway$urban.tot)
sd(urban.norway$urban.tot)
initial_value_norway_urban <- urban.norway$urban.tot[urban.norway$year == 2017]
final_value_norway_urban <- urban.norway$urban.tot[urban.norway$year == 2023]
rate_of_change_norway_urban <- (final_value_norway_urban - initial_value_norway_urban) / (2023 - 2017)*100
print(rate_of_change_norway_urban)

openland.norway <- norway2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.norway$openland.tot)
sd(openland.norway$openland.tot)
initial_value_norway_openland <- openland.norway$openland.tot[openland.norway$year == 2017]
final_value_norway_openland <- openland.norway$openland.tot[openland.norway$year == 2023]
rate_of_change_norway_openland <- (final_value_norway_openland - initial_value_norway_openland) / (2023 - 2017)*100
print(rate_of_change_norway_openland)

sweden <- filter(data, country == "Sweden")
sweden2 <- filter(sweden, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.sweden <- sweden2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.sweden$woodland.tot)
sd(woodland.sweden$woodland.tot)
initial_value_sweden_woodland <- woodland.sweden$woodland.tot[woodland.sweden$year == 2017]
final_value_sweden_woodland <- woodland.sweden$woodland.tot[woodland.sweden$year == 2022]
rate_of_change_sweden_woodland <- (final_value_sweden_woodland - initial_value_sweden_woodland) / (2022 - 2017)*100
print(rate_of_change_sweden_woodland)

urban.sweden<- sweden2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.sweden$urban.tot)
sd(urban.sweden$urban.tot)
initial_value_sweden_urban <- urban.sweden$urban.tot[urban.sweden$year == 2017]
final_value_sweden_urban <- urban.sweden$urban.tot[urban.sweden$year == 2022]
rate_of_change_sweden_urban <- (final_value_sweden_urban - initial_value_sweden_urban) / (2022 - 2017)*100
print(rate_of_change_sweden_urban)

openland.sweden <- sweden2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.sweden$openland.tot)
sd(openland.sweden$openland.tot)
initial_value_sweden_openland <- openland.sweden$openland.tot[openland.sweden$year == 2017]
final_value_sweden_openland <- openland.sweden$openland.tot[openland.sweden$year == 2022]
rate_of_change_sweden_openland <- (final_value_sweden_openland - initial_value_sweden_openland) / (2022 - 2017)*100
print(rate_of_change_sweden_openland)

finland <- filter(data, country == "Finland")
finland2 <- filter(finland, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.finland <- finland2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.finland$woodland.tot)
sd(woodland.finland$woodland.tot)
initial_value_finland_woodland <- woodland.finland$woodland.tot[woodland.finland$year == 2017]
final_value_finland_woodland <- woodland.finland$woodland.tot[woodland.finland$year == 2023]
rate_of_change_finland_woodland <- (final_value_finland_woodland - initial_value_finland_woodland) / (2023 - 2017)*100
print(rate_of_change_finland_woodland)

urban.finland<- finland2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.finland$urban.tot)
sd(urban.finland$urban.tot)
initial_value_finland_urban <- urban.finland$urban.tot[urban.finland$year == 2017]
final_value_finland_urban <- urban.finland$urban.tot[urban.finland$year == 2023]
rate_of_change_finland_urban <- (final_value_finland_urban - initial_value_finland_urban) / (2023 - 2017)*100
print(rate_of_change_finland_urban)

openland.finland <- finland2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.finland$openland.tot)
sd(openland.finland$openland.tot)
initial_value_finland_openland <- openland.finland$openland.tot[openland.finland$year == 2017]
final_value_finland_openland <- openland.finland$openland.tot[openland.finland$year == 2023]
rate_of_change_finland_openland <- (final_value_finland_openland - initial_value_finland_openland) / (2023 - 2017)*100
print(rate_of_change_finland_openland)

lithuania <- filter(data, country == "Lithuania")
lithuania2 <- filter(lithuania, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.lithuania <- lithuania2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.lithuania$woodland.tot)
sd(woodland.lithuania$woodland.tot)
initial_value_lithuania_woodland <- woodland.lithuania$woodland.tot[woodland.lithuania$year == 2017]
final_value_lithuania_woodland <- woodland.lithuania$woodland.tot[woodland.lithuania$year == 2023]
rate_of_change_lithuania_woodland <- (final_value_lithuania_woodland - initial_value_lithuania_woodland) / (2023 - 2017)*100
print(rate_of_change_lithuania_woodland)

urban.lithuania<- lithuania2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.lithuania$urban.tot)
sd(urban.lithuania$urban.tot)
initial_value_lithuania_urban <- urban.lithuania$urban.tot[urban.lithuania$year == 2017]
final_value_lithuania_urban <- urban.lithuania$urban.tot[urban.lithuania$year == 2023]
rate_of_change_lithuania_urban <- (final_value_lithuania_urban - initial_value_lithuania_urban) / (2023 - 2017)*100
print(rate_of_change_lithuania_urban)

openland.lithuania <- lithuania2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.lithuania$openland.tot)
sd(openland.lithuania$openland.tot)
initial_value_lithuania_openland <- openland.lithuania$openland.tot[openland.lithuania$year == 2017]
final_value_lithuania_openland <- openland.lithuania$openland.tot[openland.lithuania$year == 2023]
rate_of_change_lithuania_openland <- (final_value_lithuania_openland - initial_value_lithuania_openland) / (2023 - 2017)*100
print(rate_of_change_lithuania_openland)

czechia <- filter(data, country == "Czech Republic")
czechia2 <- filter(czechia, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.czechia <- czechia2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.czechia$woodland.tot)
sd(woodland.czechia$woodland.tot)
initial_value_czechia_woodland <- woodland.czechia$woodland.tot[woodland.czechia$year == 2017]
final_value_czechia_woodland <- woodland.czechia$woodland.tot[woodland.czechia$year == 2023]
rate_of_change_czechia_woodland <- (final_value_czechia_woodland - initial_value_czechia_woodland) / (2023 - 2017)*100
print(rate_of_change_czechia_woodland)

urban.czechia<- czechia2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.czechia$urban.tot)
sd(urban.czechia$urban.tot)
initial_value_czechia_urban <- urban.czechia$urban.tot[urban.czechia$year == 2017]
final_value_czechia_urban <- urban.czechia$urban.tot[urban.czechia$year == 2023]
rate_of_change_czechia_urban <- (final_value_czechia_urban - initial_value_czechia_urban) / (2023 - 2017)*100
print(rate_of_change_czechia_urban)

openland.czechia <- czechia2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.czechia$openland.tot)
sd(openland.czechia$openland.tot)
initial_value_czechia_openland <- openland.czechia$openland.tot[openland.czechia$year == 2017]
final_value_czechia_openland <- openland.czechia$openland.tot[openland.czechia$year == 2023]
rate_of_change_czechia_openland <- (final_value_czechia_openland - initial_value_czechia_openland) / (2023 - 2017)*100
print(rate_of_change_czechia_openland)

slovenia <- filter(data, country == "Slovenia")
slovenia2 <- filter(slovenia, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.slovenia <- slovenia2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.slovenia$woodland.tot)
sd(woodland.slovenia$woodland.tot)
initial_value_slovenia_woodland <- woodland.slovenia$woodland.tot[woodland.slovenia$year == 2017]
final_value_slovenia_woodland <- woodland.slovenia$woodland.tot[woodland.slovenia$year == 2023]
rate_of_change_slovenia_woodland <- (final_value_slovenia_woodland - initial_value_slovenia_woodland) / (2023 - 2017)*100
print(rate_of_change_slovenia_woodland)

urban.slovenia<- slovenia2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.slovenia$urban.tot)
sd(urban.slovenia$urban.tot)
initial_value_slovenia_urban <- urban.slovenia$urban.tot[urban.slovenia$year == 2017]
final_value_slovenia_urban <- urban.slovenia$urban.tot[urban.slovenia$year == 2023]
rate_of_change_slovenia_urban <- (final_value_slovenia_urban - initial_value_slovenia_urban) / (2023 - 2017)*100
print(rate_of_change_slovenia_urban)

openland.slovenia <- slovenia2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.slovenia$openland.tot)
sd(openland.slovenia$openland.tot)
initial_value_slovenia_openland <- openland.slovenia$openland.tot[openland.slovenia$year == 2017]
final_value_slovenia_openland <- openland.slovenia$openland.tot[openland.slovenia$year == 2023]
rate_of_change_slovenia_openland <- (final_value_slovenia_openland - initial_value_slovenia_openland) / (2023 - 2017)*100
print(rate_of_change_slovenia_openland)

switzerland <- filter(data, country == "Switzerland")
switzerland2 <- filter(switzerland, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.switzerland <- switzerland2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.switzerland$woodland.tot)
sd(woodland.switzerland$woodland.tot)
initial_value_switzerland_woodland <- woodland.switzerland$woodland.tot[woodland.switzerland$year == 2017]
final_value_switzerland_woodland <- woodland.switzerland$woodland.tot[woodland.switzerland$year == 2023]
rate_of_change_switzerland_woodland <- (final_value_switzerland_woodland - initial_value_switzerland_woodland) / (2023 - 2017)*100
print(rate_of_change_switzerland_woodland)

urban.switzerland<- switzerland2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.switzerland$urban.tot)
sd(urban.switzerland$urban.tot)
initial_value_switzerland_urban <- urban.switzerland$urban.tot[urban.switzerland$year == 2017]
final_value_switzerland_urban <- urban.switzerland$urban.tot[urban.switzerland$year == 2023]
rate_of_change_switzerland_urban <- (final_value_switzerland_urban - initial_value_switzerland_urban) / (2023 - 2017)*100
print(rate_of_change_switzerland_urban)

openland.switzerland <- switzerland2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.switzerland$openland.tot)
sd(openland.switzerland$openland.tot)
initial_value_switzerland_openland <- openland.switzerland$openland.tot[openland.switzerland$year == 2017]
final_value_switzerland_openland <- openland.switzerland$openland.tot[openland.switzerland$year == 2023]
rate_of_change_switzerland_openland <- (final_value_switzerland_openland - initial_value_switzerland_openland) / (2023 - 2017)*100
print(rate_of_change_switzerland_openland)

france <- filter(data, country == "France")
france2 <- filter(france, year != 2011 & year != 2012 & year != 2013 & year != 2014 & year != 2015 & year != 2016)
woodland.france <- france2 %>%
  group_by(year) %>%
  summarise(woodland.tot = mean(Trees_500, na.rm = TRUE))
mean(woodland.france$woodland.tot)
sd(woodland.france$woodland.tot)
initial_value_france_woodland <- woodland.france$woodland.tot[woodland.france$year == 2017]
final_value_france_woodland <- woodland.france$woodland.tot[woodland.france$year == 2023]
rate_of_change_france_woodland <- (final_value_france_woodland - initial_value_france_woodland) / (2023 - 2017)*100
print(rate_of_change_france_woodland)

urban.france<- france2 %>%
  group_by(year) %>%
  summarise(urban.tot = mean(Built_up_500, na.rm = TRUE))
mean(urban.france$urban.tot)
sd(urban.france$urban.tot)
initial_value_france_urban <- urban.france$urban.tot[urban.france$year == 2017]
final_value_france_urban <- urban.france$urban.tot[urban.france$year == 2023]
rate_of_change_france_urban <- (final_value_france_urban - initial_value_france_urban) / (2023 - 2017)*100
print(rate_of_change_france_urban)

openland.france <- france2 %>%
  group_by(year) %>%
  summarise(openland.tot = mean(Rangeland_500, na.rm = TRUE))
mean(openland.france$openland.tot)
sd(openland.france$openland.tot)
initial_value_france_openland <- openland.france$openland.tot[openland.france$year == 2017]
final_value_france_openland <- openland.france$openland.tot[openland.france$year == 2023]
rate_of_change_france_openland <- (final_value_france_openland - initial_value_france_openland) / (2023 - 2017)*100
print(rate_of_change_france_openland)




#COVARIATES TO INCLUDE: tree cover, rangeland cover, urban cover, dist box-road, mean alan, mean noise



##################################  1. NEST-BOX OCCUPANCY  ##############################


data.occ<- filter(data, Occupancy_Strix_uralensis_Slovenia != "yes" & 
                  occupancy_other_occupant != "yes" &
                  occupied != "other occupant" & 
                  occupied != "unknown" &
                  D_nearest_anyroad <= 500 & failure_type != 1 & failure_type != 4)



# Conversion type variable
data.occ$occupied <- as.factor(data.occ$occupied)

data.occ$year <- as.factor(data.occ$year)


# Check correlation between variables
data.occ.corr <- subset(data.occ, select=c('noise_mean','LP_mean','Rangeland_500', 'Trees_500', 'Built_up_500',
                                           'D_nearest_anyroad'))



correlation_result <- psych::corr.test(data.occ.corr, adjust="none", method = "pearson")
cor_matrix <- correlation_result$r 
corrplot(cor_matrix, method = "circle")
corrplot(cor_matrix, method = "color",    
         type = "upper",                  
         order = "hclust",               
         addCoef.col = "black",           
         tl.col = "black",                
         tl.srt = 45,                    
         diag = FALSE) 
cor.test(data.occ$Built_up_500, data.occ$LP_mean, method = "pearson")##
plot(data.occ$Built_up_500, data.occ$LP_mean)##




# Full model 
model.occ <- glmer(occupied ~ 
                     scale(Trees_500) +
                     scale(Rangeland_500) +
                     scale(Built_up_500) +
                     scale(D_nearest_anyroad)*scale(LP_mean) +
                     scale(D_nearest_anyroad)*scale(noise_mean) +
                     scale(LP_mean)*scale(noise_mean) +
                     (1|country/ID_CODE) +
                     (1|year),
                     control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                     family = binomial, 
                     data = data.occ)

model.occ_no_interaction1 <- glmer(occupied ~ 
                      scale(Trees_500) +
                      scale(Rangeland_500) +
                      scale(Built_up_500) +
                      scale(D_nearest_anyroad) +
                      scale(LP_mean) +
                      #scale(D_nearest_anyroad)*scale(LP_mean) +
                      scale(D_nearest_anyroad)*scale(noise_mean) +
                      scale(LP_mean)*scale(noise_mean) +
                      (1|country/ID_CODE) +
                      (1|year), 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                      family = binomial, 
                      data = data.occ)

model.occ_no_interaction2 <- glmer(occupied ~ 
                      scale(Trees_500) +
                      scale(Rangeland_500) +
                      scale(Built_up_500) +
                      scale(D_nearest_anyroad) +
                      scale(noise_mean) +
                      scale(D_nearest_anyroad)*scale(LP_mean) +
                      #scale(D_nearest_anyroad)*scale(noise_mean) +
                      scale(LP_mean)*scale(noise_mean) +
                      (1|country/ID_CODE) +
                      (1|year), 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                      family = binomial, 
                      data = data.occ)

model.occ_no_interaction3 <- glmer(occupied ~ 
                      scale(Trees_500) +
                      scale(Rangeland_500) +
                      scale(Built_up_500) +
                      scale(LP_mean) +
                      scale(noise_mean) +
                      scale(D_nearest_anyroad)*scale(LP_mean) +
                      scale(D_nearest_anyroad)*scale(noise_mean) +
                      #scale(LP_mean)*scale(noise_mean) +
                      (1|country/ID_CODE) +
                      (1|year), 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                      family = binomial, 
                      data = data.occ)


summary(model.occ)


anova1.occ <- anova(model.occ, model.occ_no_interaction1)
anova1.occ
anova2.occ <- anova(model.occ, model.occ_no_interaction2)
anova2.occ
anova3.occ <- anova(model.occ, model.occ_no_interaction3)
anova3.occ


write_xlsx(anova1.occ, "model.occ_no_interaction1.xlsx")

write_xlsx(anova2.occ, "model.occ_no_interaction2.xlsx")

write_xlsx(anova3.occ, "model.occ_no_interaction3.xlsx")


best.model.occ <- glmer(occupied ~ 
                            scale(Trees_500) +
                            scale(Rangeland_500) +
                            scale(Built_up_500) +
                            scale(D_nearest_anyroad) +
                            scale(LP_mean) +
                            scale(noise_mean) + 
                            (1|country/ID_CODE) +
                            (1|year), 
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                            family = binomial, 
                            data = data.occ)

summary(best.model.occ)


best.model.occ.r2_values <- r.squaredGLMM(best.model.occ)

print(best.model.occ.r2_values)


# Check multicollinearity
vif(best.model.occ) 


# Check spatial autocorrelation
data.occ$coords <- paste(data.occ$long,", ",data.occ$lat)
coords <- c(unique(data.occ$coords))
x_unique <- c(str_extract(coords, "^.+(?=,)"))
y_unique <- c(str_extract(coords, "(?<=, ).+$"))
sims<-simulateResiduals(best.model.occ)
simsrecalc<-recalculateResiduals(sims,group = data.occ$coords)
testSpatialAutocorrelation(simsrecalc, x = x_unique, y = y_unique)




plot.occ.1 <- ggpredict(model=best.model.occ, terms=c("noise_mean"))
plot(plot.occ.1) + 
  xlab("Traffic noise (dB)") + ylab("Nestbox occupancy (%)") + ggtitle("") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 1))


library(svglite)
ggsave("plot.occ.1.svg")


plot.occ.2 <- ggpredict(model=best.model.occ, terms=c("Trees_500"))
plot(plot.occ.2) + 
  xlab("Tree cover (%)") + ylab("Nestbox occupancy (%)") + ggtitle("") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 1))


ggsave("plot.occ.2.svg")



# Occupancy rate 

sum(data.occ$occupied==0 | data.occ$occupied==1)

(sum(data.occ$occupied==1)/sum(data.occ$occupied==0 | data.occ$occupied==1))*100




##################################  2. CLUTCH SIZE  ##############################


data.clutch<- filter(data, Occupancy_Strix_uralensis_Slovenia != "yes" & 
                      occupancy_other_occupant != "yes" &
                      occupied != "0" &
                      occupied != "other occupant" & 
                      occupied != "unknown" &
                      eggnumber != "unknown" &
                      eggnumber != "?" &
                      D_nearest_anyroad <= 500 & failure_type != 1 & failure_type != 2)




# Conversion type variable
data.clutch$year.f <- as.factor(data.clutch$year)

data.clutch$eggnumber <- as.numeric(data.clutch$eggnumber)


mean(data.clutch$eggnumber)

sd(data.clutch$eggnumber)



# Check correlation between variables
data.clutch.corr <- subset(data.clutch, select=c('noise_mean','LP_mean','Rangeland_500', 'Trees_500', 'Built_up_500',
                                           'D_nearest_anyroad'))
data.clutch.corr2 <- cor(data.clutch.corr)
corrplot(data.clutch.corr2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
pval <- psych::corr.test(data.clutch.corr2, adjust="none", method = "pearson")$p
corrplot(cor(data.clutch.corr2), type="upper", p.mat=pval, insig="p-value", 
         tl.pos="n", sig.level=0)
corrplot(cor(data.clutch.corr2), type="lower", add=T, tl.pos="d", cl.pos="n")




correlation_result <- psych::corr.test(data.clutch.corr, adjust="none", method = "pearson")
cor_matrix <- correlation_result$r 
corrplot(cor_matrix, method = "circle")
corrplot(cor_matrix, method = "color",    
         type = "upper",                  
         order = "hclust",               
         addCoef.col = "black",           
         tl.col = "black",                
         tl.srt = 45,                    
         diag = FALSE)                  



# Check distribution
hist(data.clutch$eggnumber)


# Full model
library(lmerTest)
library(languageR)

model.clutch <- lmer(eggnumber ~ 
                        scale(Trees_500) +
                        scale(Rangeland_500) +
                        scale(D_nearest_anyroad)*scale(LP_mean) +
                        scale(D_nearest_anyroad)*scale(noise_mean) +
                        scale(LP_mean)*scale(noise_mean) +
                        (1|country/ID_CODE) +
                        (1|year),
                        data = data.clutch)

model.clutch_no_interaction1 <- lmer(eggnumber ~ 
                                       scale(Trees_500) +
                                       scale(Rangeland_500) +
                                       scale(D_nearest_anyroad) +
                                       scale(LP_mean) +
                                       #scale(D_nearest_anyroad)*scale(LP_mean) +
                                       scale(D_nearest_anyroad)*scale(noise_mean) +
                                       scale(LP_mean)*scale(noise_mean) +
                                       (1|country/ID_CODE) +
                                       (1|year),
                                       data = data.clutch)

model.clutch_no_interaction2 <- lmer(eggnumber ~ 
                                       scale(Trees_500) +
                                       scale(Rangeland_500) +
                                       scale(D_nearest_anyroad) +
                                       scale(noise_mean) +
                                       scale(D_nearest_anyroad)*scale(LP_mean) +
                                       #scale(D_nearest_anyroad)*scale(noise_mean) +
                                       scale(LP_mean)*scale(noise_mean) +
                                       (1|country/ID_CODE) +
                                       (1|year),
                                       data = data.clutch)

model.clutch_no_interaction3 <- lmer(eggnumber ~ 
                                       scale(Trees_500) +
                                       scale(Rangeland_500) +
                                       scale(LP_mean) +
                                       scale(noise_mean) +
                                       scale(D_nearest_anyroad)*scale(LP_mean) +
                                       scale(D_nearest_anyroad)*scale(noise_mean) +
                                       #scale(LP_mean)*scale(noise_mean) +
                                       (1|country/ID_CODE) +
                                       (1|year),
                                       data = data.clutch)

summary(model.clutch)

anova1.clutch <- anova(model.clutch, model.clutch_no_interaction1)
anova1.clutch
anova2.clutch <- anova(model.clutch, model.clutch_no_interaction2)
anova2.clutch
anova3.clutch <- anova(model.clutch, model.clutch_no_interaction3)
anova3.clutch


write_xlsx(anova1.clutch, "model.clutch_no_interaction1.xlsx")

write_xlsx(anova2.clutch, "model.clutch_no_interaction2.xlsx")

write_xlsx(anova3.clutch, "model.clutch_no_interaction3.xlsx")



best.model.clutch <- lmer(eggnumber ~ 
                          scale(Trees_500) +
                          scale(Rangeland_500) +
                          scale(D_nearest_anyroad) +
                          scale(LP_mean) +
                          scale(noise_mean) + 
                          (1|country/ID_CODE) +
                          (1|year),  
                          data = data.clutch)

summary(best.model.clutch)



best.model.clutch.r2_values <- r.squaredGLMM(best.model.clutch)

print(best.model.clutch.r2_values)


# Check multicollinearity
vif(best.model.clutch) 


# Check spatial autocorrelation
data.clutch$coords <- paste(data.clutch$long,", ",data.clutch$lat)
coords <- c(unique(data.clutch$coords))
x_unique <- c(str_extract(coords, "^.+(?=,)"))
y_unique <- c(str_extract(coords, "(?<=, ).+$"))
sims<-simulateResiduals(best.model.clutch)
simsrecalc<-recalculateResiduals(sims,group = data.clutch$coords)
testSpatialAutocorrelation(simsrecalc, x = x_unique, y = y_unique)





##################################  3. BROOD SIZE  ##############################


data.brood<- filter(data, Occupancy_Strix_uralensis_Slovenia != "yes" & 
                    occupancy_other_occupant != "yes" &
                    occupied != "0" &
                    occupied != "other occupant" & 
                    occupied != "unknown" &
                    largeyoung != "unknown" & 
                    D_nearest_anyroad <= 500 & failure_type != 1)


# Conversion type variable
data.brood$year <- as.factor(data.brood$year)

data.brood$largeyoung <- as.numeric(data.brood$largeyoung)


mean(data.brood$largeyoung)

sd(data.brood$largeyoung)


# Check distribution
hist(data.brood$largeyoung)


# Check collinearity
data.brood.corr <- subset(data.brood, select=c('noise_mean','LP_mean','Rangeland_500', 'Trees_500', 'Built_up_500',
                                               'D_nearest_anyroad'))
data.brood.corr2 <- cor(data.brood.corr)
corrplot(data.brood.corr2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
pval <- psych::corr.test(data.brood.corr2, adjust="none", method = "pearson")$p
corrplot(cor(data.brood.corr2), type="upper", p.mat=pval, insig="p-value", 
         tl.pos="n", sig.level=0)
corrplot(cor(data.brood.corr2), type="lower", add=T, tl.pos="d", cl.pos="n")



correlation_result <- psych::corr.test(data.brood.corr, adjust="none", method = "pearson")
cor_matrix <- correlation_result$r 
corrplot(cor_matrix, method = "circle")
corrplot(cor_matrix, method = "color",    
         type = "upper",                  
         order = "hclust",               
         addCoef.col = "black",           
         tl.col = "black",                
         tl.srt = 45,                    
         diag = FALSE)    
cor.test(data.brood$Built_up_500, data.brood$LP_mean, method = "pearson")
plot(data.brood$Built_up_500, data.brood$LP_mean)


# Full model 
model.brood <- glmer(largeyoung ~ 
                     scale(Trees_500) +
                     scale(Rangeland_500) +
                     scale(D_nearest_anyroad)*scale(LP_mean) +
                     scale(D_nearest_anyroad)*scale(noise_mean) +
                     scale(LP_mean)*scale(noise_mean) +
                     (1|country/ID_CODE) +
                     (1|year), 
                     control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                     family = poisson, 
                     data = data.brood)

model.brood_no_interaction1 <- glmer(largeyoung ~ 
                        scale(Trees_500) +
                        scale(Rangeland_500) +
                        scale(D_nearest_anyroad) +
                        scale(LP_mean) +
                        #scale(D_nearest_anyroad)*scale(LP_mean) +
                        scale(D_nearest_anyroad)*scale(noise_mean) +
                        scale(LP_mean)*scale(noise_mean) +
                        (1|country/ID_CODE) +
                        (1|year), 
                        control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                        family = poisson, 
                        data = data.brood)

model.brood_no_interaction2 <- glmer(largeyoung ~ 
                                       scale(Trees_500) +
                                       scale(Rangeland_500) +
                                       scale(D_nearest_anyroad) +
                                       scale(noise_mean) +
                                       scale(D_nearest_anyroad)*scale(LP_mean) +
                                       #scale(D_nearest_anyroad)*scale(noise_mean) +
                                       scale(LP_mean)*scale(noise_mean) +
                                       (1|country/ID_CODE) +
                                       (1|year), 
                                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                                       family = poisson, 
                                       data = data.brood)

model.brood_no_interaction3 <- glmer(largeyoung ~ 
                                       scale(Trees_500) +
                                       scale(Rangeland_500) +
                                       scale(LP_mean) +
                                       scale(noise_mean) +
                                       scale(D_nearest_anyroad)*scale(LP_mean) +
                                       scale(D_nearest_anyroad)*scale(noise_mean) +
                                       #scale(LP_mean)*scale(noise_mean) +
                                       (1|country/ID_CODE) +
                                       (1|year), 
                                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                                       family = poisson, 
                                       data = data.brood)


summary(model.brood)

anova1.brood <- anova(model.brood, model.brood_no_interaction1)
anova1.brood
anova2.brood <- anova(model.brood, model.brood_no_interaction2)
anova2.brood
anova3.brood <- anova(model.brood, model.brood_no_interaction3)
anova3.brood


write_xlsx(anova1.brood, "model.brood_no_interaction1.xlsx")

write_xlsx(anova2.brood, "model.brood_no_interaction2.xlsx")

write_xlsx(anova3.brood, "model.brood_no_interaction3.xlsx")


best.model.brood <- glmer(largeyoung ~ 
                            scale(Trees_500) +
                            scale(Rangeland_500) +
                            scale(noise_mean) +
                            scale(D_nearest_anyroad)*scale(LP_mean) + 
                            (1|country/ID_CODE) +
                            (1|year), 
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                            family = poisson, 
                            data = data.brood)

summary(best.model.brood)


best.model.brood.r2_values <- r.squaredGLMM(best.model.brood)

print(best.model.brood.r2_values)


# Check multicollinearity
vif(best.model.brood) 


#Check spatial autocorrelation
data.brood$coords <- paste(data.brood$long,", ",data.brood$lat)
coords <- c(unique(data.brood$coords))
x_unique <- c(str_extract(coords, "^.+(?=,)"))
y_unique <- c(str_extract(coords, "(?<=, ).+$"))
sims<-simulateResiduals(best.model.brood)
simsrecalc<-recalculateResiduals(sims,group = data.brood$coords)
testSpatialAutocorrelation(simsrecalc, x = x_unique, y = y_unique)



plot.brood.1 <- ggpredict(model=best.model.brood, terms=c("noise_mean"))
plot(plot.brood.1) + 
  xlab("Traffic noise (dB)") + ylab("Brood size") + ggtitle("") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 6))

ggsave("plot.brood.noise.svg")


library(interactions)

interact.plot.brood <- interact_plot(best.model.brood, 
                               pred = LP_mean, 
                               modx = D_nearest_anyroad, 
                               modx.values = "plus-minus", 
                               interval = TRUE,
                               int.type = "confidence", 
                               int.width = 0.95,
                               vary.lty = F,
                               y.label = "Brood size",
                               x.label = "ALAN (W·sr−1·m−2)",
                               colors = "Qual1",
                               legend.main = "Distance nestbox-road")

interact.plot.brood + theme_bw() + 
  theme(axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggsave("interact.plot.brood.svg")



##################################  5. PROP HATCHING  ##############################



data.hatch<- filter(data, Occupancy_Strix_uralensis_Slovenia != "yes" & 
                                   occupancy_other_occupant != "yes" &
                                   occupied != "0" &
                                   occupied != "other occupant" & 
                                   occupied != "unknown" &
                                   largeyoung != "unknown" &
                                   eggnumber != "unknown" &
                                   eggs_not_hatched != "#VALUE!" &
                                   D_nearest_anyroad <= 500 & failure_type != 1)


# Conversion type variable
data.hatch$year <- as.factor(data.hatch$year)

data.hatch$eggnumber <- as.numeric(data.hatch$eggnumber)

data.hatch$eggs_not_hatched <- as.numeric(data.hatch$eggs_not_hatched)

data.hatch$largeyoung <- as.numeric(data.hatch$largeyoung)


# Check collinearity
data.hatch.corr <- subset(data.hatch, select=c('noise_mean','LP_mean','Rangeland_500', 'Trees_500', 'Built_up_500',
                                               'D_nearest_anyroad'))
data.hatch.corr2 <- cor(data.hatch.corr)
corrplot(data.hatch.corr2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
pval <- psych::corr.test(data.hatch.corr2, adjust="none", method = "pearson")$p
corrplot(cor(data.hatch.corr2), type="upper", p.mat=pval, insig="p-value", 
         tl.pos="n", sig.level=0)
corrplot(cor(data.hatch.corr2), type="lower", add=T, tl.pos="d", cl.pos="n")




correlation_result <- psych::corr.test(data.hatch.corr, adjust="none", method = "pearson")
cor_matrix <- correlation_result$r 
corrplot(cor_matrix, method = "circle")
corrplot(cor_matrix, method = "color",    
         type = "upper",                  
         order = "hclust",               
         addCoef.col = "black",           
         tl.col = "black",                
         tl.srt = 45,                    
         diag = FALSE)    




# Full model 
model.hatch <- glmer(cbind(data.hatch$largeyoung, data.hatch$eggs_not_hatched) ~ 
                      scale(Trees_500) +
                      scale(Rangeland_500) +
                      scale(D_nearest_anyroad)*scale(LP_mean) +
                      scale(D_nearest_anyroad)*scale(noise_mean) +
                      scale(LP_mean)*scale(noise_mean) +
                      (1|country/ID_CODE) +
                      (1|year),
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                      family = binomial, 
                      data = data.hatch)

model.hatch_no_interaction1 <- glmer(cbind(data.hatch$largeyoung, data.hatch$eggs_not_hatched) ~ 
                       scale(Trees_500) +
                       scale(Rangeland_500) +
                       scale(D_nearest_anyroad) +
                       scale(LP_mean) +
                       #scale(D_nearest_anyroad)*scale(LP_mean) +
                       scale(D_nearest_anyroad)*scale(noise_mean) +
                       scale(LP_mean)*scale(noise_mean) +
                       (1|country/ID_CODE) +
                       (1|year),
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                       family = binomial, 
                       data = data.hatch)

model.hatch_no_interaction2 <- glmer(cbind(data.hatch$largeyoung, data.hatch$eggs_not_hatched) ~ 
                                       scale(Trees_500) +
                                       scale(Rangeland_500) +
                                       scale(D_nearest_anyroad) +
                                       scale(noise_mean) +
                                       scale(D_nearest_anyroad)*scale(LP_mean) +
                                       #scale(D_nearest_anyroad)*scale(noise_mean) +
                                       scale(LP_mean)*scale(noise_mean) +
                                       (1|country/ID_CODE) +
                                       (1|year),
                                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                                       family = binomial, 
                                       data = data.hatch)

model.hatch_no_interaction3 <- glmer(cbind(data.hatch$largeyoung, data.hatch$eggs_not_hatched) ~ 
                                       scale(Trees_500) +
                                       scale(Rangeland_500) +
                                       scale(LP_mean) +
                                       scale(noise_mean) +
                                       scale(D_nearest_anyroad)*scale(LP_mean) +
                                       scale(D_nearest_anyroad)*scale(noise_mean) +
                                       #scale(LP_mean)*scale(noise_mean) +
                                       (1|country/ID_CODE) +
                                       (1|year),
                                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                                       family = binomial, 
                                       data = data.hatch)

summary(model.hatch)

anova1.hatch <- anova(model.hatch, model.hatch_no_interaction1)
anova1.hatch
anova2.hatch <- anova(model.hatch, model.hatch_no_interaction2)
anova2.hatch
anova3.hatch <- anova(model.hatch, model.hatch_no_interaction3)
anova3.hatch


write_xlsx(anova1.hatch, "model.hatch_no_interaction1.xlsx")

write_xlsx(anova2.hatch, "model.hatch_no_interaction2.xlsx")

write_xlsx(anova3.hatch, "model.hatch_no_interaction3.xlsx")



best.model.hatch <- glmer(cbind(data.hatch$largeyoung, data.hatch$eggs_not_hatched) ~ 
                            scale(Trees_500) +
                            scale(Rangeland_500) +
                            scale(noise_mean) +
                            scale(D_nearest_anyroad)*scale(LP_mean) + 
                            (1|country/ID_CODE) +
                            (1|year), 
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                            family = binomial, 
                            data = data.hatch)

summary(best.model.hatch)

best.model.hatch.r2_values <- r.squaredGLMM(best.model.hatch)

print(best.model.hatch.r2_values)


# Check multicollinearity
vif(best.model.hatch)


#Check spatial autocorrelation
data.hatch$coords <- paste(data.hatch$long,", ",data.hatch$lat)
coords <- c(unique(data.hatch$coords))
x_unique <- c(str_extract(coords, "^.+(?=,)"))
y_unique <- c(str_extract(coords, "(?<=, ).+$"))
sims<-simulateResiduals(best.model.hatch)
simsrecalc<-recalculateResiduals(sims,group = data.hatch$coords)
testSpatialAutocorrelation(simsrecalc, x = x_unique, y = y_unique)



plot.hatch.1 <- ggpredict(model=best.model.hatch, terms=c("noise_mean"))
plot(plot.hatch.1) + 
  xlab("Traffic noise (dB)") + ylab("Hatched eggs") + ggtitle("") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 1))

ggsave("plot.hatch.1.svg")


library(interactions)
interact.plot.hatch.noise <- interact_plot(best.model.hatch, 
                                     pred = noise_mean, 
                                     modx = LP_mean, 
                                     modx.values = "plus-minus", 
                                     interval = TRUE,
                                     int.type = "confidence", 
                                     int.width = 0.95,
                                     vary.lty = F,
                                     y.label = "Hatched eggs",
                                     x.label = "Traffic noise (dB)",
                                     colors = "Qual1",
                                     modx.labels = c(""))

interact.plot.hatch.noise + theme_bw() + 
  theme(axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(limits = c(0, 1))



ggsave("plot.hatch.noise.svg")


library(interactions)
interact.plot.hatch <- interact_plot(best.model.hatch, 
              pred = LP_mean, 
              modx = D_nearest_anyroad, 
              modx.values = "plus-minus", 
              interval = TRUE,
              int.type = "confidence", 
              int.width = 0.95,
              vary.lty = F,
              y.label = "Hatched eggs",
              x.label = "ALAN (W·sr−1·m−2)",
              colors = "Qual1",
              legend.main = "Distance nestbox-road (m)")

interact.plot.hatch + theme_bw() + 
                theme(axis.title.x = element_text(color = "black"),
                      axis.title.y = element_text(color = "black"),
                      legend.title = element_text(color = "black"),
                      legend.text = element_text(color = "black"),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank())



ggsave("interact.plot.hatch.svg")



# Hatching rate 

sum(data.hatch$hatched==0 | data.hatch$hatched==1)

(sum(data.hatch$hatched==1)/sum(data.hatch$hatched==0 | data.hatch$hatched==1))*100




##################################  5. FLEDGLING SUCCESS  ##############################



data.fled<- filter(data, Occupancy_Strix_uralensis_Slovenia != "yes" & 
                      occupancy_other_occupant != "yes" &
                      occupied != "0" &
                      occupied != "other occupant" & 
                      occupied != "unknown" &
                      fledgedyoung != "unknown" &
                      largeyoung != "unknown" &
                      eggs_not_hatched != "VALUE!" &
                      young_not_fledged != "#VALUE!" &
                      eggnumber != "unknown" & eggnumber != "?" &
                      D_nearest_anyroad <= 500 & failure_type != 5)



# Conversion type variable
data.fled$year <- as.factor(data.fled$year)

data.fled$fledgedyoung <- as.numeric(data.fled$fledgedyoung)

data.fled$young_not_fledged <- as.numeric(data.fled$young_not_fledged)

data.fled$largeyoung <- as.numeric(data.fled$largeyoung)

data.fled$eggnumber <- as.numeric(data.fled$eggnumber)

data.fled$eggs_not_hatched <- as.numeric(data.fled$eggs_not_hatched)


mean(data.fled$fledgedyoung)

sd(data.fled$fledgedyoung)


# Check collinearity
data.fled.corr <- subset(data.fled, select=c('noise_mean','LP_mean','Rangeland_500', 'Trees_500', 'Built_up_500',
                                               'D_nearest_anyroad'))
data.fled.corr2 <- cor(data.fled.corr)
corrplot(data.fled.corr2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
pval <- psych::corr.test(data.fled.corr2, adjust="none", method = "pearson")$p
corrplot(cor(data.fled.corr2), type="upper", p.mat=pval, insig="p-value", 
         tl.pos="n", sig.level=0)
corrplot(cor(data.fled.corr2), type="lower", add=T, tl.pos="d", cl.pos="n")




correlation_result <- psych::corr.test(data.fled.corr, adjust="none", method = "pearson")
cor_matrix <- correlation_result$r 
corrplot(cor_matrix, method = "circle")
corrplot(cor_matrix, method = "color",    
         type = "upper",                  
         order = "hclust",               
         addCoef.col = "black",           
         tl.col = "black",                
         tl.srt = 45,                    
         diag = FALSE)    



# Full model 
model.fled <- glmer(fledgedyoung ~ 
                       scale(Trees_500) +
                       scale(Rangeland_500) +
                       scale(D_nearest_anyroad)*scale(LP_mean) +
                       scale(D_nearest_anyroad)*scale(noise_mean) +
                       scale(LP_mean)*scale(noise_mean) +
                       (1|country/ID_CODE) +
                       (1|year), 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                       family = poisson, 
                       data = data.fled)

model.fled_no_interaction1 <- glmer(fledgedyoung ~ 
                      scale(Trees_500) +
                      scale(Rangeland_500) +
                      scale(D_nearest_anyroad) +
                      scale(LP_mean) +
                      #scale(D_nearest_anyroad)*scale(LP_mean) +
                      scale(D_nearest_anyroad)*scale(noise_mean) +
                      scale(LP_mean)*scale(noise_mean) +
                      (1|country/ID_CODE) +
                      (1|year), 
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                      family = poisson, 
                      data = data.fled)

model.fled_no_interaction2 <- glmer(fledgedyoung ~ 
                                      scale(Trees_500) +
                                      scale(Rangeland_500) +
                                      scale(D_nearest_anyroad) +
                                      scale(noise_mean) +
                                      scale(D_nearest_anyroad)*scale(LP_mean) +
                                      #scale(D_nearest_anyroad)*scale(noise_mean) +
                                      scale(LP_mean)*scale(noise_mean) +
                                      (1|country/ID_CODE) +
                                      (1|year), 
                                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                                      family = poisson, 
                                      data = data.fled)

model.fled_no_interaction3 <- glmer(fledgedyoung ~ 
                                      scale(Trees_500) +
                                      scale(Rangeland_500) +
                                      scale(LP_mean) +
                                      scale(noise_mean) +
                                      scale(D_nearest_anyroad)*scale(LP_mean) +
                                      scale(D_nearest_anyroad)*scale(noise_mean) +
                                      #scale(LP_mean)*scale(noise_mean) +
                                      (1|country/ID_CODE) +
                                      (1|year), 
                                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                                      family = poisson, 
                                      data = data.fled)

summary(model.fled)


anova1.fled <- anova(model.fled, model.fled_no_interaction1)
anova1.fled
anova2.fled <- anova(model.fled, model.fled_no_interaction2)
anova2.fled
anova3.fled <- anova(model.fled, model.fled_no_interaction3)
anova3.fled


write_xlsx(anova1.fled, "model.fled_no_interaction1.xlsx")

write_xlsx(anova2.fled, "model.fled_no_interaction2.xlsx")

write_xlsx(anova3.fled, "model.fled_no_interaction3.xlsx")



best.model.fled <- glmer(fledgedyoung ~ 
                            scale(Trees_500) +
                            scale(Rangeland_500) +
                            scale(noise_mean) +
                            scale(D_nearest_anyroad)*scale(LP_mean) + 
                            (1|country/ID_CODE) +
                            (1|year), 
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                            family = poisson, 
                            data = data.fled)

summary(best.model.fled)


best.model.fled.r2_values <- r.squaredGLMM(best.model.fled)

print(best.model.fled.r2_values)


# Check multicollinearity
vif(best.model.fled)


#Check spatial autocorrelation
data.fled$coords <- paste(data.fled$long,", ",data.fled$lat)
coords <- c(unique(data.fled$coords))
x_unique <- c(str_extract(coords, "^.+(?=,)"))
y_unique <- c(str_extract(coords, "(?<=, ).+$"))
sims<-simulateResiduals(best.model.fled)
simsrecalc<-recalculateResiduals(sims,group = data.fled$coords)
testSpatialAutocorrelation(simsrecalc, x = x_unique, y = y_unique)



plot.fled.1 <- ggpredict(model=best.model.fled, terms=c("noise_mean"))
plot(plot.fled.1) + 
  xlab("Traffic noise (dB)") + ylab("Number of fledglings") + ggtitle("") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(limits = c(0, 7))


ggsave("plot.fled.noise.svg")


interact.plot.fled <- interact_plot(best.model.fled, 
                                     pred = LP_mean, 
                                     modx = D_nearest_anyroad, 
                                     modx.values = "plus-minus", 
                                     interval = TRUE,
                                     int.type = "confidence", 
                                     int.width = 0.95,
                                     vary.lty = F,
                                     y.label = "Fledged young",
                                     x.label = "ALAN (W·sr−1·m−2)",
                                     colors = "Qual1",
                                     legend.main = "Distance nestbox-road")

interact.plot.fled + theme_bw() + 
  theme(axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



ggsave("interact.plot.fled.svg")


## SEPARATE BREEDING ESTIMATES PER COUNTRY ##

# Clutch size

scotland <- filter(data.clutch, country == "Scotland")
mean(scotland$eggnumber)
sd(scotland$eggnumber)

norway <- filter(data.clutch, country == "Norway")
mean(norway$eggnumber)
sd(norway$eggnumber)

sweden <- filter(data.clutch, country == "Sweden")
mean(sweden$eggnumber)
sd(sweden$eggnumber)

finland <- filter(data.clutch, country == "Finland")
mean(finland$eggnumber)
sd(finland$eggnumber)

lithuania <- filter(data.clutch, country == "Lithuania")
mean(lithuania$eggnumber)
sd(lithuania$eggnumber)

czechia <- filter(data.clutch, country == "Czech Republic")
mean(czechia$eggnumber)
sd(czechia$eggnumber)

slovenia <- filter(data.clutch, country == "Slovenia")
mean(slovenia$eggnumber)
sd(slovenia$eggnumber)

switzerland <- filter(data.clutch, country == "Switzerland")
mean(switzerland$eggnumber)
sd(switzerland$eggnumber)

france <- filter(data.clutch, country == "France")
mean(france$eggnumber)
sd(france$eggnumber)


# Hatching success

scotland2 <- filter(data.hatch, country == "Scotland")
sum(scotland2$hatched==0 | scotland2$hatched==1)
(sum(scotland2$hatched==1)/sum(scotland2$hatched==0 | scotland2$hatched==1))*100

norway2 <- filter(data.hatch, country == "Norway")
sum(norway2$hatched==0 | norway2$hatched==1)
(sum(norway2$hatched==1)/sum(norway2$hatched==0 | norway2$hatched==1))*100

sweden2 <- filter(data.hatch, country == "Sweden")
sum(sweden2$hatched==0 | sweden2$hatched==1)
(sum(sweden2$hatched==1)/sum(sweden2$hatched==0 | sweden2$hatched==1))*100

finland2 <- filter(data.hatch, country == "Finland")
sum(finland2$hatched==0 | finland2$hatched==1)
(sum(finland2$hatched==1)/sum(finland2$hatched==0 | finland2$hatched==1))*100

lithuania2 <- filter(data.hatch, country == "Lithuania")
sum(lithuania2$hatched==0 | lithuania2$hatched==1)
(sum(lithuania2$hatched==1)/sum(lithuania2$hatched==0 | lithuania2$hatched==1))*100

czechia2 <- filter(data.hatch, country == "Czech Republic")
sum(czechia2$hatched==0 | czechia2$hatched==1)
(sum(czechia2$hatched==1)/sum(czechia2$hatched==0 | czechia2$hatched==1))*100

slovenia2 <- filter(data.hatch, country == "Slovenia")
sum(slovenia2$hatched==0 | slovenia2$hatched==1)
(sum(slovenia2$hatched==1)/sum(slovenia2$hatched==0 | slovenia2$hatched==1))*100

switzerland2 <- filter(data.hatch, country == "Switzerland")
sum(switzerland2$hatched==0 | switzerland2$hatched==1)
(sum(switzerland2$hatched==1)/sum(switzerland2$hatched==0 | switzerland2$hatched==1))*100

france2 <- filter(data.hatch, country == "France")
sum(france2$hatched==0 | france2$hatched==1)
(sum(france2$hatched==1)/sum(france2$hatched==0 | france2$hatched==1))*100


# Brood size

scotland3 <- filter(data.brood, country == "Scotland")
mean(scotland3$largeyoung)
sd(scotland3$largeyoung)

norway3 <- filter(data.brood, country == "Norway")
mean(norway3$largeyoung)
sd(norway3$largeyoung)

sweden3 <- filter(data.brood, country == "Sweden")
mean(sweden3$largeyoung)
sd(sweden3$largeyoung)

finland3 <- filter(data.brood, country == "Finland")
mean(finland3$largeyoung)
sd(finland3$largeyoung)

lithuania3 <- filter(data.brood, country == "Lithuania")
mean(lithuania3$largeyoung)
sd(lithuania3$largeyoung)

czechia3 <- filter(data.brood, country == "Czech Republic")
mean(czechia3$largeyoung)
sd(czechia3$largeyoung)

slovenia3 <- filter(data.brood, country == "Slovenia")
mean(slovenia3$largeyoung)
sd(slovenia3$largeyoung)

switzerland3 <- filter(data.brood, country == "Switzerland")
mean(switzerland3$largeyoung)
sd(switzerland3$largeyoung)

france3 <- filter(data.brood, country == "France")
mean(france3$largeyoung)
sd(france3$largeyoung)



