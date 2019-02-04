library(tidyverse)
library(magrittr)
library(lmerTest)
setwd("C:/Users/Md Farhad/OneDrive/Rdir")

#reading data and converting into columns into factor

gs <- read.csv("combine_all_latest.csv")
gs
head(gs)
gs$Plot <-as.factor(gs$Plot)
gs$Rep <- as.factor(gs$Rep)
gs$Entry <-as.factor(gs$Entry)
gs$SubBlock <- as.factor(gs$SubBlock)
gs$Loc <- as.character(gs$Loc)
gs$Booting <- as.numeric(gs$Booting)
gs$Heading <- as.numeric(gs$Heading)
gs$Maturity <- as.numeric(gs$Maturity)
gs$Trial <- as.factor(gs$Trial)

##filter first location data
gs %>% 
  filter(Loc == "BMZLDH_2017")

##determine how to model the data, examine replication and incomplete block levels
gs %>% 
  group_by(Loc) %>% 
  summarize(Nobs = n(),
            Ngen = length(unique(Entry)),
            Nrep = length(unique(Rep)),
            Nblk = length(unique(SubBlock)),
            Ntrl=length(unique(Trial)))
#select only first location and one trait
gs %>%   
  filter(Loc == "BMZLDH_2017") %>%
  select(Rep,Entry,SubBlock,Trial, GrainYield)

##look for shape of our data. Let’s check the variability and distribution of the trait across the locations
boxplot(GrainYield~Loc,data = gs)
##checking for outliers
gs %>%
  filter(Loc == "BMZLDH_2017", GrainYield < 5)
##removing the unexoected data
badYieldObs  <-  which(gs$Loc == "BMZLDH_2017" & gs$Plot == 692)

gs$GrainYield[badYieldObs][1] <- NA

gs %>%
  filter(Loc == "BMZLDH_2017", Entry == 6028)


#analyze each Trait-Location
trials <- gs %>% 
  gather(Trait,Value,Booting:GrainYield) %>% 
  group_by(Trait,Loc) %>% 
  nest(.key = TrialData)

trials$TrialData[[1]] %>% head


trials %<>%
  mutate(Nobs = map_dbl(TrialData,~nrow(.)),
         Ngen = map_dbl(TrialData,~length(unique(.$Entry))),
         Nrep = map_dbl(TrialData,~length(unique(.$Rep))))

trials %>% head

####fiiting mixed model

trials %<>% 
  mutate(DataModel = "Value ~ Rep+(1|Rep:SubBlock)+Entry")

trials %<>% 
  mutate(lmerModel = pmap(.,function(TrialData,DataModel,...){
    form <- as.formula(DataModel)
    out <- lmer(formula = form, data = TrialData)
    return(out)}))

# extracting ANOVA 
extract_anova <- function(lmer_out){
  aov_fixed <- anova(lmer_out)
  return(aov_fixed)
}

anovatable <- trials %>% 
  mutate(anovaF = map(lmerModel, extract_anova)) %>% 
  select(Trait,Loc,anovaF)

# extracting BLUE
extract_BLUE <- function(lmer_out){
  output <- lsmeansLT(lmer_out,test.effs = "Gen")
  out <- tibble(Geno = rownames(output),BLUE = output$Estimate) 
  return(out)
}

(meantable <- trials %>% 
    mutate(blues = map(lmerModel,extract_BLUE)) %>% 
    select(Trait,Loc,blues) %>%
    unnest())


