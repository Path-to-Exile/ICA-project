library("MASS")
library("ggplot2")
library("gridExtra")
library("gtable")
library("grid")
library("latex2exp")


#setwd("/Users/some_folder") # Folder with Jupyter notebook data
df = read.csv("r-pipeline-v2.csv")
head(df)

#########  data for beat_pct and mean table
beat_pct = function(x) { # Function calculating percentage of time one ICA outperform the other
  score = rep(NA,length(x))
  for (i in 1:length(x)) {
    score[i] = ifelse(x[i] >= 0, 1, 0)
  }
  score = mean(score)
  return(score)
}
fast_sobi = df[df$X %% 4 == 0,]$Accuracy-df[df$X %% 4 == 3,]$Accuracy
fast_choi = df[df$X %% 4 == 0,]$Accuracy-df[df$X %% 4 == 2,]$Accuracy
fast_coro = df[df$X %% 4 == 0,]$Accuracy-df[df$X %% 4 == 1,]$Accuracy
sobi_choi = df[df$X %% 4 == 2,]$Accuracy-df[df$X %% 4 == 3,]$Accuracy
sobi_coro = df[df$X %% 4 == 1,]$Accuracy-df[df$X %% 4 == 3,]$Accuracy
choi_coro = df[df$X %% 4 == 1,]$Accuracy-df[df$X %% 4 == 2,]$Accuracy
mean(fast_sobi)
mean(fast_choi)
mean(fast_coro)
mean(sobi_choi)
mean(sobi_coro)
mean(choi_coro)
beat_pct(fast_sobi)
beat_pct(fast_choi)
beat_pct(fast_coro)
beat_pct(sobi_choi)
beat_pct(sobi_coro)
beat_pct(choi_coro)
xtable(aggregate(Accuracy ~ Pipeline, df, mean), digits = 3)

#########  Obtaining correct dataframe
band = rep(c(rep('alpha+beta',80),rep('beta',80)),4)
CAR = rep(c(rep(1,160),rep(0,160)),2)
clf = c(c(rep('QDA',320),rep('RF',320)))
df$CAR = CAR
df$clf = clf
df$band = band
head(df)

glm_full = glm(Accuracy ~ band * clf * ICA_method, data = df)
summary(glm_full)
######### USED IN PAPER
glm_add = glm(Accuracy ~ band + clf + ICA_method + CAR, data = df)
summary(glm_add)
######### 

#########  Soft model testing, checking for sanity
stepAIC(glm(Accuracy ~ 1, data = df), 
        scope = list(upper = ~band * clf * ICA_method, lower = ~1),
        direction = "forward")
stepAIC(glm_full, 
        scope = list(upper = ~band * clf * ICA_method * CAR, lower = ~1),
        direction = "backward")

glm_add = glm(Accuracy ~ band + clf + ICA_method + CAR, data = df)
summary(glm_add)

#########  Soft diagnostics for sanity, although not espcially usefull 
plot(glm_add)
hist(df$Accuracy, freq = FALSE, breaks = 20)
ggplot(df, aes(x=Accuracy)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  stat_function(fun = dnorm, n = 101, colour = "red", size = 1, args = list(mean = mean(df$Accuracy), sd = sd(df$Accuracy))) + 
  ylab("") +
  scale_y_continuous(breaks = NULL) 
  #+facet_grid(. ~ ICA_method)
  

######### Comparison to wide filter
df2 = read.csv("bands_collected")
head(df2)
band = rep('alpha/beta',120)
CAR = rep(c(rep(1,60),rep(0,60)),1)
clf = rep('QDA',120)
df2$CAR = CAR
df2$clf = clf
df2$band = band
head(df2)
df3 = rbind(df,df2)
df3 = df3[df3$ICA_method != 'SOBI',]
mean(df3[df3$band == 'alpha/beta',]$Accuracy)-mean(df3[df3$band == 'alpha+beta',]$Accuracy)


######### Table for Train Size experiment
eeg = read.csv("big-analysis.csv")
aggregate(Accuracy ~ Train_Size + ICA_method, eeg, mean, simplify = TRUE)
aggregate(Accuracy ~ Train_Size, eeg, mean, simplify = TRUE)



