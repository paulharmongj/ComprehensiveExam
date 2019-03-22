## Data Analysis Code

## Requires the relevant packages
library(ggplot2);library(dplyr);library(magrittr);library(readr)
library(tibble);library(corrplot);library(readxl)

## Reads in the data
bal <- read_excel('Data/LiangData.xlsx', sheet = "Liang_etal")
head(bal)

#nice to see these guys are real data gurus. What an awful format

#goal: modify the data for AOS to be prepped for analysis
bal_area <- bal[,1:8] #pulls the first 8 columns before column 9 (the empty one)
column_names <- bal_area[1,]
bal_tibble <- as_tibble(bal_area[-1,]);names(bal_tibble) <- as.character(column_names)
dim(bal_tibble)
bal_tibble$Participant <- 1:30
#the above code outputs a dataset with 30 rows and 8 measurements

#still a weird format - we would prefer to have 3 columns: Sport, Eyes, Position
eyenames <- grep('C',column_names) #want C because O would be conflated with OL
#recall that we don't have a column for open toes here so only 7 columns instead of 8
bal_tibble_open <- select(bal_tibble, -eyenames)
bal_tibble_closed <- select(bal_tibble, c(eyenames,Sport,Participant))

#adds a feature to the datset for eyes open or closed
bal_tibble_open$Eyes <- rep("Open", nrow(bal_tibble_open))
bal_tibble_closed$Eyes <- rep("Closed", nrow(bal_tibble_closed))


#these will be our position variables to append to the 'long'-form dataset
open_position <- rep(c('Q','OL','OL_F','T'), each = nrow(bal_tibble_open))
closed_position <- rep(c('Q','OL','OL_F'), each = nrow(bal_tibble_closed))

#now we want to actually do the converstion on bal_tibbles to long form
open_long <- bal_tibble_open %>% gather(key = Position, value = Y,OQ,OOL, OOL_F,OT)
closed_long <- bal_tibble_closed %>% gather(key = Position, value = Y, CQ,COL,COL_F)
open_long$POS <- open_position
closed_long$POS <- closed_position

#Whew! Now we put it all together by rbinding the two tibbles
full_dat <- rbind(open_long, closed_long)
full_dat$Y <- as.numeric(as.character(full_dat$Y))

fdat <- full_dat %>% filter(!POS %in% 'T') 
filter_dat <- fdat %>% select(-Y) %>% lapply(as.factor)%>% as_tibble()
filter_dat$Y <- fdat$Y

#Make a plot that resembles their Figure 2

means <- filter_dat %>% group_by(Sport,Eyes,POS) %>% summarise(mean(Y))
names(means)[4] <- 'Agmean'
means$POS <- factor(means$POS, levels = c("Q","OL","OL_F"))

sd <- filter_dat %>% group_by(Sport, Eyes, POS) %>% summarise(sd(Y))
names(sd)[4] <- "SD"
means$upper <- means$Agmean + sd$SD/sqrt(3) #since xbar has sd sqrt(sigma^2/n)
means$lower <- means$Agmean -sd$SD/sqrt(3) #same

means$Sport <- factor(means$Sport, levels = c("Baseball",'Soccer','Novice'))
#for eyes open

#actually creates the plot (with ribbons instead of bands for uncertainty)
O <- ggplot(filter(means,Eyes == 'Open'), aes(POS,Agmean,group = Sport)) +
  geom_line(aes(color = as.numeric(Sport)), size = 3) + ylab("Mean Area (mm)") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.numeric(Sport)), alpha = .2)+ 
  theme_bw() +theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Eyes Open")
#for eyes closed

C <- ggplot(filter(means, Eyes == 'Closed'), aes(POS, Agmean, group = Sport))+
  geom_line(aes(color = as.numeric(Sport)), size = 3) + ylab("Mean Area (mm)") + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.numeric(Sport)), alpha = .2)+ 
  theme_bw() +theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Eyes Closed")
  
 
# alternate solution but this method screws up the labels, better to re-level factors 
#scale_color_discrete(name = "Sport", labels = c('Baseball','Soccer','Novice'))
  



#create the plot with ggarrange
library(ggpubr)
ggarrange(O,C)


## Could always just grab Mark's code from before



## Replication of the ANOVA
#this fits the cell-means version
aov1 <- lm(Y ~ POS * Eyes *Sport, data = filter_dat)
summary(aov1)
anova(aov1)

#The above code matches their output OK! 

# Were the F-tests they reported based on Type I, Type II, or Type III tests? 
library(car)
Anova(aov1, type = "II")
Anova(aov1, type = "III")

par(mfrow = c(2,2))
plot(aov1, pch = 20, col = "blue3")
# not sure about their Shapiro-Wilks test 
shapiro.test(filter_dat$Y)
leveneTest(filter_dat$Y, group = interaction(filter_dat$Sport,filter_dat$Eyes,filter_dat$POS))
 # I suspect they grouped by Sport when they ran this because that's the only non-significant result


## A more appropriate model
#
library(lme4);library(lmerTest)
mm1 <- lmer(Y ~ POS * Eyes *Sport + (1|Participant), data = filter_dat)
anova(mm1)

summary(mm1)
qqnorm(resid(mm1));qqline(resid(mm1))

#other versions of this
library(nlme)
gls1 <- gls(Y ~ POS * Eyes * Sport, data = filter_dat, correlation = corCompSymm(form = ~ 1 | Participant))
summary(gls1)
qqnorm(gls1)

## A SEM-based model


## Effects Plots
library(effects)
plot(allEffects(mm1))

plot(allEffects(gls1))




## For Paper:

## EXPLORATORY ANALYSIS
#ages, heights and weights of the kids (technically I'm simulating these)
# BUT... based off the summary standard deviations and means I was given
# so for illustrative purposes, I'm not terrifically concerned
studs <- rep(c('Soccer','Baseball','Control'), each = 10)
age <- c(rnorm(10, 21.5,1.9),rnorm(10,19.3,1.6),rnorm(10,22.4, 1.5))
height<- c(rnorm(10,171.7,2.2),rnorm(10,174.3,4),rnorm(10,173.3,3.6))
bm <- c(rnorm(10,64.3,4.8), rnorm(10,71.83,7.4),rnorm(10,68.83,5.8))
bmi_dat <- tibble(studs, age, height, bm)

library(beanplot)
par(mfrow=c(1,3))
beanplot(age ~ studs, kernel = "cosine", main = "Age by Group", what = c(1,1,1,0), col =  c("dodgerblue"))
beanplot(height ~ studs, kernel = "optcosine", main = "Height by Group", what = c(1,1,1,0), col = c("dodgerblue"))
beanplot(bm ~ studs, kernel = "gaussian", main = "BMI by Group", what = c(1,1,1,0), col = c("dodgerblue"))


### Beanplot of response values by Sport
beanplot(Y ~ Sport, data = filter_dat, what = c(0,1,1,1), col = c("gold3",'blue4'), ylab = "Area", xlab = "Sport",
         main = "Beanplot of Area by Sport", names = c("Baseball","Soccer","Control"))

#Note we don't see a ton of differences at this point











