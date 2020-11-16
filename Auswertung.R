library(dplyr)
library(car)
library(tidyverse)
library(lubridate)
library(moments)
library(todor)

################## functions ######################

descriptive_stats <- function(x){
  list(mean = mean(x), median = median(x), variance = var(x), standard_deviation = sd(x))
}

fun.ct = function(d){c(mean = mean(d), variance=var(d), standard_deviation=sd(d))}

calcDuration = function (start, end) {
  startInSeconds = ms(start) %>% as.numeric(.)
  endInSeconds = ms(end) %>% as.numeric(.)
  endInSeconds - startInSeconds
}


################## setup ######################

##### TODO die qualitativen Daten müssen als separate Tabelle eingelesen und am besten in jeder Zelle nur OSM oder eigeneAnwendung
post_data = read.csv2("./post_data.csv", sep=";")

post_data_cleaned = select(post_data, -Heatmap...Maskierung)

data = read.csv2("./data.csv", sep=";")

# split the condition column into 3 separate ones but also keep the old column
splitted_condition = data %>% separate(Condition, c("Anwendung", "Stadt", "Task"), sep="-")
data = cbind(data, select(splitted_condition, Anwendung, Stadt, Task)) %>% .[c(1, 2, 7:9, 3:6)]

# remove trailing and leading whitespaces from new columns
data$Stadt = lapply(data$Stadt, trimws)
data$Anwendung = lapply(data$Anwendung, trimws)
data$Task = lapply(data$Task, trimws)

# add the overall places for each condition as a new column 
data["Places.Overall"] <- NA
data$Places.Overall[data$Stadt == "Erlangen" & data$Task == "A"] <- 3
data$Places.Overall[data$Stadt == "Erlangen" & data$Task == "B"] <- 13
data$Places.Overall[data$Stadt == "Ingolstadt" & data$Task == "A"] <- 3
data$Places.Overall[data$Stadt == "Ingolstadt" & data$Task == "B"] <- 8

# debugging
is.numeric(data$Correct.Places)
is.numeric(data$Places.Overall)

# calculate the ratio of correct places in percent
data = transform(data, correctRatio =  (data$Correct.Places / data$Places.Overall ) * 100)

# calculate the difference between each start and end time in seconds
data$duration = calcDuration(data$Start, data$End)

# remove the now obsolete columns
cleaned_data = select(data, -Start, -End, -Places.Overall, -Correct.Places)

merged_data = cbind(cleaned_data, select(post_data_cleaned, -Proband))


# split per subject
tp1 = subset(merged_data, Proband == "1")
tp2 = subset(merged_data, Proband == "2")


###### TODO:
# - ist es vllt relevant wofür sich die leute letztlich entschieden als eigene Spalte -> also osm oder eigene???


################ Convert to other formats #################

### to long-format

#merged_data$Proband <- as.factor(merged_data$Proband)

shorter_data = select(merged_data, Proband, Condition, duration, correctRatio)
shorter_data_long = gather(shorter_data, key, value, duration:correctRatio)
shorter_data_long_alternativ = gather(shorter_data, key, value, Condition:correctRatio)


### filter and combine per condition (only take duration and correctRatio)
condition1 <- subset(merged_data, Condition == "EigeneAnwendung + Erlangen + A") %>% select(., Condition, duration, correctRatio)
condition2 <- subset(merged_data, Condition == "EigeneAnwendung + Erlangen + B") %>% select(., Condition, duration, correctRatio)

nav_bind <- rbind(condition1, condition2)
nav_bind_table <- table(nav_bind$duration, nav_bind$correctRatio)
nav_bind_table



################ Descriptive Statistics #################

summary(merged_data)
descriptive_stats(merged_data$duration)

# descriptive statistics for duration over all conditions for this variable
by(merged_data$duration, merged_data$Condition, fun.ct)



################ Test preconditions #################

# DVs: duration, correctRatio, (Places.Wrong)
# IVs: Conditions


### Normalverteilung

# shapiro: if the p-value is less than 0.05, we can assume the data is not normally distributed.
shapiro.test(tp1$duration)  # 0.9845 -> normalverteilt
kurtosis(tp1$duration) # 2.37 -> nicht ganz 3, aber trotzdem relativ unbedenklich
skewness(tp1$duration) # -0.21 -> fast 0, also relativ normalverteilt
# -> excess = 2.37 - 3 = -0.63
# schiefe und excess < 1 völlig unbedenklich


### Varianzhomogenität  
# > 0.5 / nicht signifikant heißt Varianzhomogenität!

# leveneTest(studienleistung4$Errors ~ interaction(studienleistung4$InterfaceType, studienleistung4$Experience), center = median)
leveneTest(merged_data$duration ~ merged_data$Condition)




################ Statistical Tests #################

# cor.test für korrelation


# regression für zusammenhänge!!!!!!!!

### logistische Regression
#log_model = glm(train$trust_as_boole ~ negative_opinion_phrases + weak_subj_words + biased_words + NUM + PART + PUNCT, data = train[, -c(1,31,34:35)], family = binomial(link="logit"))
log_model = glm(duration ~ Condition, data = merged_data)
#TODO: log_model = glm(duration ~ Anwendung * Stadt * Task, data = merged_data)

anova(log_model, test = 'Chisq')
summary(log_model) # AIC: 272.77



### Anova

# two way anova -> zweifaktorielle varianzanalyse
# manova mehrdimensionale varianzanalyse

# daten müssen im long format sein für anova!!! -> gather()
# unabhängige variable needs to be factor is.factor

# modell erstellen
#model <- aov(data=merged_data, duration ~ Condition)
model <- aov(data=merged_data, duration ~ Anwendung * Stadt * Task) # DV ~ IV +|* IV

# long: model <- aov(data=shorter_data_long, key ~ value)  

#summary(model)
anova(model)

# Anova() aus car package stattdessen für typ 2 ergebnisse?
Anova(model)  # sind auch die gleichen ergebnisse ...

plot(model)
# plot(aov(studienleistung4$Errors ~ studienleistung4$InterfaceType * studienleistung4$Experience))



############### Plots / Histogramme ###################

### Histogramm
table(merged_data$duration)
#png(filename="./Plots/hist_duration.png")
hist_duration <- barplot(tp1$duration ~ tp1$Condition, main="Task durations over all participants", 
                         xlab=colnames(merged_data$Proband), ylab="Duration (in seconds)", las=2, legend=TRUE,
                         col="orange", horiz = FALSE, space=0.8, cex.axis="0.7", cex.names="0.9")
#dev.off()


### Bar Chart
ggplot(data = merged_data, aes(x = Condition, y=duration)) + geom_bar(aes(fill=Condition), position="dodge", stat="identity") 
#ggsave("./Plots/bar_plot.png")


### Boxplots

plot = ggplot(merged_data, aes(x=merged_data, y=duration)) + xlab(colnames(merged_data)) + ylab("Duration (in seconds)") + geom_boxplot()
plot

plot_2 <- ggplot(data=cleaned_data, aes(x = Proband, y= duration, group=1)) + geom_boxplot() +  theme_bw() + 
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank()) + ggtitle("Duration for both participants")
plot_2
# ggsave("./Plots/boxplot.png")


# plot time development over conditions for tp1
plot_new = ggplot(tp1, aes(x=Anwendung, y=duration)) + xlab(colnames(tp1)) + ylab("Duration (in seconds)") + geom_point(aes(colour = factor(Condition)))
plot_new
plot_new2 = ggplot(tp1, aes(x=Anwendung, y=duration)) + xlab(colnames(tp1)) + ylab("Duration (in seconds)") + geom_count(aes(colour = factor(Condition)))
plot_new2


