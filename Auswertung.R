library(dplyr)
library(car)
library(tidyverse)
library(lubridate)
library(moments)

################################################################################

#### Variablen:

## Abhängige Variablen:
# - Duration (intervallskaliert)
# - correctRatio (verhältnisskaliert)
# - Post-Anwtorten (dichotom, bzw. nominalskaliert)

# (- wrong answers) -> sinnvoll????

## Unabhängige Variablen:
# - Condition (nominal)
# oder alle 3: Stadt, Anwendung, Task (alle nominal)

###############


#### Fragestellungen:
# - ist ein Proband schneller geworden, je öfter er eine Anwendung benutzt hat (grafisch plotten pro Anwendung?)
# - unterschied bei 1. Nutzung und bei forfolgenden bzgl. der Duration? -> getrennt analysieren??
# - Durschnitt und SD über Duration und correctRatio insgesamt und pro Teilnehmer
# - Unterschiede bei Duration und der correctRatio für Post-Antworten (Haus am See oder OSM) ???????????????? (hängt das wirklich überhaupt zusammen?)
# - bei welcher Anwendung schneller im Durschnitt
# - theoretisch auch Kombis mit Task und Stadt möglich, aber hilft das was?
# - Auswirkung der Condition allgemein auf Duration und CorrectRatio
# - Gab es irgendjemanden, der bei einer aufgabe keinen ort mit einer anwendung gefunden hat?? Ansonsten min und max vllt?
# - Timeseries pro Participant über die duration beider anwendungen plotten, um rauszufinden ob sie bei mir schneller wurden (also ein Lerneffekt da war)?
# - bevorzugten leute mit osm vorerfahrung die manuelle suche oder OSM ?
#####################


################## functions ######################

descriptive_stats <- function(x){
  list(min = min(x), max = max(x), mean = mean(x), median = median(x), variance = var(x), standard_deviation = sd(x))
}

calcDuration = function (start, end) {
  startInSeconds = ms(start) %>% as.numeric(.)
  endInSeconds = ms(end) %>% as.numeric(.)
  endInSeconds - startInSeconds
}


############### load data #####################

pretask_answers = read.csv("./Pre-task Questionnaire(Antworten).csv")
data = read.csv2("./data.csv")
post_answers = read.csv2("./post_data.csv")


################## setup ######################

# remove unnecessary column from post data
post_data_cleaned = select(post_data, -Heatmap...Maskierung)

# remove the participants 1-4 and 6-8 :(
# and columns that only have pure qualitative data
participants = pretask_answers %>% .[-c(1:4, 6:9),] %>% .[c(9:11)]
pretask_cleaned = pretask_answers %>% .[-c(1:4, 6:9),] %>% .[c(2:6)]
nrow(pretask_cleaned)  # should be 16!


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
data$Places.Overall[data$Stadt == "Ingolstadt" & data$Task == "B"] <- 9

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

# or group and calculate mean per group
grp_tp = group_by(merged_data, Proband) %>% summarise(., avg = mean(duration), sd=sd(duration))



### TODO geht erst wenn gleiche größe

# add pretask knowledge
#merged_data = cbind(merged_data, select(pretask_cleaned, Warst.du.schon.einmal.in.Erlangen., Warst.du.schon.einmal.in.Ingolstadt.)) 
merged_data$Erlangen = pretask_cleaned$Warst.du.schon.einmal.in.Erlangen.
merged_data$Erlangen = merged_data[merged_data$Erlangen == "Ja"] = 1
merged_data$Erlangen = merged_data[pretask_cleaned$Warst.du.schon.einmal.in.Erlangen. == "Nein"] = 0

#TODO für ingolstadt auch noch


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

table(participants$Geschlecht)
table(participants$Studienfach)
descriptive_stats(participants$Alter)

summary(pretask_cleaned)
table(pretask_cleaned$Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)
table(pretask_cleaned$Warst.du.schon.einmal.in.Erlangen.)
as.data.frame(table(pretask_cleaned$Warst.du.schon.einmal.in.Ingolstadt.))
descriptive_stats(pretask_cleaned$Wie.gut.kennst.du.dich.in.Erlangen.aus.)
descriptive_stats(pretask_cleaned$Wie.gut.kennst.du.dich.in.Ingolstadt.aus.)


summary(merged_data)
descriptive_stats(merged_data$duration)

# descriptive statistics for duration over all conditions for this variable
by(merged_data$duration, merged_data$Condition, descriptive_stats)

#length(which(merged_data$Entscheidung.insgesamt == "OSM"))
#length(which(merged_data$Entscheidung.insgesamt == "Haus am See"))
table(merged_data$Entscheidung.insgesamt)

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


###########
#TODO er will hier unbedingt zahlen, sonst gehts nicht
for(i in c("duration", "correctRatio")) {
  plot_data = data.frame(x=merged_data[,i], y=merged_data$Condition)
  y_name = i
  x_name = 'Condition'
  print(paste('correlating ', x_name, ' with ', y_name))
  
  # Scatterplot with linear model
  #plot = ggplot(plot_data, aes(x=x, y=y)) + xlab(x_name) + ylab(y_name) + geom_point() + geom_smooth(method = 'lm')
  #print(plot)
  
  # Boxplot
  plot = ggplot(merged_data, aes(x=Condition, y=merged_data[,i])) + xlab(x_name) + ylab(y_name) + geom_boxplot() #geom_boxplot(outlier.shape = NA)
  print(plot)
  
  #Boxplot ohne Ausreißer
  boxplot(merged_data[,i] ~ merged_data$Condition, xlab = x_name, ylab = y_name, outline = FALSE)
  
  
  #TODO hat untersch. dimens:
  # print(cor.test(merged_data[,i], merged_data$Condition))
}
