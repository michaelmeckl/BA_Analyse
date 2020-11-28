library(dplyr)
library(car)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(moments)
library(rstatix)
library(lme4)
library(ggpubr)

################################################################################

#### Variablen:

## Abhängige Variablen:
# - Duration (intervallskaliert)
# - correctRatio (verhältnisskaliert)

## Unabhängige Variablen:
# - Condition (nominal)
# bzw alle 3: Stadt, Anwendung, Task (alle nominal)

###############

#### Fragestellungen:
# - ist ein Proband schneller geworden, je öfter er eine Anwendung benutzt hat
#   -> Timeseries pro Participant über die duration beider anwendungen plotten, um rauszufinden ob sie bei mir schneller wurden?
# - Durschnitt und SD über Duration und correctRatio insgesamt und pro Teilnehmerbei welcher Anwendung schneller im Durschnitt
# - Auswirkung der Condition allgemein auf Duration und CorrectRatio
# - Gab es irgendjemanden, der bei einer aufgabe keinen ort mit einer anwendung gefunden hat?
# - bevorzugten leute mit osm vorerfahrung die manuelle suche oder OSM ?
# - wo wurden mehr Fehler gemacht?
# - allg. deskriptive statistiken zu teilnehmern und abh. variablen

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

pretask_answers = read.csv2("./Pretask-Answers.csv")
data = read.csv2("./Data.csv")
post_answers = read.csv2("./Posttask-Answers.csv")


################## tidy data ###################

participants = pretask_answers %>% .[c(8:10)]
# remove columns that only have pure qualitative data
pretask_cleaned = pretask_answers %>% .[c(1:5)]
#nrow(pretask_cleaned)  # should be 16!


# split the condition column into 3 separate ones but also keep the old column
splitted_condition = data %>% separate(Condition, c("Anwendung", "Stadt", "Task"), sep="-")
data = cbind(data, select(splitted_condition, Anwendung, Stadt, Task)) %>% .[c(1, 2, 7:9, 3:6)]

# remove trailing and leading whitespaces from new columns and convert them back to characters
data$Stadt = lapply(data$Stadt, trimws)
data$Stadt = as.character(data$Stadt)
data$Anwendung = lapply(data$Anwendung, trimws)
data$Anwendung = as.character(data$Anwendung)
data$Task = lapply(data$Task, trimws)
data$Task = as.character(data$Task)

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

merged_data = left_join(cleaned_data, post_answers, by = "Proband")


# add pretask knowledge
pretask_cleaned$Proband <- seq.int(nrow(pretask_cleaned)) # add row number as proband id so it can be joined
merged_data = left_join(merged_data, pretask_cleaned, by = "Proband")


################## participants ###################

table(participants$Geschlecht)
table(participants$Studienfach)
descriptive_stats(participants$Alter)
summary(participants)

################## descriptive stuff for the rest ###################

summary(pretask_cleaned)
table(pretask_cleaned$Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)
table(pretask_cleaned$Warst.du.schon.einmal.in.Erlangen.)
table(pretask_cleaned$Warst.du.schon.einmal.in.Ingolstadt.)
descriptive_stats(pretask_cleaned$Wie.gut.kennst.du.dich.in.Erlangen.aus.)
descriptive_stats(pretask_cleaned$Wie.gut.kennst.du.dich.in.Ingolstadt.aus.)

# Post-question answers:
table(post_answers$Besser.zurechtgekommen)
table(post_answers$Zufriedenheit)
table(post_answers$Intuitivität)
table(post_answers$Nutzung.im.Alltag)


descriptive_stats(merged_data$duration)
descriptive_stats(merged_data$correctRatio)

# by calculates descriptive statistics for duration grouped by anwendung for this variable
by(merged_data$duration, merged_data$Anwendung, descriptive_stats)
by(merged_data$correctRatio, merged_data$Anwendung, descriptive_stats)


#length(which(merged_data$Besser.zurechtgekommen == "OSM"))
table(merged_data$Besser.zurechtgekommen)


# Wrong answers
descriptive_stats(merged_data$Wrong.Places)
participants_With_Wrong_Answers = subset(merged_data, Wrong.Places > 0)
table(participants_With_Wrong_Answers$Anwendung)


# no correct answers
participants_With_No_Correct_Answers = subset(merged_data, correctRatio == 0)
table(participants_With_No_Correct_Answers$Anwendung)


################## inductive stuff starts here ###################

# get all mean values for every condition per participant
by_Proband_Anwendung = group_by(merged_data, Proband, Anwendung) %>%
  summarise(., avg_duration = mean(duration), sd_duration=sd(duration), avg_correctRatio = mean(correctRatio), sd_correctRatio=sd(correctRatio))


model <- aov(data=merged_data, duration ~ Anwendung * Stadt * Task + Error(Proband))
summary(model)

model2 <- aov(data=merged_data, correctRatio ~ Anwendung * Stadt * Task + Error(Proband))
summary(model2)
#TukeyHSD(model2, "Anwendung")
# pairwise.t.test(merged_data$correctRatio, merged_data$Anwendung, p.adj = "none")


by_Anwendung = group_by(merged_data, Anwendung) %>%
  summarise(., avg_duration = mean(duration), sd_duration=sd(duration), avg_correctRatio = mean(correctRatio), sd_correctRatio=sd(correctRatio), 
            se_correctRatio = sd(correctRatio)/sqrt(length(merged_data$Proband)), se_duration = sd(duration)/sqrt(length(merged_data$Proband)))


# keinerlei korrelation
cor.test(merged_data$duration, merged_data$correctRatio, method = "pearson")
a1 = subset(merged_data, Anwendung == "EigeneAnwendung")
a2 = subset(merged_data, Anwendung == "OSM")
cor.test(a1$duration, a1$correctRatio, method = "pearson")
cor.test(a2$correctRatio, a2$duration, method = "pearson")


# auf normalverteilung prüfen
# shapiro: if the p-value is less than 0.05, we can assume the data is not normally distributed.
# shapiro.test(merged_data$duration)
# kurtosis(merged_data$duration)
# skewness(merged_data$duration)
# 
# merged_data %>% group_by(Anwendung) %>% shapiro_test(duration)
# merged_data %>% group_by(Anwendung) %>% shapiro_test(correctRatio)
# 
# # looks more or less normally distributed to me ...
# ggqqplot(merged_data, "duration", facet.by="Anwendung")
# ggqqplot(merged_data, "correctRatio", facet.by="Anwendung")


t.test(data=merged_data, duration ~ Anwendung, paired = TRUE, alternative = "two.sided")
t.test(data=merged_data, correctRatio ~ Anwendung, paired = TRUE, alternative = "two.sided")


################## boxplots and barcharts ###################

# test = gather(merged_data, key, value, duration)
# test2 = gather(merged_data, key, value, correctRatio)
# test3 = gather(merged_data, key, value, duration, correctRatio)
# 
# ggplot(test2, aes(x=as.factor(Proband), y=value, fill=Anwendung)) + geom_boxplot() +  theme(legend.position="bottom")
# 
# ggplot(test2, aes(x=as.factor(Proband), y=value, fill=Anwendung)) + geom_boxplot() + facet_wrap(~Anwendung) +  theme(legend.position="bottom")
# ggplot(test3, aes(x=as.factor(Proband), y=value, fill=Anwendung)) + geom_boxplot() + facet_wrap(~key, scales = "free") +  theme(legend.position="bottom")


ggplot(by_Anwendung, aes(x=Anwendung, y=avg_correctRatio, fill=Anwendung,  label = scales::percent(avg_correctRatio/100))) + 
  geom_bar(show.legend=FALSE, stat='identity', width = 0.2, position = position_dodge(width=0.2)) + theme_bw() +
  scale_y_continuous(limits = c(0,100)) + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.8,    # nudge above top of bar
            size = 3) + 
  xlab("Anwendung") +  ylab("Anteil korrekter Gebiete in %") + ggtitle("Durschnittlicher Anteil an richtigen Orten für beide Anwendungen")

ggplot(by_Anwendung, aes(x=Anwendung, y=avg_duration, fill=Anwendung, label = round(avg_duration, digits = 1))) + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.8,    # nudge above top of bar
            size = 3) + 
  geom_bar(show.legend=FALSE, stat='identity', width = 0.2, position = position_dodge(width=0.2)) + theme_bw() +
  xlab("Anwendung") +  ylab("benötigte Zeit in ms") + ggtitle("Durschnittlich benötigte Zeit pro Anwendung")


by_Anwendung_long = gather(by_Anwendung, key, value, avg_duration, avg_correctRatio)


ggplot(by_Anwendung_long, aes(x=Anwendung, y=value, fill=Anwendung)) + 
  geom_bar(show.legend=FALSE, stat='identity', width = 0.2, position = position_dodge(width=0.2)) + theme_bw() + facet_wrap(~key,scales = "free_y")
#geom_errorbar(aes(ymin=avg_correctRatio-se_correctRatio, ymax=avg_correctRatio+se_correctRatio),  position=position_dodge(.7) , width=0.1, colour="orange", alpha=0.8, size=1)


new_names <- c(
  `duration` = "Task Dauer in Sekunden",
  `correctRatio` = "Anteil an korrekten Orten in %"
)

#shorter_data_long = gather(by_Proband_Anwendung, key, value, avg_duration, avg_correctRatio)
shorter_data_long = gather(merged_data, key, value, duration, correctRatio)

ggplot(shorter_data_long, aes(x=Anwendung, y=value, fill=Anwendung)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) +
  facet_wrap(~key,scales = "free_y", labeller = as_labeller(new_names))
ggsave("./boxplots.svg")


############## Pretask and Posttask  #####################
v = xtabs(~Besser.zurechtgekommen + Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., data = merged_data)
as.data.frame(v)

# log_model2 = glm(Besser.zurechtgekommen ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.,  family = binomial, data = merged_data)
# anova(log_model2, test = 'Chisq')
# summary(log_model2)


################## time over conditions ###################

merged_data["nr"] <- seq(8) # to keep the order!!

ggplot(merged_data, aes(x=nr, y=duration, group= Anwendung, color=Anwendung)) + 
  theme_classic() +
  geom_line() +
  geom_point() +
  ggtitle("Benötigte Zeit in Sekunden pro Condition über alle Probanden") +
  theme(legend.position="bottom",axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank()) +
  facet_wrap(~Proband, ncol = 4)

ggsave("./time_over_conditions.svg")
