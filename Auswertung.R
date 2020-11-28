################################################################################
# early version with a lot of tests etc. 
# the real analysis was performed in the final_analysis - File
################################################################################

library(dplyr)
library(car)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(moments)
library(rstatix)
library(lme4)
library(ggpubr)


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

# rename column and replace yes with 1 and no with 0
names(merged_data)[names(merged_data) == 'Warst.du.schon.einmal.in.Erlangen.'] <- 'Erlangen'
merged_data$Erlangen = merged_data[merged_data$Erlangen == "Ja"] = 1
merged_data$Erlangen = merged_data[merged_data$Erlangen == "Nein"] = 0

#TODO für ingolstadt auch noch


############# split in separate data frames #############

# split per subject
tp1 = subset(merged_data, Proband == "1")
tp2 = subset(merged_data, Proband == "2")
tp5 = subset(merged_data, Proband == "5")
tp6 = subset(merged_data, Proband == "6")

# or group and calculate mean per group
grp_tp = group_by(merged_data, Proband) %>% summarise(., avg = mean(duration), sd=sd(duration))




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


plot_names <- c(
  `Hospital#1` = "Some Hospital",
  `Hospital#2` = "Another Hospital",
  `Hospital#3` = "Hospital Number 3",
  `Hospital#4` = "The Other Hospital"
)

ggplot(by_Anwendung_long, aes(x=Anwendung, y=value, fill=Anwendung)) + 
  geom_bar(show.legend=FALSE, stat='identity', width = 0.2, position = position_dodge(width=0.2)) + theme_bw() + facet_wrap(~key,scales = "free_y")
  #geom_errorbar(aes(ymin=avg_correctRatio-se_correctRatio, ymax=avg_correctRatio+se_correctRatio),  position=position_dodge(.7) , width=0.1, colour="orange", alpha=0.8, size=1)

plot
plot + ggsave("./t.svg", height = 7 , width = 7)


# keinerlei korrelation
cor.test(merged_data$duration, merged_data$correctRatio, method = "pearson")
a1 = subset(merged_data, Anwendung == "EigeneAnwendung")
a2 = subset(merged_data, Anwendung == "OSM")
cor.test(a1$duration, a1$correctRatio, method = "pearson")
cor.test(a2$correctRatio, a2$duration, method = "pearson")


#TODO auf normalverteilung prüfen

t.test(data=merged_data, duration ~ Anwendung, paired = TRUE, alternative = "two.sided")
t.test(data=merged_data, correctRatio ~ Anwendung, paired = TRUE, alternative = "two.sided")


v = xtabs(~Besser.zurechtgekommen + Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., data = pretask_analysis)
as.data.frame(v)

log_model2 = glm(Besser.zurechtgekommen ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.,  family = binomial, data = pretask_analysis)
anova(log_model2, test = 'Chisq')
summary(log_model2)



#boxplot(avg_duration ~ Proband + Anwendung, data=by_Proband_Anwendung, main="Duration over conditions", cex.axis=0.5)

# Vergleich mean duration und correctRatio pro Anwendung über alle Teilnehmer 
# als Boxplot
ggplot(by_Proband_Anwendung, aes(x=Anwendung, y=avg_duration, fill=Anwendung)) + geom_boxplot() + ggsave("./test.svg")
ggplot(by_Proband_Anwendung, aes(x=Anwendung, y=avg_correctRatio, fill=Anwendung)) + geom_boxplot()
# und als Barchart
ggplot(by_Proband_Anwendung) + geom_bar(aes(x=Proband, y=avg_duration, fill=Anwendung), stat='identity', position = "dodge") +
  labs(x = "Participant", y="Average Duration in s") + ggtitle("Average Duration over all Participants")
ggplot(by_Proband_Anwendung) + geom_bar(aes(x=Proband, y=avg_correctRatio, fill=Anwendung), stat='identity', position = "dodge")


ggboxplot(by_Proband_Anwendung, x="Anwendung", y="avg_duration", fill = "Anwendung", palette = "npg", short.panel.labs = TRUE, title = "Average Duration pro Anwendung")


### filter and combine per condition (only take duration and correctRatio)
condition1 <- subset(merged_data, Condition == "EigeneAnwendung - Erlangen - A") %>% select(., Condition, duration, correctRatio)
condition2 <- subset(merged_data, Condition == "EigeneAnwendung - Erlangen - B") %>% select(., Condition, duration, correctRatio)

nav_bind <- rbind(condition1, condition2)
nav_bind_table <- table(nav_bind$duration, nav_bind$correctRatio)
nav_bind_table



##### Auswirkungen des vorwissens: ########
pretask_analysis = group_by(merged_data, Proband) %>% summarise(., avg_dur = mean(duration), sd_dur=sd(duration), avg_ratio = mean(correctRatio), sd_ratio=sd(correctRatio))
pretask_analysis = left_join(pretask_analysis, pretask_cleaned, by="Proband") %>% left_join(., post_answers, by="Proband")

descriptive_stats(pretask_analysis)
by(pretask_analysis$avg_dur, pretask_analysis$Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., descriptive_stats)

regModel = lm(data=pretask_analysis, avg_dur ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)
#aovModel = aov(data=pretask_analysis, avg_dur ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)
summary(regModel)
anova(regModel)

ggplot(pretask_analysis, aes(x=Proband, y=avg_dur, group=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., color=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)) + geom_point() + geom_smooth(method = 'lm')


ggplot(pretask_analysis, aes(x=Proband, y=avg_dur, group=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., color=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)) + geom_boxplot()

ggplot(pretask_analysis, aes(x=Proband, y=avg_dur, group=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., fill=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)) + geom_bar(stat = "identity")


# Beispiel mit string wrap for labels:
ggplot(pretask_analysis, aes(x=stringr::str_wrap(Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., 15), y=avg_dur)) +
  aes(color=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.) +
  geom_boxplot() +
  theme(legend.position="bottom") +
  xlab(NULL)

lv = length(unique(pretask_analysis$Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.))
pretask_analysis %>% summarise(n())
ggplot(pretask_analysis, aes(x=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., y=avg_dur, fill=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)) +
  geom_bar(stat = "identity")  + 
  scale_x_discrete(labels = c('Ein bisschen','Sehr oft','Nie', "sehr wenig"))



regModel = lm(data=pretask_analysis, avg_dur ~ Wie.gut.kennst.du.dich.in.Erlangen.aus. * Wie.gut.kennst.du.dich.in.Ingolstadt.aus.)
#aovModel = aov(data=pretask_analysis, avg_dur ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)
summary(regModel)
anova(regModel) # kein signifikatner Einfluss auf die duration



#### TODO das gleiche für ratio!
regModel = lm(data=pretask_analysis, avg_ratio ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)
#aovModel = aov(data=pretask_analysis, avg_dur ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)
summary(regModel)
anova(regModel) # kein signifikantes ergebnis

ggplot(pretask_analysis, aes(x=Proband, y=avg_ratio, group=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., color=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)) + geom_boxplot()
ggplot(pretask_analysis, aes(x=Proband, y=avg_ratio, group=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., fill=Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap.)) + geom_bar(stat = "identity")





### Auswirkung auf ergebnis
pretask_analysis$Besser.zurechtgekommen <- factor(pretask_analysis$Besser.zurechtgekommen)
logModel <- glm(Besser.zurechtgekommen ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., data = pretask_analysis, family = "binomial")
res = anova(logModel, test = 'Chisq')
summary(logModel) # nicht signifikant für entscheidung

pretask_analysis$Nutzung.im.Alltag <- factor(pretask_analysis$Nutzung.im.Alltag)
logModel <- glm(Nutzung.im.Alltag ~ Hast.du.bereits.Erfahrung.mit.der.Verwendung.von.OpenStreetMap., data = pretask_analysis, family = "binomial")
anova(logModel, test = 'Chisq')
summary(logModel) # nicht signifikant für entscheidung


pretask_analysis$Besser.zurechtgekommen <- factor(pretask_analysis$Besser.zurechtgekommen)
logModel <- glm(Besser.zurechtgekommen ~  Wie.gut.kennst.du.dich.in.Erlangen.aus. * Wie.gut.kennst.du.dich.in.Ingolstadt.aus., data = pretask_analysis, family = "binomial")
anova(logModel, test = 'Chisq')
summary(logModel) # nur bei Erlangen vorwissen signifikant (was sinn macht, da vorwissen bei erlangen aufgrund der verteilten uni etwas mehr hilft als in ingolstadt)


pretask_analysis$Besser.zurechtgekommen <- factor(pretask_analysis$Besser.zurechtgekommen)
logModel <- glm(Besser.zurechtgekommen ~  ., data = pretask_analysis, family = "binomial")
anova(logModel, test = 'Chisq')
#summary(logModel)







##### Auswirkungen auf answers in posttask: ########
frqCol <- sapply(post_answers, table)
apply(frqCol, 1, max)
apply(post_answers,MARGIN=1,table)


# für die duration
tabelle_aggregated = group_by(merged_data, Proband, Anwendung) %>% summarise(., avg_duration = mean(duration))
tabelle_aggregated = left_join(tabelle_aggregated, post_answers, by = "Proband")  %>% select(., c(1:4))

tabelle_wide = spread(tabelle_aggregated, Anwendung, avg_duration)

length(which(tabelle_wide$Besser.zurechtgekommen == "Haus am See"))

tabelle_wide$Besser.zurechtgekommen <- factor(tabelle_wide$Besser.zurechtgekommen)
logModel <- glm(Besser.zurechtgekommen ~ EigeneAnwendung + OSM, data = tabelle_wide, family = "binomial")
anova(logModel, test = 'Chisq')
summary(logModel)

tabelle_aggregated$Besser.zurechtgekommen <- factor(tabelle_aggregated$Besser.zurechtgekommen)
logModel <- glm(Besser.zurechtgekommen ~ avg_duration, data = tabelle_aggregated, family = "binomial")
anova(logModel, test = 'Chisq')
summary(logModel) # nichts signifikantes


#chi_test4 <- chisq.test(tabelle_wide)
#chi_test4


#boxplot(EigeneAnwendung ~ Besser.zurechtgekommen, ylab="Pedigree", xlab= "Diabetes", col="light blue",data = tabelle_wide)
#xtabs(~Besser.zurechtgekommen + EigeneAnwendung, data = tabelle_wide)


#das gleiche für die correctRatio
tabelle_aggregated2 = group_by(merged_data, Proband, Anwendung) %>% summarise(., avg_correctRatio = mean(correctRatio))
tabelle_aggregated2 = left_join(tabelle_aggregated2, post_answers, by = "Proband")  %>% select(., c(1:4))

tabelle_wide2 = spread(tabelle_aggregated2, Anwendung, avg_correctRatio)

tabelle_aggregated2$Besser.zurechtgekommen <- factor(tabelle_aggregated2$Besser.zurechtgekommen)
logModel2 <- glm(Besser.zurechtgekommen ~ avg_correctRatio, data = tabelle_aggregated2, family = "binomial")
anova(logModel2, test = 'Chisq')
summary(logModel2)   # auch nicht signifikant


##### für beide:
tabelle_aggregated_both = tabelle_wide %>% .[-c(2)] %>% left_join(., tabelle_wide2, by = "Proband", suffix = c("_duration", "_ratio"))

tabelle_aggregated_both$Besser.zurechtgekommen <- factor(tabelle_aggregated_both$Besser.zurechtgekommen)
logModel_both <- glm(Besser.zurechtgekommen ~ EigeneAnwendung_duration * OSM_duration * OSM_ratio * EigeneAnwendung_ratio, data = tabelle_aggregated_both, family = "binomial")
anova(logModel_both, test = 'Chisq')
# das einzig signifikante ist die osm duration (0.002049), der Rest absolut nicht -> nur die mittlere benötigte Dauer für die OSM tasks hat einen einfluss darauf, wo (subjektiv) besser zurechtgekommen 



################ Convert to other formats #################

### to long-format

#merged_data$Proband <- as.factor(merged_data$Proband)

shorter_data = select(merged_data, Proband, Anwendung, Stadt, Task, duration, correctRatio)
shorter_data_long = gather(shorter_data, key, value, duration:correctRatio)
shorter_data_long_alternativ = gather(shorter_data, key, value, Condition:correctRatio)

test = gather(merged_data, key, value, duration)
test2 = gather(merged_data, key, value, correctRatio)
test3 = gather(merged_data, key, value, duration, correctRatio)

ggplot(test2, aes(x=as.factor(Proband), y=value, fill=Anwendung)) + geom_boxplot()
ggplot(test2, aes(x=as.factor(Proband), y=value, fill=Anwendung)) + geom_boxplot() + facet_wrap(~Anwendung)
ggplot(test3, aes(x=as.factor(Proband), y=value, fill=Anwendung)) + geom_boxplot() + facet_wrap(~key, scales = "free")
ggplot(test, aes(x=as.factor(Proband), y=value, fill=Anwendung)) + geom_boxplot()


# neuer Boxplot:
ggboxplot(test, x="Proband", y="value", fill = "Anwendung", palette = "npg", facet.by = "Anwendung", short.panel.labs = TRUE)
ggboxplot(test2, x="Proband", y="value", fill = "Anwendung", palette = "npg", facet.by = "Anwendung", short.panel.labs = TRUE)

sd_val = sd(test$value, na.rm = TRUE)
se=sd_val/sqrt(length(test$value))
# TODO add se as separate column so it can be used as errorbar: 
test$se = mutate(test, se = sd(test$value, na.rm = TRUE)/sqrt(length(test$value)))

ggplot(test, aes(x=Proband, y=value, fill=Anwendung)) + geom_bar(stat='identity', position = "dodge") +  theme_classic() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),  position=position_dodge(.7) , width=0.4, colour="orange", alpha=0.9, size=1)

sd_val2 = sd(test2$value, na.rm = TRUE)
ggplot(test2) + geom_bar(aes(x=Proband, y=value, fill=Anwendung), stat='identity', position = "dodge") + 
  geom_errorbar( aes(x=Proband, ymin=value-sd_val2, ymax=value+sd_val2), width=0.4, colour="orange", alpha=0.9, size=1.3)


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

table(pretask_answers$Welches.Betriebssystem.verwendest.du.)

# Post-question answers:
table(post_answers$Besser.zurechtgekommen)
table(post_answers$Zufriedenheit)
table(post_answers$Intuitivität)
table(post_answers$Nutzung.im.Alltag)


#summary(merged_data)
descriptive_stats(merged_data$duration)

# descriptive statistics for duration over all conditions for this variable
by(merged_data$duration, merged_data$Condition, descriptive_stats)

#length(which(merged_data$Besser.zurechtgekommen == "OSM"))
table(merged_data$Besser.zurechtgekommen)


# Wrong answers
descriptive_stats(merged_data$Wrong.Places)
participants_With_Wrong_Answers = subset(merged_data, Wrong.Places > 0)
table(participants_With_Wrong_Answers$Anwendung)


# no correct answers
participants_With_No_Correct_Answers = subset(merged_data, correctRatio == 0)
table(participants_With_No_Correct_Answers$Anwendung)


################ Test preconditions #################

# DVs: duration, correctRatio, (Places.Wrong)
# IVs: Conditions


### Normalverteilung (für Anova muss normalverteilt innerhalb jeder der Gruppe sein)

# shapiro: if the p-value is less than 0.05, we can assume the data is not normally distributed.
shapiro.test(tp1$duration)  # 0.9845 -> normalverteilt
kurtosis(tp1$duration) # 2.37 -> nicht ganz 3, aber trotzdem relativ unbedenklich
skewness(tp1$duration) # -0.21 -> fast 0, also relativ normalverteilt
# -> excess = 2.37 - 3 = -0.63
# schiefe und excess < 1 völlig unbedenklich


tp6 %>% group_by(Anwendung) %>% shapiro_test(duration)


by_Proband_Anwendung %>% group_by(Anwendung) %>% shapiro_test(avg_duration)


t1 = subset(by_Proband_Anwendung, Anwendung == "EigeneAnwendung")
t2 = subset(by_Proband_Anwendung, Anwendung == "OSM")

shapiro.test(t1$avg_duration)
shapiro_test(t1$avg_duration)  
shapiro.test(t2$avg_duration)
shapiro_test(t2$avg_duration)  

kurtosis(t1$avg_duration, na.rm = TRUE)
skewness(t1$avg_duration, na.rm = TRUE)



ggqqplot(by_Proband_Anwendung, "avg_duration", facet.by="Anwendung")


merged_data %>% group_by(Anwendung) %>% shapiro_test(duration)

t1 = subset(merged_data, Anwendung == "EigeneAnwendung")
t2 = subset(merged_data, Anwendung == "OSM")

shapiro.test(t1$duration)
shapiro_test(t1$duration)  
shapiro.test(t2$duration)
shapiro_test(t2$duration)  

ggqqplot(merged_data, "duration", facet.by="Anwendung")
ggqqplot(merged_data, "duration", facet.by="Condition")



merged_data %>% group_by(Anwendung, Stadt, Task) %>% shapiro_test(duration)  # nicht normalverteilt



##### => Zusammenfassng:
# - pro Teilnehmer normalverteilt
# - pro Teilnehmer + Anwendung auch normalverteilt
# - pro Anwendung über alle absolut nicht normalverteilt bei beiden Anwendungen
# - means pro Anwendung nicht normalverteilt für Haus am See
# - pro Condition über alle nur teilweise normalverteilt


### Ausreißer

table(c(abs(scale(merged_data$duration))) < 4 )  # keine Ausreißer, wenn ca. kleiner 4 (für größere Datenmenge > 80 wie bei mir)
# -> keine Ausreißer
outlier_values <- boxplot.stats(merged_data$duration)$out 
boxplot(merged_data$duration, main="merged_data$duration", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
# -> 10 Ausreißer (sollte aber bei der Größe der Datenmenge nicht wirklich schlimm sein)

#pairs(~merged_data$duration + merged_data$correctRatio)

by_Proband_Anwendung %>% group_by(Anwendung) %>% identify_outliers(avg_duration)


### Varianzhomogenität  
# > 0.5 / nicht signifikant heißt Varianzhomogenität!

# TODO
# leveneTest(studienleistung4$Errors ~ interaction(studienleistung4$InterfaceType, studienleistung4$Experience), center = median)
leveneTest(merged_data$duration ~ merged_data$Condition)


#klappt so nicht ...
mlm <- lm(by_Proband_Anwendung ~ 1)

# Mauchly's test
mauchly.test(mlm, x = ~ 1)


################ Statistical Tests #################

# cor.test für korrelation
cor.test(merged_data$duration, merged_data$correctRatio, method = "pearson")
# Correct Ratio und Duration der Tasks korrellieren nicht (p=0.899)


t.test(merged_data$duration ~ merged_data$Anwendung)  # signifikant
t.test(merged_data$duration ~ merged_data$Task)
t.test(merged_data$duration ~ merged_data$Stadt)
t.test(merged_data$correctRatio ~ merged_data$Anwendung) # signifikant
t.test(merged_data$correctRatio ~ merged_data$Task)
t.test(merged_data$correctRatio ~ merged_data$Stadt)

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
anova(model)
summary(model)

# long: model <- aov(data=shorter_data_long, key ~ value) 

model <- aov(data=merged_data, duration ~ Anwendung * Stadt * Task + Error(Proband))
model_alt <- aov(data=merged_data, duration ~ Anwendung * Stadt * Task + Error(Anwendung * Stadt * Task))
summary(model)
summary(model_alt)

model2 = aov(data = merged_data, duration ~ Condition)
summary(model2)

# mixed effects linear model
lmeModel = lmer(data=merged_data, duration ~ Anwendung * Stadt * Task + (1|Proband))
anova(lmeModel)
summary(lmeModel)


model3 = aov(data = by_Proband_Anwendung, avg_duration ~ Anwendung)
summary(model3) # sinifikanter einfluss der Anwendung auf die Duration
model4 = aov(data = by_Proband_Anwendung, avg_correctRatio ~ Anwendung)
summary(model4) # sinifikanter einfluss der Anwendung auf die correctRatio

# nur Anwendung alleine hat signifikanten Einfluss
anova(lm(duration ~ Anwendung * Task * Stadt, merged_data))
pairwise.t.test(merged_data$duration, merged_data$Anwendung, p.adj = "none") # statisch signifikanter unterschied zw. beiden Anwendungen



by_Proband_Anwendung2 = by_Proband_Anwendung %>% convert_as_factor(Proband, Anwendung)
head(by_Proband_Anwendung, 5)
head(shorter_data, 5)
head(shorter_data_long, 5)


d = subset(merged_data, Proband != "14" & Proband != "15" & Proband != "16"  & Proband != "13")
any(is.na(d))
d2 = subset(by_Proband_Anwendung, Proband != "14" & Proband != "15" & Proband != "16"  & Proband != "13")

# klappt auch nicht ....
anova_test(data = by_Proband_Anwendung, dv=avg_duration, wid=Proband, within = Anwendung)

anova_test(data = shorter_data_long, dv=value, wid=Proband, within = c(Anwendung, Stadt, Task))
anova_test(data = shorter_data, dv=duration, wid=Proband, within = Anwendung)


#interaction.plot(by_Proband_Anwendung$Anwendung, by_Proband_Anwendung$avg_duration, by_Proband_Anwendung$avg_correctRatio, xlab = "InterfaceType", ylab = "mean of Errors", trace.label = "Experience", col = 3:4)


# kein complete block design ???
merged_data %>% friedman_test(duration ~ Anwendung |Proband)



#pairwise.t.test(merged_data$duration, merged_data$Anwendung, p.adj = "bonf", paired = TRUE)
pairwise.t.test(merged_data$duration, merged_data$Anwendung, p.adj = "bonf")

# geht nicht, erwarte binäres Argument: t.test(by_Proband_Anwendung$avg_duration, by_Proband_Anwendung$Anwendung, paired = TRUE)

TukeyHSD(model) # nur bei Anwendung ein statistisch signifikanter Unterschied bzgl der Duration
TukeyHSD(model2)
TukeyHSD(model3)


# Anova() aus car package stattdessen für typ 2 ergebnisse?
Anova(model)  # sind auch die gleichen ergebnisse ...

plot(model)
# plot(aov(studienleistung4$Errors ~ studienleistung4$InterfaceType * studienleistung4$Experience))



############### Plots / Histogramme ###################

# TODO nur die mittelwerte plotten pro anwendung / condition anstatt alle ??


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

boxplot(duration ~ Condition, data=merged_data, main="Duration over conditions", cex.axis=0.5)
ggplot(aes(x=Condition, y = duration), data=merged_data, main="Duration over conditions") + 
  geom_boxplot(show.legend = TRUE) + theme(axis.text.x  = element_text(angle=45, vjust=0.5))


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

### timeseries

# make condition an ordered factor so ggplot doesn't order it
tp1$Condition <- factor(tp1$Condition, levels = tp1$Condition)
tp2$Condition <- factor(tp2$Condition, levels = tp2$Condition)


merged_data$Condition = group_by(merged_data, Proband) %>% factor(.$Condition, levels = .$Condition)

ggplot(merged_data, aes(x=Condition, y=duration, group= Condition, color=Anwendung)) + theme_classic() +
  geom_line() +
  geom_point() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))

for(i in 1:5) {
  tp = subset(merged_data, Proband == i)
  tp$Condition <- factor(tp$Condition, levels = tp$Condition)
  #plot_data = data.frame(x=merged_data[,i], y=merged_data$Condition)
  plot = ggplot(tp, aes(x=Condition, y=duration, group= Anwendung, color=Anwendung)) + theme_classic() +
    geom_line() +
    geom_point() + 
    ggtitle("Proband Nr.", i) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  print(plot)
}


merged_data2 <- factor(merged_data$Condition, levels = unique(merged_data$Condition))
merged_data3 = make.unique(as.character(merged_data$Condition), sep = "_")
merged_data["nr"] <- seq(8) 

ggplot(merged_data, aes(x=nr, y=duration, group= Anwendung, color=Anwendung)) + 
  theme_classic() +
  geom_line() +
  geom_point() +
  ggtitle("Benötigte Zeit in Sekunden pro Condition über alle Probanden") +
  theme(legend.position="bottom",axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank()) +
  facet_wrap(~Proband, ncol = 4)



lm(data=merged_data, duration ~ time(Anwendung))




md = lm(data=tp1, duration ~ time(Condition))
md
plot(md)

ci <- predict(md, interval="confidence")
plot(ci)
abline(md, col='red')
lines(x=as.vector(time(tp1$Condition)), y=as.vector(ci[,3]), col='red', lty=2)
lines(x=as.vector(time(tp1$Condition)), y=as.vector(ci[,2]), col='red', lty=2)
plot(ci)
