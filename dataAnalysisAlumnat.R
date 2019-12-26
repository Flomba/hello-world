#################### R code - Survey Analysis ######################
# Treball com??: Sociologia Psicologia Pedagogia
# Master de formaci?? del Professorat ESO 
# Universitat Autonoma
# Grup: Vadillo, S??ndez, Lombardi
# 23 december 2019
####################################################################


# load the libraries 
library("ggplot2")

### Functions ####

median_ordinal <- function(x) {
  d <- table(x)
  cfd <- cumsum(d / sum(d))
  idx <- min(which(cfd >= .5))
  return(levels(x)[idx])
}

###################### DATA CLEANING ######################################

# download google-form results as cvs file

# read in data 

d <- read.csv("~/Desktop/MASTER_PROFESSORADO/MATERIALE/PRACTICUM/Alumnat/rawData1.csv", sep=",", header=TRUE)


# Change colnames of all columns to facilitate handling

colnames(d) <- c("timestamp",
                     "year.of.birth",
                     "gender",
                     "curs",
                     "good.marks.important",
                     "search.extra.info.once.home",
                     "homework.done",
                     "lessons.go.by.quickly",
                     "behaviour.happy",
                     "not.reading.questions",
                     "fav.subject",
                     "hate.subject",
                     "school.because.mandatory",
                     "school.for.success",
                     "school.for.friends",
                     "school.for.personal.growth",
                     "fine.at.school",
                     "fine.with.peers",
                     "answering.mad",
                     "issues.peers",
                     "fine.with.teachers",
                     "issues.with.teachers",
                     "get.goodmarks",
                     "speak.with.friends",
                     "speak.at.home",
                     "tutoring.from.family",
                     "tutoring.form.school",
                     "tutoring.from.others",
                     "afterschool",
                     "afterschool.type",
                     "afterESO.work",
                     "afterESO.CF",
                     "afterESO.batxi",
                     "afterESO.not.sure",
                     "want.to.go.to.uni",
                     "parents.advice",
                     "teachers.advice",
                     "future.job",
                     "like.julia",
                     "dontlike.julia",
                     "one.million.julia")

# REMOVE SPURIOUS DATA

# individuals who failed the check questions 
d <- subset(d, !(not.reading.questions!="Gens d'acord" & answering.mad !="Gens d'acord"))
# 2n de batxillerat. There's only one responder in this catergory
# and it can be considered an outlier therefore it can be removed safely
d <- subset(d, curs!="2r BATXILLERAT")

# CHANGE LEVELS (Catal?? i Castell?? to Aut??cton) 

levels(d$speak.at.home)[levels(d$speak.at.home)=="Catal??"] <- "Aut??cton"
levels(d$speak.at.home)[levels(d$speak.at.home)=="Castell??"] <- "Aut??cton"

# REORDER FACTORS LEVELS

d$good.marks.important <- factor(d$good.marks.important,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$search.extra.info.once.home <- factor(d$search.extra.info.once.home,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$homework.done <- factor(d$homework.done,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$lessons.go.by.quickly <- factor(d$lessons.go.by.quickly,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$behaviour.happy <- factor(d$behaviour.happy,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
# note reversed levels 
d$school.because.mandatory <- factor(d$school.because.mandatory,levels=c("Molt d'acord","D'acord","Poc d'acord","Gens d'acord")) 
d$school.for.success <- factor(d$school.for.success,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$school.for.friends <- factor(d$school.for.friends,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$school.for.personal.growth <- factor(d$school.for.personal.growth,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$fine.at.school <- factor(d$fine.at.school,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$fine.with.peers <- factor(d$fine.with.peers,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d$fine.with.teachers <- factor(d$fine.with.teachers,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
# note reversed levels 
d$issues.with.teachers <- factor(d$issues.with.teachers,levels=c("Molt d'acord","D'acord","Poc d'acord","Gens d'acord") )
# note reversed levels 
d$issues.peers <- factor(d$issues.peers,levels=c("Molt d'acord","D'acord","Poc d'acord","Gens d'acord") )
d$get.goodmarks <- factor(d$get.goodmarks,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )

# composition gender and origin

origin_table <- table(d$speak.at.home)
gender_table <- table(d$gender)


################# STUDENTS IDENTIFICATION ##################################

# combine variables to create dataset d1 student identification

d1 <- subset(d, select = c("gender",
                            "curs",
                            "speak.at.home",
                            "good.marks.important",
                            "search.extra.info.once.home",
                            "homework.done",
                            "homework.done",
                            "lessons.go.by.quickly",
                            "school.because.mandatory",
                            "school.because.mandatory",
                            "school.for.success",
                            "school.for.friends",
                            "school.for.personal.growth",
                            "fine.at.school",
                            "fine.with.peers", 
                            "issues.peers", 
                            "fine.with.teachers",
                            "issues.with.teachers",
                            "get.goodmarks"))

# rename variables based on expressive or instrumental 

names(d1) <- c("gender",
               "curs", 
               "origin", 
               "inst1", 
               "exp1",
               "exp2" ,
               "inst2", 
               "exp3",
               "expN1",
               "instN1", 
               "inst3",
               "exp4",
               "exp5",
               "exp6",
               "exp7",
               "exp8",
               "expN2",
               "expN3",
               "marks"
)

   
# convert factors to integers 

d1$inst1 <- as.integer(d1$inst1)
d1$exp1 <- as.integer(d1$exp1)
d1$exp2 <- as.integer(d1$exp2)
d1$inst2 <- as.integer(d1$inst2)
d1$exp3 <- as.integer(d1$exp3)
d1$expN1 <- as.integer(d1$expN1)
d1$instN1 <- as.integer(d1$instN1)
d1$inst3 <- as.integer(d1$inst3)
d1$exp4 <- as.integer(d1$exp4)
d1$exp5 <- as.integer(d1$exp5)
d1$exp6 <- as.integer(d1$exp6)
d1$exp7 <- as.integer(d1$exp7)
d1$expN2 <- as.integer(d1$expN2)
d1$exp8 <- as.integer(d1$exp8)
d1$expN3 <- as.integer(d1$expN3)
d1$marks <- as.integer(d1$marks)

d1$expNorm <- (d1$exp1 + d1$exp2 + d1$exp3 + d1$exp4 + d1$exp5 + d1$exp6 + d1$exp7+ d1$exp8+d1$expN1 +d1$expN2 +d1$expN3)/11
d1$instNorm <- (d1$inst1+d1$inst2 + d1$inst3 + d1$instN1)/4

#d1$expNorm <- (d1$exp1 + d1$exp2 + d1$exp3 + d1$exp4 + d1$exp5 + d1$exp6 + d1$exp7+ d1$exp8+d1$expN1 +d1$expN2 +d1$expN3)
#d1$instNorm <- (d1$inst1+d1$inst2 + d1$inst3 + d1$instN1)

# dotplots with overplotting 
qplot(expNorm, instNorm, data = d1, colour= gender,xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental")
qplot(expNorm, instNorm, data = d1, colour= origin,xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental")
qplot(expNorm, instNorm, data = d1, colour= origin,shape = gender, xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental")

# add jitter to prevent overplotting

# origin
qplot(expNorm, instNorm, data = d1, colour= origin,geom="jitter" , xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental", size=1)

# gender
qplot(expNorm, instNorm, data = d1, colour= gender,geom="jitter" , xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental", size=1)


# gender and origin
qplot(expNorm, instNorm, data = d1, colour= origin,shape = gender,geom="jitter" , xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental")

#curs
qplot(expNorm, instNorm, data = d1, colour= curs, geom="jitter", size=1, xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental")

#marks
qplot(expNorm, instNorm, data = d1, color= marks, geom="jitter", size=1, xlab = "Identificaci?? Expressiva", ylab = "Identificaci?? Instrumental")


# pi?? applicati
d2 <-  subset(d1, expNorm >3.2 &instNorm >3.2 &gender!="Altres" )

# whisker plot
qplot(gender, instNorm, data = d2, 
      geom=c("boxplot"), 
      stackdir = "center", binaxis = "y",
      fill=origin,
      ylab = "Identificaci?? Instrumental"
)


qplot(gender, expNorm, data = d2, 
      geom=c("boxplot"), 
      stackdir = "center", binaxis = "y",
      fill=origin,
      ylab = "Identificaci?? Expressiva"
)

# barplots 
qplot(expNorm, data=d1, fill=gender, binwidth=0.1, xlab="Identificaci?? Expressiva", ylab = "N??mero d'observacions", geom = "histogram")
qplot(expNorm, data=d1, fill=gender, binwidth=0.1, xlab="Identificaci?? Expressiva", ylab = "N??mero d'observacions", geom = "histogram")


# density plot
qplot(expNorm, data=d1, color=gender, binwidth=0.1, xlab="Score Identificaci?? Expressiva", ylab = "Proporci?? d'observacions", geom = "density")


qplot(factor(origin), data=d1, fill=gender, binwidth=0.1, xlab="Score Identificaci?? Expressiva", ylab = "N??mero d'observacions")
qplot(factor(gender), expNorm, data = d1, geom = "jitter", color=origin)

# whisker plot

#qplot(gender, instNorm, data = d1, 
     # geom=c("boxplot"), 
     # stackdir = "center", binaxis = "y",
     # fill=origin,
     # ylab = "Identificaci?? Instrumental"
)


qplot(gender, expNorm, data = d1, 
      geom=c("boxplot"), 
      stackdir = "center", binaxis = "y",
      fill=origin,
      ylab = "Identificaci?? Expressiva"
      )


############ IMPLICACI?? FAMILIES ##################################

d2 <- subset(d, select = c("gender",
                           "curs",
                           "speak.at.home",
                           "tutoring.from.family",
                           "afterschool"))

plot(d2$tutoring.from.family,d2$speak.at.home, col=c("steelblue1","steelblue2", "steelblue3"))

plot(d2$afterschool,d2$speak.at.home, col=c("steelblue1", "steelblue3"))


################## ORIENTACI?? #######################################


d3 <- subset(d, select = c("gender",
                           "curs",
                           "speak.at.home",
                           "afterESO.work" ,
                           "afterESO.CF" ,
                           "get.goodmarks",
                           "afterESO.batxi",
                           "want.to.go.to.uni",   
                           "teachers.advice",
                           "parents.advice"))


# reorder levels
d3$want.to.go.to.uni <- factor(d3$want.to.go.to.uni,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d3$afterESO.work <- factor(d3$afterESO.work,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d3$afterESO.CF <- factor(d3$afterESO.CF,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )
d3$afterESO.batxi<- factor(d3$afterESO.batxi,levels=c("Gens d'acord","Poc d'acord", "D'acord", "Molt d'acord") )

# convert to integer
d3$afterESO.work <- as.integer(d3$afterESO.work)
d3$get.goodmarks <- as.integer(d3$get.goodmarks)




qplot(teachers.advice, parents.advice, color=factor(curs),data=d3, geom = "jitter" , size=1)
qplot(teachers.advice, parents.advice, color=get.goodmarks,data=d3, geom = "jitter" , size=1)
qplot(teachers.advice, parents.advice, color=factor(gender),data=d3, geom = "jitter" , size=1)
qplot(teachers.advice, parents.advice, color=factor(speak.at.home),data=d3, geom = "jitter" , size=1)
qplot(teachers.advice, parents.advice, color=factor(want.to.go.to.uni),data=d3, geom = "jitter" , size=1)



qplot(gender, afterESO.work, data = d3, 
      geom=c("boxplot"), 
      stackdir = "center", binaxis = "y",
      fill=origin,
      ylab = "Work After ESO"
)


plot(d3$afterESO.CF,d3$gender, col=c("steelblue1", "steelblue3"), main="AfterESO WORK")

plot(d3$afterESO.work,d3$speak.at.home, col=c("steelblue1", "steelblue3"), main="Want to go to Uni")

plot(d3$want.to.go.to.uni,d3$gender, col=c("steelblue1","steelblue2", "steelblue3"), main="Want to go to Uni")



# Pie Charts
# they will be done with excel

setwd("~/Desktop/Plots/csv/")


#df_WORK <- subset(d, afterESO.work=="D'acord" |afterESO.work=="Molt d'acord" )
#df_CF <- subset(d, afterESO.CF=="D'acord" |afterESO.CF=="Molt d'acord" )
#df_batxi <- subset(d, afterESO.batxi=="D'acord" |afterESO.batxi=="Molt d'acord" )

df_WORK <- subset(d3, afterESO.work=="Molt d'acord" )
df_CF <- subset(d3, afterESO.CF=="Molt d'acord" )
df_batxi <- subset(d3, afterESO.batxi=="Molt d'acord" )
df_batxi <- subset(d3, afterESO.batxi=="Molt d'acord" )


## table the results to feed to excel

table(df_WORK$gender)
table(df_WORK$speak.at.home)
table(df_WORK$curs)
table(df_WORK$get.goodmarks)

table(df_CF$gender)
table(df_CF$speak.at.home)
table(df_CF$curs)
table(df_CF$get.goodmarks)

table(df_batxi$gender)
table(df_batxi$speak.at.home)
table(df_batxi$curs)
table(df_batxi$get.goodmarks)


# test how many people replied that that they strongly agree with 
# looking for a job after school and NOT going for a Cicle formatiu
# or batxillerat
test <- subset(d3, afterESO.work=="Molt d'acord" & !(afterESO.CF=="Molt d'acord"| afterESO.batxi=="Molt d'acord") )
# only 8 satisfy this criteria


# export dataframe to csv
write.csv(df_WORK,"work.csv", row.names = FALSE)
write.csv(df_CF,"CF.csv", row.names = FALSE)
write.csv(df_batxi,"batxi.csv", row.names = FALSE)

