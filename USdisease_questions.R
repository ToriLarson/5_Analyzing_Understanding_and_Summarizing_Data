## 1. What was the measles total count for Hawaii vs the United states average in 1960.

## 1A. Total count for Hawaii 1960.

# Contagious disease data
data("us_contagious_diseases")

# Adding to a variable
Diseases <- (us_contagious_diseases)

# getting the year 1960
subset(Diseases,year==1960)
Year_1960 <- subset(Diseases,year==1960)

# Measles for all in 1960
Measles_1960 <- subset(Year_1960,disease=="Measles")
MeaslesHI_1960 <- subset(Measles_1960,state=="Hawaii")
HIMeaslesCount <-MeaslesHI_1960$count

#Measles Count Hawaii
HIMeaslesCount

# Answer
# 5322

#1B. 
USMeasles_avg <- mean(Measles_1960$count)
#Answer
# 8474.922

#2A. Which state had the highest count (in 52 weeks) for each  disease.

# filtering 52 weeks from diseases report
Week52_reporting <- subset(Diseases, weeks_reporting==52)
#filtering out just Measles from the weeks reporting 52
Measles52wks <- subset(Week52_reporting, disease=="Measles")
#doing which.max tells you which line the code is on.
which.max(Measles52wks$count)
#ANSWER!
MeaslesHigh <- Measles52wks[317,]
MeaslesHigh

#filtering out just HEP  A  from the weeks reporting 52
HEPA52wks <- subset(Week52_reporting, disease=="Hepatitis A")
#doing which.max tells you which line the code is on.
which.max(HEPA52wks$count)
#ANSWER!
HEPAHigh <- HEPA52wks[5,]
HEPAHigh

#filtering out just Mumps  from the weeks reporting 52
mumps52wks <- subset(Week52_reporting, disease=="Mumps")
#doing which.max tells you which line the code is on.
which.max(mumps52wks$count)
#ANSWER!
MumpsHigh <- mumps52wks[47,]

#filtering out just Pertussis  from the weeks reporting 52
Pert52wks <- subset(Week52_reporting, disease=="Pertussis")
#doing which.max tells you which line the code is on.
which.max(Pert52wks$count)
#ANSWER!
PertussisHigh <- Pert52wks[164,]

#filtering out just Polio  from the weeks reporting 52
Polio52wks <- subset(Week52_reporting, disease=="Polio")
#doing which.max tells you which line the code is on.
which.max(Polio52wks$count)
#ANSWER!
PolioHigh <- Polio52wks[164,]

#filtering out just Rubella from the weeks reporting 52
Rubella52wks <- subset(Week52_reporting, disease=="Rubella")
#doing which.max tells you which line the code is on.
which.max(Rubella52wks$count)
RubellaHigh <- Rubella52wks[3,]

#filtering out just Small pox  from the weeks reporting 52
Smlpx52wks <- subset(Week52_reporting, disease=="Smallpox")
#doing which.max tells you which line the code is on.
which.max(Smlpx52wks$count)
#ANSWER!
SmallpoxHigh <- Smlpx52wks[163,]
SmallpoxHigh

# thanks ben... 
Diseases %>% subset(weeks_reporting == 52) %>% group_by(disease) %>% summarise(max(count)) 

# messing with ddply - its what I was looking for!
DiseaseHigh <- ddply(Week52_reporting, 'disease', function(x) x[x$count==max(x$count),])
DiseaseHigh

#ggplot for all Diseases 52 Weeks Reporting
ggplot(Week52_reporting, aes(x=year, y=count)) + geom_point()

# Now I can see which disease is which!
Week52plot <- ggplot(Week52_reporting, aes(x=year, y=count, color=disease)) + geom_point()

#getting rid of scientific notation
options(scipen = 10)

# Adding color to the scatterplot
Week52plot <- (Week52plot + scale_color_manual(values = c("royalblue1", "green3", "violet", "red4", "darkorchid", "cyan1","coral")))

#Joining DiseaseHigh and Week52_reporting by disease and year
total <- merge(DiseaseHigh, Week52_reporting, by=c("disease", "year"))

# Creating the table with state counts, US counts, and percentages.
Disease_percent = total %>% group_by(disease) %>% summarise(first(count.x), sum(count.y), first(state.x), first(year)) %>% mutate(percent.of.disease=`first(count.x)`/`sum(count.y)`*100)
Disease_percent

