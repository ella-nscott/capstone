#install.packages(c("sf", "dplyr"))
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("stringr)

library(sf)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)

#create race demographic layer for ramsey and hennepin counties
#load in tables and existing layer
shp <- st_read("/cloud/project/tracts_henn_rams.shp")
race <- read.csv("/cloud/project/race_table2.csv")
tracts <- read.csv("/cloud/project/tracts_table.csv")

shp$GEOIDn <- as.numeric(shp$GEOID)
#join table and layer
join <- left_join(shp, race, by = c("GEOIDn"= "GEOID"))

#test join
test <- join$GEOID - join$GEOIDt2

#create new shapefile
st_write(join,"/cloud/project/demographics/race.shp")

#create population layer 
#load in table
pop <- read.csv("/cloud/project/population.table.csv")
#join table and tracts layer
join.pop <- left_join(shp, pop, by = c("GEOIDn" = "GEOID"))
#new shapefile
st_write(join.pop, "/cloud/project/population/pop.shp")

#create income layer
#load in table
income <- read.csv("/cloud/project/income2table.csv")
#join table and tracts layer
join.inc <- left_join(shp, income, by = c("GEOIDn" = "GEOID"))
#remove missing data
incomef <- join.inc %>%
  filter(!Med_Inc=="-")
#new shapefile
st_write(incomef, "/cloud/project/income/income.shp")

#create poverty layer
#load in table
poverty <- read.csv("/cloud/project/pov_hr_table.csv")
#join table and tracts layer
join.pov <- left_join(shp, poverty, by = c("GEOIDn" = "GEOID"))
#remove missing data
povertyf <- join.pov %>%
  filter(!Pct_blw=="-")
#new shapefile
st_write(povertyf, "/cloud/project/poverty/poverty.shp")

#box plot with demographic data
#create reformatted data table
#remove unneeded columns
race2 <- race
race2 <- subset(race2, select=-c(GEO_ID, NAME, Total_pop, tWhite, 
                                 tBl_AfAm, tAmIn_AlNa, tAsian, tNaHa_PaIs,
                                 tOther, tMultiple, GEOIDt, GEOIDt2))
#move percentages to one column
re.race <- melt(race2, id="GEOID")

#create boxplot
ggplot(re.race, aes(x=GEOID, y=value, color=variable))+
  geom_boxplot()

#box plot: demographic data for census tracts with data centers
#subset of table that only includes tracts with data centers
centers <- re.race %>%
  filter(GEOID=="27053024003" | GEOID=="27053026006" | GEOID=="27053026007" |
         GEOID=="27053026104" | GEOID=="27053026403" | GEOID=="27053026507" |
         GEOID=="27053026812" | GEOID=="27053104002" | GEOID=="27053104400" |
         GEOID=="27053126102" | GEOID=="27053126202" | GEOID=="27123030202" |
         GEOID=="27123031600" | GEOID=="27123034201" | GEOID=="27123041107")
#create boxplot
ggplot(centers, aes(x=GEOID, y=value, color=variable))+
  geom_boxplot()

#income boxplot: comparing all tracts and tracts with centers
#create table that isolates tracts
income.centers <- income %>%
  filter(GEOID=="27053024003" | GEOID=="27053026006" | GEOID=="27053026007" |
           GEOID=="27053026104" | GEOID=="27053026403" | GEOID=="27053026507" |
           GEOID=="27053026812" | GEOID=="27053104002" | GEOID=="27053104400" |
           GEOID=="27053126102" | GEOID=="27053126202" | GEOID=="27123030202" |
           GEOID=="27123031600" | GEOID=="27123034201" | GEOID=="27123041107")
#create joined data frame 
income$tracts <- rep("all", 472)
income.centers$tracts <- rep("centers", 15)
full.income <- rbind(income, income.centers)
#create boxplot
ggplot(full.income, aes(x=tracts, y=as.numeric(Med_Inc)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Median Income")+
  ggtitle("Median Income of Households Lower in Census Tracts with Data Centers")

#poverty boxplots
#create table that isolates tracts
poverty.centers <- poverty %>%
  filter(GEOID=="27053024003" | GEOID=="27053026006" | GEOID=="27053026007" |
           GEOID=="27053026104" | GEOID=="27053026403" | GEOID=="27053026507" |
           GEOID=="27053026812" | GEOID=="27053104002" | GEOID=="27053104400" |
           GEOID=="27053126102" | GEOID=="27053126202" | GEOID=="27123030202" |
           GEOID=="27123031600" | GEOID=="27123034201" | GEOID=="27123041107")
#create joined data frame
poverty$tracts <- rep("all", 472)
poverty.centers$tracts <- rep("centers", 15)
full.poverty <- rbind(poverty, poverty.centers)
#create boxplot
#axis labels
poverty.labels <- c("All Census \nTracts", "Tracts Containing \nData Centers")
ggplot(full.poverty, aes(x=tracts, y=as.numeric(Pct_blw)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Percent Below Poverty")+
  ggtitle("Percentage of Population Below Poverty Higher in Census Tracts with 
          Data Centers")

#race boxplots side by side
#joined data frame
re.race$tracts <- rep("all", 3304)
centers$tracts <- rep("centers", 105)
full.race <- rbind(re.race, centers)
#create labels for x axis
race.labels <- c("White", "Black or \nAfrican American", "American Indian \nand Alaska Native", 
                 "Asian", "Native Hawaiian \nand Other Pacific Islander", "Other Race", "Multiple Races")
#create boxplot
ggplot(full.race, aes(x=variable, y=value, color=tracts))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=6))+
  scale_x_discrete(labels=race.labels)+
  xlab("Race")+
  ylab("Percent of Population")

#comparing Hennepin and Ramsey Counties
#poverty
#isolate hennepin
hennepin.pov <- full.poverty %>%
  filter(str_detect(GEOID, "^2705"))
hennepin.pov$County <- rep("Hennepin", 340)

ggplot(hennepin.pov, aes(x=tracts, y=as.numeric(Pct_blw)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Percent Below Poverty")

#isolate ramsey
ramsey.pov <- full.poverty %>%
  filter(str_detect(GEOID, "^2712"))
ramsey.pov$County <- rep("Ramsey", 147)

ggplot(ramsey.pov, aes(x=tracts, y=as.numeric(Pct_blw)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Percent Below Poverty")

#put boxplots on same plot
#combine dataframes
full.poverty2 <- rbind(hennepin.pov, ramsey.pov)
#create joined boxplot
ggplot(full.poverty2, aes(x=tracts, y=as.numeric(Pct_blw), fill=tracts))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  xlab("")+
  ylab("Percent Below Poverty")+
  scale_x_discrete(labels=poverty.labels)+
  facet_grid(~County)

#income
#isolate hennepin
hennepin.inc <- full.income %>%
  filter(str_detect(GEOID, "^2705"))
hennepin.inc$County <- rep("Hennepin", 340)

ggplot(hennepin.inc, aes(x=tracts, y=as.numeric(Med_Inc)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Median Income")

#ramsey

ramsey.inc <- full.income %>%
  filter(str_detect(GEOID, "^2712"))
ramsey.inc$County <- rep("Ramsey", 147)

ggplot(ramsey.inc, aes(x=tracts, y=as.numeric(Med_Inc)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Median Income")

#put boxplots on same plot
#combine data frames
full.inc.2 <- rbind(hennepin.inc, ramsey.inc)
#create joined boxplot
ggplot(full.inc.2, aes(x=tracts, y=as.numeric(Med_Inc), fill=tracts))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Median Income")+
  facet_grid(~County)

#race
#hennepin
hennepin.race <- full.race %>%
  filter(str_detect(GEOID, "^2705"))
hennepin.race$County <- rep("Hennepin", 340)


ggplot(hennepin.race, aes(x=variable, y=value, color=tracts))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=6))+
  scale_x_discrete(labels=race.labels)+
  xlab("Race")+
  ylab("Percent of Population")

#ramsey
ramsey.race <- full.race %>%
  filter(str_detect(GEOID, "^2712"))
ramsey.race$County <- rep("Ramsey", 147)

ggplot(ramsey.race, aes(x=variable, y=value, color=tracts))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=6))+
  scale_x_discrete(labels=race.labels)+
  xlab("Race")+
  ylab("Percent of Population")

#put boxplots on one plot
#combine dataframes
race.full.2 <- rbind(hennepin.race, ramsey.race)
#create joined plot
ggplot(race.full.2, aes(x=variable, y=value, color=tracts))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=6))+
  scale_x_discrete(labels=race.labels)+
  xlab("Race")+
  ylab("Percent of Population")+
  facet_grid(rows = vars(County))

#analyze population trends
#isolate census tracts with data centers
pop.centers <- pop %>%
  filter(GEOID=="27053024003" | GEOID=="27053026006" | GEOID=="27053026007" |
         GEOID=="27053026104" | GEOID=="27053026403" | GEOID=="27053026507" |
         GEOID=="27053026812" | GEOID=="27053104002" | GEOID=="27053104400" |
         GEOID=="27053126102" | GEOID=="27053126202" | GEOID=="27123030202" |
         GEOID=="27123031600" | GEOID=="27123034201" | GEOID=="27123041107")
#add identifying columns
pop$tracts <- rep("all", 472)
pop.centers$tracts <- rep("centers", 15)
#join dataframes
pop.full <- rbind(pop, pop.centers)
#create boxplot
ggplot(pop.full, aes(x=tracts, y=TPOP))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Total Population")
#compare hennepin and ramsey
#hennepin
hennepin.pop <- pop.full %>%
  filter(str_detect(GEOID, "^2705"))
hennepin.pop$County <- rep("Hennepin", 340)
#look at boxplot
ggplot(hennepin.pop, aes(x=tracts, y=TPOP))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Total Population")

#ramsey
ramsey.pop <- pop.full %>%
  filter(str_detect(GEOID, "^2712"))
ramsey.pop$County <- rep("Ramsey", 147)
#look at boxplot
ggplot(ramsey.pop, aes(x=tracts, y=TPOP))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Total Population")
#put boxplots on one plot
#combine dataframes
pop.full.2 <- rbind(hennepin.pop, ramsey.pop)
#create joined plot
ggplot(pop.full.2, aes(x=tracts, y=TPOP, fill=tracts))+
  geom_boxplot()+
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Total Population")+
  facet_grid(~County)

#create violin plots for presentation
#income: counties combined
ggplot(full.income, aes(x=tracts, y=as.numeric(Med_Inc), fill=tracts))+
  geom_violin(width=1.0) +
  geom_boxplot(width=0.15, color="white", alpha=0.2) +
  scale_x_discrete(labels=poverty.labels)+
  theme(axis.text.x = element_text(size=12, color="black"))+
  xlab("")+
  ylab("Median Income")+
  theme_minimal()

#income: comparing ramsey and hennepin
ggplot(full.inc.2, aes(x=tracts, y=as.numeric(Med_Inc), fill=tracts))+
  geom_violin(width=1.0) +
  geom_boxplot(width=0.15, color="white", alpha=0.2) +
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Median Income")+
  facet_grid(~County)

#poverty: counties combined
ggplot(full.poverty2, aes(x=tracts, y=as.numeric(Pct_blw), fill=tracts))+
  geom_violin()+
  geom_boxplot(width=0.15, color="white", alpha=0.2) +
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Percent Below Poverty")+
  theme_minimal()

#poverty: comparing ramsey and hennepin
ggplot(full.poverty2, aes(x=tracts, y=as.numeric(Pct_blw), fill=tracts))+
  geom_violin()+
  geom_boxplot(width=0.15, color="white", alpha=0.2) +
  theme(axis.text.x = element_text(size=10, color="black"))+
  scale_x_discrete(labels=poverty.labels)+
  xlab("")+
  ylab("Percent Below Poverty Threshold")+
  facet_grid(~County)

#race: counties combined
#isolate key groups
race.subset <- race.full.2 %>%
  filter(variable=="pWhite"|variable=="pBl_AfAm"|variable=="pAsian")
#create boxplot
race.sub.labels <- c("White", "Black or \nAfrican American", "Asian")

ggplot(race.subset, aes(x=variable, y=value))+
  geom_violin(aes(fill = tracts), trim = FALSE, position = position_dodge(0.8) ) +
  geom_boxplot(aes(color = tracts), width = 0.09, position = position_dodge(0.8))+
  scale_x_discrete(labels=race.sub.labels)+
  xlab("")+
  ylab("Percent of Population")+
  theme_minimal()

#full boxplot
ggplot(race.full.2, aes(x=variable, y=value))+
  geom_violin(aes(fill = tracts), trim = FALSE, position = position_dodge(0.8) ) +
  geom_boxplot(aes(color = tracts), width = 0.09, position = position_dodge(0.8))+
  scale_x_discrete(labels=race.sub.labels)+
  xlab("")+
  ylab("Percent of Population")+
  theme_minimal()

#stat summaries

#income
#counties together
income.box <- boxplot(as.numeric(full.income$Med_Inc) ~ tracts, data = full.income)
income.box
#counties individually
#hennepin
henn.inc.box <- boxplot(as.numeric(hennepin.inc$Med_Inc) ~ tracts, data=hennepin.inc)
#ramsey
rams.inc.box <- boxplot(as.numeric(ramsey.inc$Med_Inc) ~ tracts, data = ramsey.inc)

#poverty
#counties together
poverty.box <- boxplot(as.numeric(full.poverty$Pct_blw) ~ tracts, data = full.poverty)
poverty.box
#counties individually
#hennepin
henn.pov.box <- boxplot(as.numeric(hennepin.pov$Pct_blw) ~ tracts, data = hennepin.pov)
#ramsey 
rams.pov.box <- boxplot(as.numeric(ramsey.pov$Pct_blw) ~ tracts, data = ramsey.pov)

#race
#counties together
race.box.all <- boxplot(as.numeric(full.race$value) ~ variable, data = full.race)
race.box.centers <- boxplot(as.numeric(centers$value) ~ variable, data = centers)
#counties 
#hennepin 
#split tracts into groups
henn.race.all <- hennepin.race %>%
  filter(tracts=="all")
henn.race.cntrs <- hennepin.race %>%
  filter(tracts=="centers")
#calculate stats
henn.race.all.box <- boxplot(as.numeric(henn.race.all$value) ~ variable, data = henn.race.all) 
henn.race.cntrs.box <- boxplot(as.numeric(henn.race.cntrs$value) ~ variable, data = henn.race.cntrs) 
#ramsey
#split tracts into groups
rams.race.all <- ramsey.race %>%
  filter(tracts=="all")
rams.race.cntrs <- ramsey.race %>%
  filter(tracts=="centers")
#calculate stats
rams.race.all.box <- boxplot(as.numeric(rams.race.all$value) ~ variable, data = rams.race.all)
rams.race.cntrs.box <- boxplot(as.numeric(rams.race.cntrs$value) ~ variable, data = rams.race.cntrs)   



#geographic analysis results

#build data frame
num <- c(9, 11, 6, 1, 8)
type <- c("Downtown", "In/Near Residential Area", "Industrial", "Agricultural", "Commercial")
geo_table <- data.frame(Number = num,
                        Type = type)

#create bar graph
ggplot(geo_table, aes(x=Type, y=Number, fill=Type))+
  geom_bar(stat="identity")+
  theme_minimal()+
  xlab("")+
  theme(axis.text.x = element_text(size=10, color="black"))
