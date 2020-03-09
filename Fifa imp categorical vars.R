#exploring Categorical variables using aov() --Ruichen
fifa_data <- read.delim("fifa.txt",encoding ="UTF-8",sep = "，",header = TRUE)
head(fifa_data)
colnames(fifa_data)[1] = 'Wage'
colnames(fifa_data)[7] = 'Reputation'

head(fifa_data)

table(fifa_data$Work.Rate)
unique(fifa_data$Work.Rate)
library(corrplot)
# nation, club, work.rate, position, body type, preferred foot
plot(fifa_data$Work.Rate,fifa_data$Wage)
aov1 = aov(fifa_data$Wage ~ fifa_data$Work.Rate, fifa_data$Position,fifa_data$Body.Type,fifa_data$Preferred.Foot)
summary(aov1)
aov2 = aov(fifa_data$Wage ~ fifa_data$Body.Type)
summary(aov2)
aov3 = aov(fifa_data$Wage ~ fifa_data$Preferred.Foot)
summary(aov3)
aov4 = aov(fifa_data$Wage ~ fifa_data$Position)
summary(aov4)


nation_data <- read.delim("fifa2.txt",encoding ="UTF-8",sep = "，",header = TRUE)
head(nation_data)
aov5 = aov(nation_data$Wage ~ nation_data$Nationality)
summary(aov5)

table(nation_data$Nationality)


dictionary_data <- read.csv("country_csv.csv",encoding ="UTF-8",header = TRUE)
head(dictionary_data)

# for (val in nation_data$Nationality){
#   if (val %in% dictionary_data$Country_Name){
#     #print(dictionary_data$Continent_Name[which(val %in% dictionary_data$Country_Name==TRUE)])
#     print(which(val %in% dictionary_data$Country_Name))
#     val = dictionary_data$Continent_Name[which(val %in% dictionary_data$Country_Name==TRUE)]
#   }
#   
# }


library(countrycode)
df <- data.frame(country = nation_data$Nationality)
head(df)
df$continent <- countrycode(sourcevar = df[, "country"],
                            origin = "country.name",
                            destination = "continent")
df[is.na(df$continent),]
head(df)
summary(df)
table(df$continent)

new <- read.delim("fifa_nation.txt",encoding ="UTF-8",sep = "，",header = TRUE)
head(new)

aov4 = aov(new$X.U.FEFF.Wage ~ new$Club)
summary(aov4)
