library(tidyverse)
library(data.table)
library(skimr)
library(inspectdf)
library(modeest)
library(graphics)
library(SparkR)
library(dplyr)
library(hrbrthemes)
library(plotly)
library(ggExtra)
library(tidycharts)

df <- read_csv('Life Expantancy Data.csv')

df %>% inspect_na()
df %>% inspect_types()
df %>% inspect_cor()
df %>% inspect_imb()
df %>% skim()

colnames(df) <- df %>% colnames() %>% tolower()

colnames(df) <- df %>% 
  colnames() %>% 
  gsub('  ',' ',.) %>%
  gsub(' ','_',.) %>%
  gsub('\'','',.) %>% 
  gsub('-','_',.) %>% 
  gsub('/','_',.)

df$country <- df$country %>% 
  gsub(' ','_',.) %>% 
  gsub('\'','',.) %>% 
  gsub('\\(','',.) %>% 
  gsub('\\)','',.) %>% 
  gsub('-','_',.)
  
df[!complete.cases(df),] %>% View()

df[is.na(df$bmi),] %>% view()

# Imputation

for (i in df$country) {
  df[is.na(df$alcohol) & df$country==sprintf("%s",i),'alcohol'] <- 
    df$alcohol %>% mean(na.rm=TRUE)
  df[is.na(df$hepatitis_b) & df$country==sprintf("%s",i),'hepatitis_b'] <- 
    df$hepatitis_b %>% mfv(na_rm=T)
  df[is.na(df$bmi) & df$country==sprintf("%s",i),'bmi'] <- 
    df$bmi %>% mean(na.rm=TRUE)
  df[is.na(df$polio) & df$country==sprintf("%s",i),'polio'] <- 
    df$polio %>% mfv(na_rm=T)
  df[is.na(df$diphtheria) & df$country==sprintf("%s",i),'diphtheria'] <- 
    df$diphtheria %>% mfv(na_rm=T)
  df[is.na(df$total_expenditure) & df$country==sprintf("%s",i),'total_expenditure'] <- 
    df$total_expenditure %>% mean(na.rm=TRUE)
  df[is.na(df$gdp) & df$country==sprintf("%s",i),'gdp'] <- 
    df$gdp %>% mean(na.rm=TRUE)
  df[is.na(df$population) & df$country==sprintf("%s",i),'population'] <- 
    df$population %>% mean(na.rm=TRUE)
  df[is.na(df$thinness_1_19_years) & df$country==sprintf("%s",i),'thinness_1_19_years'] <- 
    df$thinness_1_19_years %>% mfv(na_rm=T)
  df[is.na(df$thinness_5_9_years) & df$country==sprintf("%s",i),'thinness_5_9_years'] <- 
    df$thinness_5_9_years %>% mfv(na_rm=T)
  df[is.na(df$income_composition_of_resources) & df$country==sprintf("%s",i),'income_composition_of_resources'] <- 
    df$income_composition_of_resources %>% mean(na.rm=TRUE)
  df[is.na(df$schooling) & df$country==sprintf("%s",i),'schooling'] <- 
    df$schooling %>% mfv(na_rm=T)
}

# Filter data only related to Azerbaijan

aze <- df %>% dplyr::filter(country=='Azerbaijan')

# Find outliers

numeric_vars <- aze %>% 
  select_if(is.numeric) %>% 
  names()

for (variable in numeric_vars) {
  outliers <- boxplot(aze[[variable]],plot=F)$out
  if(length(outliers)>0){
    print(paste0("Column name: ",variable))
    print(outliers)
  }
}

boxplot(aze[["population"]])
boxplot(aze[["gdp"]])

# First method to correct misinformation on Population and GDP columns

aze$population <- as.character(aze$population)

sparkR.session()

aze <- as.DataFrame(aze)

aze$population <- rpad(aze$population,7,'0')

aze <- as.data.frame(aze)

sparkR.session.stop()

# Second method to correct misinformation on Population and GDP columns

aze$population <- str_pad(aze$population,7,'right',0)

aze$population <-  as.numeric(aze$population)

aze$gdp <- aze$gdp %>% 
  gsub('\\.','',.) %>% 
  as.numeric() %>% 
  str_pad(.,11,'right',0)

aze$gdp <- as.numeric(aze$gdp)

for (variable in numeric_vars) {
  outliers <- boxplot(aze[[variable]],plot=F)$out
  if(length(outliers)>0){
    print(paste0("Column name: ",variable))
    print(outliers)
  }
}

boxplot(aze[["population"]])
boxplot(aze[["gdp"]])

# Visualization

scatterplot <- aze %>% 
  ggplot(aes(year,life_expectancy)) +
  geom_point(color = ft_cols$yellow,
             aes(size=population)) +
  labs(x="Years", 
       y="Life expectancy",
       title="Life expectancy by Year",
       subtitle="Scatterplot") +
  theme_modern_rc()

scatterplot %>% ggplotly()

histogram <- aze %>% 
  ggplot(aes(diphtheria)) + 
  geom_histogram(colour="Black", binwidth=1, center=T) +
  scale_x_continuous(breaks = seq(0,100,1)) +
  scale_y_continuous(breaks = seq(0,100,1))

histogram %>% ggplotly()

aze %>% skim

aze$year <- aze$year %>% as_factor()

aze %>% 
  ggplot(aes(diphtheria)) + 
  geom_density(alpha=0.41, position = 'stack',color="yellow", fill="purple") +
  scale_x_continuous(breaks = seq(0,100,5))

aze %>% 
  ggscatterstats(x = diphtheria,
                 y = life_expectancy,
                 centrality.parameter = "median",
                 messages = F)

aze %>% 
   dumbbell_chart(x1_name=infant_deaths,
                 x2_name=under_five_deaths,
                 y_name=year,
                 line_color="black",
                 x1_color="blue",
                 x2_color="red",
                 show_legend=T,
                 plot_title='Deaths by Years',
                 x_axis_title='Death',
                 y_axis_title='Year')

aze %>% 
  lollipop_chart(x_name=infant_deaths,
                 y_name=year,
                 line_color="green",
                 x_color="darkgreen",
                 show_legend=F,
                 plot_title='Infant deaths',
                 x_axis_title='Death',
                 y_axis_title='Year')
