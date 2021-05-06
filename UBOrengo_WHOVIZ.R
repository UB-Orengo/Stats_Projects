library(tidyr)
library(tidyverse)
library(dplyr)
library(countrycode)

tidyr::who

who5 <-who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

print(who5)

## Removed Serbia and Serbia & Montenegro from DF, further in the document, I created a df for continent 
## and cbind'd it to the cleaned WHO data, but it was unable to id those countries. I was also unable to replace the NA values.
## because it represented such a small subset, I removed them

who5_clean <- who5[!(who5$country =="Serbia" | who5$country == "Serbia & Montenegro"),]

##Converted it back to who5 DF
who5 <- who5_clean

##Created continents list using package "countrycode"
continents <- countrycode(sourcevar = who5$country, 
                          origin = "country.name", 
                          destination = "continent")

##Converted that into a DF
continents_df <- data.frame(continent = continents)

print(continents_df)

##Bound that DF with the rest of the dataframe for who5
who5_with_continent <- cbind(continents_df,who5)

print(who5_with_continent)

##summarized the data by continent, year, and sex using group_by & summarize from the dplyr package
who_summ <- who5_with_continent %>%
  group_by(continent,year, sex) %>%
  summarise(number_of_cases = sum(cases))

print(who_summ)

##plotted the data, using x axis for year and y axis for number of cases. I color coded it with the continent variable
##and created two linetypes for sex
who_viz <- ggplot(data = who_summ, mapping = aes(x = year, y = number_of_cases, color = continent)) + 
            geom_line(aes(linetype = sex)) +
            geom_point()

##printed the plot, added title and cleaned up labels
print(who_viz + ggtitle("Tuberculosis Cases, By Region and Assigned Sex At Birth \nfrom 1980 to 2013") +labs(x = "Year", y = "Number Of Cases"))
