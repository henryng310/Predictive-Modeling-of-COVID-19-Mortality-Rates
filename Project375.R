#Part 1: Data Wrangling
library(tidyverse)
covid_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
world_data <- read_csv("WorldBankData.csv")
view(world_data)
view(covid_data)
#data wrangling steps b-c for world_data
world_data <- world_data %>% filter(nchar(`Country Code`) == 3)
world_data <- world_data %>% mutate(`2023 [YR2023]` = na_if(`2023 [YR2023]`, ".."))
view(world_data)
wide_data <- world_data %>% pivot_wider(names_from = `Series Code`, values_from = `2023 [YR2023]`, values_fn = list(`2023 [YR2023]` = as.numeric))
wide_data <- wide_data %>% mutate(Population_total = as.numeric(SP.POP.TOTL))
view(wide_data)
wide_data <- wide_data %>% select(-`Series Name`)
view(wide_data)
tidy_data <- wide_data %>% group_by(`Country Code`) %>% summarize(across(everything(), ~first(.x[!is.na(.x)])), .groups = 'drop')
view(tidy_data)
#filter out countries whose total pop is <1mill
tidy_data_million <- tidy_data %>% filter(Population_total >= 1000000)
view(tidy_data_million)

#data wrangling steps b-c for covid_data
covid_data_iso <- covid_data %>% filter(nchar(iso_code) == 3)
view(covid_data_iso)
#filter out countries whose total pop is <1mill
covid_data_million <- covid_data_iso %>% filter(population >= 1000000)
view(covid_data_million)
#step d new_deaths_smoothed_2wk
covid_data_2wk <- covid_data_million %>% arrange(iso_code, date) %>% group_by(iso_code) %>% mutate(new_deaths_smoothed_2wk = lead(new_deaths_smoothed, 14)) %>% ungroup()
view(covid_data_2wk)

#step e and f, tidying and merging tables
tidy_data_renamed <-   rename(tidy_data_million, iso_code = `Country Code`, population = Population_total)
view(tidy_data_renamed)
final_data <- inner_join(covid_data_2wk, tidy_data_renamed, by = "iso_code")
merged_data <- final_data %>% select(iso_code, location, population = population.x, everything(), -`population.y`, -`Country Name`) %>% relocate(new_deaths_smoothed_2wk, .after = date)
view(merged_data)
#back up merged_data:
write.csv(merged_data, file = "mergeddata.csv")

#Part 2: Linear modeling
#Step a)list of all columns:
#iso_code	location	population	continent	date	new_deaths_smoothed_2wk	total_cases	new_cases	new_cases_smoothed	total_deaths	new_deaths	new_deaths_smoothed	total_cases_per_million	new_cases_per_million	new_cases_smoothed_per_million	total_deaths_per_million	new_deaths_per_million	new_deaths_smoothed_per_million	reproduction_rate	icu_patients	icu_patients_per_million	hosp_patients	hosp_patients_per_million	weekly_icu_admissions	weekly_icu_admissions_per_million	weekly_hosp_admissions	weekly_hosp_admissions_per_million	total_tests	new_tests	total_tests_per_thousand	new_tests_per_thousand	new_tests_smoothed	new_tests_smoothed_per_thousand	positive_rate	tests_per_case	tests_units	total_vaccinations	people_vaccinated	people_fully_vaccinated	total_boosters	new_vaccinations	new_vaccinations_smoothed	total_vaccinations_per_hundred	people_vaccinated_per_hundred	people_fully_vaccinated_per_hundred	total_boosters_per_hundred	new_vaccinations_smoothed_per_million	new_people_vaccinated_smoothed	new_people_vaccinated_smoothed_per_hundred	stringency_index	population_density	median_age	aged_65_older	aged_70_older	gdp_per_capita	extreme_poverty	cardiovasc_death_rate	diabetes_prevalence	female_smokers	male_smokers	handwashing_facilities	hospital_beds_per_thousand	life_expectancy	human_development_index	excess_mortality_cumulative_absolute	excess_mortality_cumulative	excess_mortality	excess_mortality_cumulative_per_million	SP.POP.80UP.FE	SP.POP.80UP.MA	SP.POP.TOTL	SP.POP.TOTL.MA.IN	SP.POP.TOTL.FE.IN
#list of predictor variables:
  #Epidemiological Variables: total_cases new_cases new_cases_smoothed total_deaths new_deaths new_deaths_smoothed reproduction_rate
  #Healthcare Capacity: icu_patients icu_patients_per_million hosp_patients hosp_patients_per_million hospital_beds_per_thousand
  #Testing & Vaccination Rates: total_tests new_tests new_tests_smoothed positive_rate total_vaccinations people_vaccinated people_fully_vaccinated total_boosters new_vaccinations_smoothed
  #Demographic and Social Metrics: population_density median_age aged_65_older aged_70_older extreme_poverty diabetes_prevalence cardiovasc_death_rate life_expectancy human_development_index
  #Behavioral Indicators: stringency_index handwashing_facilities female_smokers male_smokers
  #Economic Indicators: gdp_per_capita

#Step b) Generate 3 transformed variables:
#Cardiovascular Death Rate Adjusted for Population
merged_data$cardiovasc_deaths = merged_data$cardiovasc_death_rate * merged_data$population
#ICU Demand Rate: Combining ICU patients with the total cases to understand pressure on healthcare
merged_data$icu_demand_rate = merged_data$icu_patients / merged_data$total_cases
#Adjusted Vaccination Rate: Combining total vaccinations with the population to get a per capita rate
merged_data$vaccination_rate_per_capita = merged_data$total_vaccinations / merged_data$population

#Step c) split dataset into train and test
train_data_2022 <- merged_data[format(merged_data$date, "%Y") == "2022", ]
test_data_2023 <- merged_data[format(merged_data$date, "%Y") == "2023", ]

#Step d) Run linear regression with 5 different combos of predictor variables
#Model 1: Basic Epidemiological Model
model1 <- lm(new_deaths_smoothed_2wk ~ new_cases_smoothed + total_deaths_per_million + reproduction_rate, data = train_data_2022)
summary(model1)
#Model 2: Health System Capacity Model
model2 <- lm(new_deaths_smoothed_2wk ~ icu_patients + hospital_beds_per_thousand + icu_demand_rate, data = train_data_2022)
summary(model2)
#Model 3: Socioeconomic Model
model3 <- lm(new_deaths_smoothed_2wk ~ gdp_per_capita + diabetes_prevalence + population_density, data = train_data_2022)
summary(model3)
#Model 4: Comprehensive Model Including Transformations
model4 <- lm(new_deaths_smoothed_2wk ~ new_cases_smoothed + gdp_per_capita + diabetes_prevalence + icu_patients + cardiovasc_deaths, data = train_data_2022)
summary(model4)
#Model 5: Vaccination Impact Model
model5 <- lm(new_deaths_smoothed_2wk ~ vaccination_rate_per_capita + new_cases_smoothed + total_boosters_per_hundred, data = train_data_2022)
summary(model5)

#Part 3: Evaluating the linear models
#Step a) calculate RMSE over all days in Jan-June 2023 and all countries
library(modelr)

predictions_model1 <- predict(model1, newdata = test_data_2023)
residuals_model1 <- test_data_2023$new_deaths_smoothed_2wk - predictions_model1
rmse_model1 <- sqrt(mean(residuals_model1^2, na.rm = TRUE))
print(paste("RMSE for Model 1:", rmse_model1))
#"RMSE for Model 1: 214.667084032869"

predictions_model2 <- predict(model2, newdata = test_data_2023)
residuals_model2 <- test_data_2023$new_deaths_smoothed_2wk - predictions_model2
rmse_model2 <- sqrt(mean(residuals_model2^2, na.rm = TRUE))
print(paste("RMSE for Model 2:", rmse_model2))
#"RMSE for Model 2: 30.4069361249312"

predictions_model3 <- predict(model3, newdata = test_data_2023)
residuals_model3 <- test_data_2023$new_deaths_smoothed_2wk - predictions_model3
rmse_model3 <- sqrt(mean(residuals_model3^2, na.rm = TRUE))
print(paste("RMSE for Model 3:", rmse_model3))
#"RMSE for Model 3: 84.5334791957253"

predictions_model4 <- predict(model4, newdata = test_data_2023)
residuals_model4 <- test_data_2023$new_deaths_smoothed_2wk - predictions_model4
rmse_model4 <- sqrt(mean(residuals_model4^2, na.rm = TRUE))
print(paste("RMSE for Model 4:", rmse_model4))
#"RMSE for Model 4: 31.2089611214389"

predictions_model5 <- predict(model5, newdata = test_data_2023)
residuals_model5 <- test_data_2023$new_deaths_smoothed_2wk - predictions_model5
rmse_model5 <- sqrt(mean(residuals_model5^2, na.rm = TRUE))
print(paste("RMSE for Model 5:", rmse_model5))
#"RMSE for Model 5: 105.998463475542"

#Step b) for only the best model, calculate RMSE for every country
test_data_2023$model2_predictions <- predict(model2, newdata = test_data_2023)
test_data_2023$residuals_model2 <- test_data_2023$new_deaths_smoothed_2wk - test_data_2023$model2_predictions
country_rmse_model2 <- test_data_2023 %>% group_by(iso_code) %>% summarise(rmse = sqrt(mean(residuals_model2^2, na.rm = TRUE)),
                                                                           +         .groups = 'drop')
#Above gives NaN values


#Scatterplots:
#First scatterplot  
specific_date_data <- test_data_2023 %>% filter(date == as.Date("2023-06-30")) %>% filter(!is.na(new_deaths_smoothed_2wk) & !is.na(new_cases_smoothed)) %>% group_by(iso_code) %>%
summarise(new_deaths_smoothed_2wk = last(new_deaths_smoothed_2wk), new_cases_smoothed = last(new_cases_smoothed), .groups = 'drop' )
if(nrow(specific_date_data) > 0) { scatter_plot <- ggplot(specific_date_data, aes(x = new_cases_smoothed, y = new_deaths_smoothed_2wk)) +
    geom_point() +labs( title = "Scatterplot of New Deaths vs. New Cases on 2023-06-30",x = "New Cases Smoothed",y = "New Deaths Smoothed Two Weeks Ahead") +
     theme_minimal() 
print(scatter_plot) } else { print("No data available for plotting.")}

#second scatterplot
specific_date_data <- test_data_2023 %>% filter(date == as.Date("2023-06-30")) %>% filter(!is.na(new_deaths_smoothed) & !is.na(SP.POP.80UP.FE) & !is.na(SP.POP.80UP.MA)) %>%
  group_by(iso_code) %>% summarise(new_deaths_smoothed = last(new_deaths_smoothed),
   total_population_over_80 = last(SP.POP.80UP.FE + SP.POP.80UP.MA), .groups = 'drop' )
 if(nrow(specific_date_data) > 0) {scatter_plot <- ggplot(specific_date_data, aes(x = total_population_over_80, y = new_deaths_smoothed)) +
   geom_point() +labs(title = "Scatterplot of New Deaths vs. Total Population Over 80 on 2023-06-30",
     x = "Total Population Over 80",
     y = "New Deaths Smoothed") + theme_minimal()
    print(scatter_plot) } else {print("No data available for plotting.")}


#Top 20 countries

test_data_2023$model2_predictionsTesting <- predict(model2, newdata = test_data_2023)
test_data_2023$residuals_model2Testing <- test_data_2023$new_deaths_smoothed_2wk - test_data_2023$model2_predictionsTesting

country_rmse_model2Testing <- test_data_2023 %>% filter(!is.na(new_deaths_smoothed_2wk) & !is.na(model2_predictionsTesting)) %>% 
 group_by(iso_code) %>%summarise(rmse = sqrt(mean((new_deaths_smoothed_2wk - model2_predictionsTesting)^2, na.rm = TRUE)), .groups = 'drop')
view(country_rmse_model2Testing)

rmse_population_data <- test_data_2023 %>% filter(!is.na(new_deaths_smoothed_2wk) & !is.na(model2_predictionsTesting)) %>%
  group_by(iso_code) %>% summarise(rmse = sqrt(mean((new_deaths_smoothed_2wk - model2_predictionsTesting)^2, na.rm = TRUE)),
   population = last(population), .groups = 'drop')

top_20_countries_by_population <- rmse_population_data %>% arrange(desc(population)) %>% slice_head(n = 20)

view(top_20_countries_by_population)




