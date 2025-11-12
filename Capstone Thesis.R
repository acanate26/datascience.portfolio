setwd("C:/Users/acana/OneDrive/Pictures/QMSS CAPSTONE")
require(data.table)
install.packages("tidycensus")
require(tidycensus)
require(ggplot2)
census_api_key("aa512886c5449a582d837da8d3a07af66a043fe5")

census_data <- load_variables(2023, "acs5", cache=T)
View(census_data) 

vars <- c(Total = 'B01003_001',  
          Poverty = 'B05010_001',
          Did_Not_Recieve = 'B22007_023',
          Income = 'B99191_008', 
          Educ = 'C15010F_005',
          medage = 'B01002_001',
          bpop = 'B02001_003',
          wpop = 'B02001_002',
          hpop = 'B03002_012',
          apop = 'B02001_005') 

View(vars)

NY_data <- get_acs(state = "NY", 
                geography = "county",
                variables = vars, 
                geometry = TRUE,
                survey = "acs5",
                output = "wide")
View(NY_data)

library(dplyr)
NY_data_estimates <- NY_data %>%
  select(GEOID, NAME, ends_with("E"))

NY_data_estimates$pct_pov <- (NY_data_estimates$PovertyE/NY_data_estimates$TotalE)*100
summary(NY_data_estimates$pct_pov)
summary(NY_data_estimates)
View(NY_data_estimates)

plot(NY_data_estimates)
plot(NY_data_estimates["TotalE"])
plot(NY_data_estimates["pct_pov"])
plot(NY_data_estimates["medageE"])
plot(NY_data_estimates["EducE"])
plot(NY_data_estimates['bpopE'])

NY_data$geometry <- NULL

NY_data_estimates$incpct <- (NY_data_estimates$IncomeE / NY_data_estimates$TotalE) *100
NY_data_estimates$did_notreceivepct <- (NY_data_estimates$Did_Not_RecieveE / NY_data_estimates$TotalE) *100
NY_data_estimates$agepct <- (NY_data_estimates$medageE / NY_data_estimates$TotalE) *100
NY_data_estimates$povpct <- (NY_data_estimates$pct_pov/NY_data_estimates$TotalE) *100
NY_data_estimates$educpct <- (NY_data_estimates$EducE / NY_data_estimates$TotalE)*100
NY_data_estimates$bpct <- (NY_data_estimates$bpopE / NY_data_estimates$TotalE) *100
NY_data_estimates$wpct <- (NY_data_estimates$wpopE / NY_data_estimates$TotalE) *100

###Add race diversity 
NY_data_estimates$race_div <- 1 - (((NY_data_estimates$wpopE*(NY_data_estimates$wpopE-1))+
                          (NY_data_estimates$bpopE*(NY_data_estimates$bpopE-1))+
                          (NY_data_estimates$hpopE*(NY_data_estimates$hpopE-1))+
                          (NY_data_estimates$apopE*(NY_data_estimates$apopE-1)))/
                         (NY_data_estimates$TotalE*(NY_data_estimates$TotalE-1)))

head(NY_data_estimates)
summary(NY_data_estimates$race_div)
plot(NY_data_estimates["race_div"])

###Bivariate Analysis: Ttests seeing the relation between race, age, education,
###connecting to poverty.
NY_data_estimates$age_group <- ifelse(NY_data_estimates$agepct >
                                        median(NY_data_estimates$agepct, 
                                               na.rm = TRUE), "High", "Low")
Ttest_medage <- t.test(povpct ~ age_group, data = NY_data_estimates)

NY_data_estimates$race_group <- ifelse(NY_data_estimates$race_div >
                                         median(NY_data_estimates$race_div, 
                                                na.rm = TRUE), "High", "Low")
Ttest_race <- t.test(povpct ~ race_group, data = NY_data_estimates)

NY_data_estimates$no_service <- ifelse(NY_data_estimates$did_notreceivepct >
                                         median(NY_data_estimates$did_notreceivepct, 
                                                na.rm = TRUE), "High", "Low")
Ttest_noservice <- t.test(povpct ~ no_service, data = NY_data_estimates)

NY_data_estimates$educ <- ifelse(NY_data_estimates$educpct >
                                         median(NY_data_estimates$educpct,
                                                na.rm = TRUE), "High", "Low")
Ttest_educ <- t.test(povpct ~ educ, data = NY_data_estimates)

Ttest_medage
Ttest_race
Ttest_noservice
Ttest_educ

###Is race and not being able to receive services related to poverty?

ANOVA_noserv <- aov(povpct ~ race_div * no_service, data = NY_data_estimates)
summary(ANOVA_noserv)


####Explore the variables

hist(NY_data_estimates$pct_pov)
hist(NY_data_estimates$race_div)


####Research Question: Is there a significant difference in poverty rates between
##counties with high versus low rates of residents who did not receive services?

require(ggplot2)

NY_data_estimates %>%
  group_by(race_group, no_service) %>%
  summarise(mean_povpct = mean(povpct, na.rm = TRUE)) %>%
  ggplot(aes(x = race_group, y = no_service, fill = mean_povpct)) +
  geom_tile() +
  geom_text(aes(label = round(mean_povpct, 3)), color = "white") +
  labs(
    title = "Mean Poverty Percentage by Race Diversity and Service Access",
    x = "Race Diversity Group",
    y = "Service Access",
    fill = "Poverty %"
  ) +
  theme_minimal() +
  scale_fill_gradient(low = "#2ca02c", high = "#d62728") +
  theme(legend.position = "bottom")

####NYC Counties

neighbor_class <- fread ("Neighborhood_Prioritization_Map_2023.csv", 
                          stringsAsFactors = F, 
                          data.table = F)
names(neighbor_class)
head(neighbor_class)
View(neighbor_class)
neighbor_class <- neighbor_class[,c(2,3,4,6,8,10,13)]

names(neighbor_class)
head(neighbor_class)

summary(neighbor_class$'SNAP Perc Rank along Adj Weighted Score')
summary(neighbor_class$'Vulnerable pop perc rank along Adj Weighted Score')
summary(neighbor_class$'Food insecure percentage rank along Adj Weighted Score')

neighbor_class$foodins_binary <- ifelse(neighbor_class$'Food insecure percentage rank along Adj Weighted Score'
                                        <mean(neighbor_class$'Food insecure percentage rank along Adj Weighted Score'),
                            "Below Avg. Food Insecur ","Above Avg. Food Insecur")

table(neighbor_class$foodins_binary)

foodins_table <- table(neighbor_class$foodins_binary)
foodins_prop <- prop.table(foodins_table)
print(foodins_prop)

pie(foodins_table,
    main = "Proportion of Respondents Above/Below Avg. Food Insecurity",
    col = c("tomato", "skyblue"),
    labels = paste(names(foodins_table), "\n", foodins_table))


