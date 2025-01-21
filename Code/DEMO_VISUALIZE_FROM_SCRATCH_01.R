library(tidyverse)

region <- c("West","Midwest","Mid-Atlantic","New England","Southwest","South")

data <- tibble(places = paste(LETTERS[1:15],"CITY",sep = "-"),
               population_2020 = sample(15000:35000,length(places),replace = TRUE),
               population_2021 = sample(15000:35000,length(places),replace = TRUE),
               population_2022 = sample(15500:35000,length(places),replace = TRUE),
               population_2023 = sample(16000:45000,length(places),replace = TRUE),
               region = sample(c("West","Midwest","Mid-Atlantic","New England","Southwest","South"),
                               length(places),replace = TRUE))


long_data <- pivot_longer(data,cols = population_2020:population_2023,names_to ="population_year",values_to ="population_total") |> mutate(year = as.integer(str_sub(population_year,-4)))

grouped_data <- long_data |> group_by(region,year) |> summarise(total = sum(population_total))


grouped_data |> ggplot(mapping = aes(x = year, y  = total, group = region, color = region ))+
  geom_line()



purrr::map_chr(region,~{paste(.x,"Region",sep = "_")})
  




matrix(data = (sample(45000:55000,size = 24,replace = FALSE,)),nrow = 5,ncol = 24)

data <- matrix(data = runif(144,min = 60000,max = 65700),nrow = 6,ncol =24) |> round()




places <- paste(LETTERS[1:6],"CITY",sep = "-")


region <- c("West","Midwest","Mid-Atlantic","New England","Southwest","South")

population <- paste("population",2000:2024,sep = "_")

data_final <- as.tibble(data)

colnames(data_final)[1:25] <- population

data_final <- data_final |> mutate(location = places,
region = sample(c("West","Midwest","Mid-Atlantic","New England","Southwest","South"),
                6,replace = FALSE)) |> select(location,everything())



data_final_v02 <- pivot_longer(data_final,cols = population_2000:population_2023,names_to ="population_year",values_to ="population_total",cols_vary = "fastest") |> 
  mutate(year = as.integer(str_sub(population_year,-4)))

grouped_data <- data_final_v02 |> group_by(region,year) |> summarise(total = sum(population_total))

data_final_v02|> ggplot(mapping = aes(x = year, y  = population_total,color = location, group = location ))+geom_line()+ geom_point()
