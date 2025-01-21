library(tidyverse)
library(ggthemes)
library(showtext)
library(ggtext)
library(sf)
library(cartography)
library(paletteer)
library(MexBrewer)
library(MetBrewer)
library(classInt)
library(PrettyCols)



#ADD SHOWTEXT FUNCTION####
showtext_auto()







#ADD FONTS HERE####
font_add_google(name = "Roboto Mono" ,family ="Roboto Mono")
font_add_google(name = "Inconsolata",family = "Inconsolata")






#READ IN DEMOLITION DATA####
demolition_data <- read.csv("Demolition_Permits_20240703.csv")









#CONVERT COLUMNS WITH DATES TO DATE TYPE WITH LUBRIDATE PACKAGE####
demolition_data <- demolition_data |> mutate(Added_Date = lubridate::mdy(Added_Date),
                                             Issue_Date = lubridate::mdy(Issue_Date),
                                             Final_Date = lubridate::mdy(Final_Date),
                                             Issue_Year = lubridate::year(Issue_Date),
                                             Final_Year = lubridate::year(Final_Date))






#EXTRACT LAT AND LONG DATA AND CREATE A NEW COLUMN####
#CREATE COLUMNS WHERE LONGITUDE IS FIRST AND LATITUDE IS SECOND AND REMOVE OLD LAT AND LONG VALUES#
demolition_data <- demolition_data |> mutate(Coordinates = str_extract(Location,"(?<=\\().*(?=\\))")) |> 
  separate_wider_delim(cols = Coordinates,delim = ",",names = c("Lat","Long"),cols_remove = F) |> 
  mutate(Longitude= Long,Latitude = Lat) |> select(-Lat,-Long)









#REMOVES ALL NA VALUES. HAS NO NA VALUES IN THE DATASET AFTER RUNNING THIS LINE OF CODE####
demo_data_clean <- drop_na(data = demolition_data)








#CREATE NEW DATASET WITH SINGLE FAMILY HOMES ONLY BY CITY WITH COLUMN SHOWING PERCENT BY CITY LOCATION####
demos_by_location <- demo_data_clean |> 
  filter(Use_Code == "SINGLE FAMILY DWELLING" & Status %in% c("Finaled","Completed")) |>  
  group_by(City) |>  
  summarise(total_by_city = n()) |> 
  arrange(desc(total_by_city)) |> 
  mutate(pct = round(total_by_city/sum(total_by_city)*100,2))








#GET TOTAL COUNT OF DEMOLITIONS BY YEAR FOR EACH CITY IN THE DATASET####
yearly_demolition_data  <- demo_data_clean |> st_drop_geometry() |> 
  filter(Use_Code == "SINGLE FAMILY DWELLING" & Status %in% c("Finaled","Completed")) |>
  group_by(City,Final_Year) |> 
  summarise("Total_by_Year" =  n()) |> 
  ungroup() |> 
  group_by(Final_Year) |>
  mutate("Pct_by_Year" = Total_by_Year/sum(Total_by_Year)*100) 









#ADD DECADE TO THE YEARLY DATASET WITH CASE WHEN STATEMENT####
yearly_demolition_data<- yearly_demolition_data |> 
  mutate(Decade = case_when(
  between(Final_Year,2000,2009)~"2000",
  between(Final_Year,2010,2019)~"2010",
  between(Final_Year,2020,2024)~"2020",
.default = "other"))


total_by_year <- yearly_demolition_data |> group_by(City, Final_Year) |> 
  summarise("Total_by_Decade" = sum(Total_by_Year)) |> print(n = 30)

total_by_year |> ggplot(mapping = aes(x = Final_Year, y = Total_by_Decade, group = City, color = City))+
  #geom_bar(stat ="identity", width = .85)
  geom_line()
  
  #ylim(-95,400)+
  #coord_polar(start =-.10)
  #scale_x_discrete(expand = c(.75,.75))+
  #scale_y_continuous(limits =c(0,400))





#CREATE ALL TIME TOTAL COUNT BY CITY####
total_lifetime_demolitions <- yearly_demolition_data |> 
  group_by(City) |> 
  summarise("Total_Demos" = sum(Total_by_Year)) |> 
  ungroup() |> 
  mutate("Pct_by_City" = round(Total_Demos/sum(Total_Demos)*100,2))
 

  






#FILTER TOTAL COUNT TO AREAS IN THE COUNTY WHERE THE CITY HAS MORE THAN 4% OF THE TOTAL####
top_demolition_areas<- total_lifetime_demolitions |> filter(Pct_by_City>=4)








#FILTER TO AREAS IN THE COUNTY WHERE THE CITY HAS LESS THAN 4% OF THE TOTAL####  
other_areas <- total_lifetime_demolitions |> filter(Pct_by_City<4)|> 
summarise(City = "OTHER AREAS", "Total_Demos" =  sum(Total_Demos,.groups = FALSE),
"Pct_by_City" = sum(Pct_by_City)) 




  






#COMBINE TOP DEMOLITION LOCATION WITH OTHER AREAS####
final_demo_dataset <- bind_rows(top_demolition_areas,other_areas)





final_demo_dataset <- final_demo_dataset |> 
  mutate(City =  fct_relevel(City,"BETHESDA","CHEVY CHASE","POTOMAC","SILVER SPRING","KENSINGTON",
                             "ROCKVILLE","OTHER AREAS"))



#CREATE GRAPHIC



final_demo_dataset |> ggplot(mapping = aes(x=fct_rev(City), y = Total_Demos))+
  
  geom_linerange(mapping = aes(ymin = 0, ymax =Total_Demos),linewidth =1, color = "#103783")+
  geom_linerange(mapping = aes(ymin = 0, ymax =Total_Demos),linewidth =.5, color = "#9bafd9", alpha =.5)+
  geom_point(size = 2.5, fill = "#9bafd9",color = "#103783", shape =21 )+
  geom_point(size = 2.0, color = "#103783", alpha = .45 )+
  
  annotate(geom = "linerange",xmin = .7,xmax = 7.3,y = 0, color = "#103783", linewidth = 1)+
  annotate(geom = "linerange",xmin = .7,xmax = 7.3,y = 0, color = "#9bafd9", linewidth = .5, alpha =.5)+
  
  #ADD IN CITY NAME TEXT TO THE GRAPHIC
  geom_text(mapping = aes(label = City, y =0),hjust =1.15, family = "Inconsolata", 
  size  =7.95, fontface = "bold")+
  
  #ADD IN TOTAL DEMOLITION TEXT TO THE GRAPHIC
  #geom_text(mapping = aes(label = scales::comma(Total_Demos)),vjust =-1.45, hjust =.30, 
            #family = "Roboto Mono", 
            #size  =4.95, fontface = "bold")+
 
  
  #annotate(geom = "text",x =7.15 ,y = 2845,label = "2,813", family = "Roboto Mono", 
           #size  =4.95, fontface = "bold")+
  
  #annotate(geom = "text",x =5.75 ,y = 2925,label = "# of Issued Permits", family = "Inconsolata", 
           #size  =4.95, fontface = "bold")+
 
  
 coord_polar(theta = "y",clip = "off",direction = 1 )+
 theme_void()+
 scale_x_discrete(expand = c(.5,.5))+
 scale_y_continuous(limits =c(0,3500))+
 guides(size = "none")+
 #theme(plot.margin = margin(rep(-50,4)))+
 theme(plot.margin = margin(t = -65,b =-20,r=0,l=0,unit = "pt"))+ 
 theme(plot.background = element_rect(fill =  "#F0F0E9",linewidth = 0))+
 
 
  
theme(plot.title = element_markdown(size = 45,face ="bold",family = "Inconsolata", hjust = .5,
                                                      margin = margin(t=-50,b = 0)))+
theme(plot.subtitle = element_markdown(size = 25, family = "Inconsolata",hjust = .5,
                                         margin = margin(t = 5,r = 0,b = 0,l = 0),
                                         lineheight = .35))+
  theme(plot.caption = element_markdown(size =13.5,family = "Inconsolata",hjust = 0,
                                        margin = margin(t = 0,b = 2.50)))+
  labs(title = str_to_title("Out with the old. Infill with the new."))+
  labs(subtitle = "From Dec.2000 to Apr.2024, <b>90%</b> of the 4,753 issued permits to demolish and construct  <br> infill single-family homes are located in six areas of Montgomery County,MD",
       caption = "Source:Montgomery County, MD Open Data Portal | Visualization by @ETTS_12")



  
 





####EXPORT DATA####
ggsave(filename = "Demolition_Polor_Chart.png",plot = last_plot(),width =6.5 ,height = 5.5,units = "in",dpi = 300)

