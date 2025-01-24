library(tidyverse)
library(ggthemes)
library(showtext)
library(ggtext)



#ADD SHOWTEXT FUNCTION####
showtext_auto()







#ADD FONTS HERE####
font_add_google(name = "Roboto Mono" ,family ="Roboto Mono")
font_add_google(name = "Inconsolata",family = "Inconsolata")









#READ IN DEMOLITION DATA####
demolition_data <- read.csv("MoCo_Demolition/Demolition_Permits_20240703.csv")









#CONVERT COLUMNS WITH DATES TO DATE FILES USING LUBRIDATE PACKAGE####
demolition_data <- demolition_data |> mutate(Added_Date = lubridate::mdy(Added_Date),
                                             Issue_Date = lubridate::mdy(Issue_Date),
                                             Final_Date = lubridate::mdy(Final_Date),
                                             Issue_Year = lubridate::year(Issue_Date),
                                             Final_Year = lubridate::year(Final_Date))






#EXTRACT LAT AND LONG DATA AND CREATE A NEW COLUMN####
demolition_data <- demolition_data |> mutate(Coordinates = str_extract(Location,"(?<=\\().*(?=\\))")) |> 
  separate_wider_delim(cols = Coordinates,delim = ",",names = c("Lat","Long"),cols_remove = F)









#REMOVES ALL NA VALUES. HAS NO NA VALUES IN THE DATASET AFTER RUNNING THIS LINE OF CODE####
demo_data_clean <- drop_na(data = demolition_data)










#PROVIDES A COUNT OF NA VALUES BY COLUMN####
sum(is.na(demo_data_clean))








#REMOVES NA VALUES FROM THE COLUMN YOU SELECT
test_3 <- demolition_data[!is.na(demolition_data$Final_Date),]








#FILTER TO SINGLE FAMILY HOMES ONLY AND GET COUNT OF DEMOS####
demo_data_clean |> filter(Use_Code == "SINGLE FAMILY DWELLING" & Status %in% c("Finaled","Completed")) |> 
  summarise(n())








#CREATE NEW DATASET WITH SINGLE FAMILY HOMES ONLY BY CITY WITH COLUMN SHOWING PERCENT BY CITY LOCATION####
demos_by_location <- demo_data_clean |> 
  filter(Use_Code == "SINGLE FAMILY DWELLING" & Status %in% c("Finaled","Completed")) |>  
  group_by(City) |>  summarise(total_by_city = n()) |> #creates new column with total by city
  arrange(desc(total_by_city)) |> # orders in descending order
  mutate(pct = round(total_by_city/sum(total_by_city)*100,2)) #creates new column with percent of demos in each city









#GET SUM OF THE LOCATIONS AFTER TOP 5. TOTAL IS 14%####
sum(demos_by_location$pct[1:6])









#CREATED SEPERATE DATASET TO SELECT TOP FIVE DEMO LOCATIONS IN THE COUNTY####
top_5_cities <- demos_by_location |> filter(total_by_city >=100)








#OTHER THAN THE TOP FIVE DEMO LOCATIONS####
#CREATES A SINGLE ROW WITH A NEW VARIABLE IN THE CITY COLUMN NAMES AS OTHER CITIES WITH TOTAL 
other_cities <- demos_by_location |> filter(total_by_city<100) |>  
  summarise(City= "OTHER AREAS",across(c(total_by_city,pct),.fns = sum))

demos_by_location |> filter(total_by_city<100) |> 
  summarise(City = "OTHER CITIES",
            total_by_city = sum(total_by_city),
            pct = sum(pct))





#BIND THE ROWS TOGETHER TO CREATE FINAL DATASET####
final_demo_dataset <- (bind_rows(top_5_cities,other_cities)) |> rename("city"="City")









#USED FACTOR TO CHANGE LEVELS OF DATASETS####
final_demo_dataset <- final_demo_dataset |> 
  mutate(city =  fct_relevel(city,"BETHESDA","CHEVY CHASE","POTOMAC","SILVER SPRING","KENSINGTON","ROCKVILLE","OTHER AREAS"))









#CREAT BAR GRAPHIC#####
final_demo_dataset |>
  ggplot(mapping = aes(x =fct_rev(city),y  = total_by_city))+
  geom_bar(stat = "identity",width = .550,fill = "#0d3b66",alpha =1,position=position_dodge(-7))+
  geom_bar(stat = "identity",width = .400,fill = "#557c93",alpha = .25,position=position_dodge(-7))+
  geom_hline(yintercept = 0, color = "#282828")+
  scale_y_continuous(breaks = seq(0,2500,500),labels = scales::label_comma(),position = "right")+
  theme_void()+
  theme(axis.text.y = element_text(size = 30,family = "Inconsolata", face = "bold",
                                   margin = margin(l =5,r =-10,unit = "pt")))+
  
  theme(axis.text.x = element_text(size = 27,family = "Inconsolata",margin = margin(b =2.5,unit = "pt")))+
  
  theme(panel.grid.major.x = element_line(color = "#E8E7E2"))+
  theme(panel.grid.major.y = element_line(color = "#E8E7E2"))+
  
  theme(plot.margin = margin(l =5,t = 20,b =10,r = 5,unit = "pt"))+
  
  theme(plot.background = element_rect(fill = "#F0F0E9",linewidth = 0))+
  
  
  
  #ADD TEXT TO GRAPHIC####
annotate(geom = "text",x = 7,y = 2200,label = "59% of Demolition & Move Permits",color = "#F0F0E9", 
         family = "Inconsolata",fontface = "bold", size = 12 )+
  
  annotate(geom = "text",x = c(6,5,4,3,2,1),y = c(444-75,297-75,269-75,249-75,229-75,452-75),
           label = c("9%","6%","6%","5%","5%","10%"),color = "#F0F0E9", 
           family = "Inconsolata",fontface = "bold", size = 12 )+
  
  annotate(geom = "text", x = 7.75, y = 2750, label = "# of Permits",family = "Inconsolata",size = 8,
           
           lineheight = .5)+ 
  
  
  
  #ADD TITLE, SUBTITLE AND SOURCE CAPTION
  
  labs(title = str_to_title("Out with the old. Infill with the new."))+
  labs(subtitle = "From Dec.2000 to Apr.2024, <b>90%</b> of demolish and construct permits issued to redevelop <br>
       single-family homes are located in six areas of Montgomery County,MD.",
       caption = "Source:Montgomery County, MD Open Data Portal | Visualization by @ETTS_12")+
  
  
  
  
  theme(plot.title = element_markdown(size = 45,face ="bold",family = "Inconsolata", margin = margin(b = 0)))+
  theme(plot.subtitle = element_markdown(size = 32, family = "Inconsolata",
                                         margin = margin(t = 5,r = 0,b = 7,l = 0),lineheight = .45))+
  theme(plot.caption = element_markdown(size =20,family = "Inconsolata",hjust = 0,margin = margin(t =5,b =-2.5)))+
  
  
  
  coord_flip(clip = "off",expand = TRUE,xlim = c(1,7))





####EXPORT DATA####
ggsave(filename = "Demolition.tiff",plot = last_plot(),width =7.5 ,height = 5,units = "in",dpi = 300)







