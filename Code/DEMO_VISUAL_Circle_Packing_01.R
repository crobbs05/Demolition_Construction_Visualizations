library(tidyverse)
library(ggthemes)
library(showtext)
library(ggtext)
library(packcircles)
library(MexBrewer)
library(MetBrewer)




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








#CREATE NEW DATASET WITH SINGLE FAMILY HOMES BY CITY WITH COLUMN SHOWING PERCENT BY CITY LOCATION####
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








#SHOW VALUES OF DEMOS FOR OTHRE LOCATION IN THE COUNTY
demos_by_location |> filter(total_by_city<100) |> 
  summarise(City = "OTHER CITIES",
            total_by_city = sum(total_by_city),
            pct = sum(pct))









#BIND THE ROWS TOGETHER TO CREATE FINAL DATASET####
final_demo_dataset <- (bind_rows(top_5_cities,other_cities)) |> rename("city"="City") #CHANGE THE NAME OF THE CITY TO LOWER CASE









#USED FACTOR TO CHANGE LEVELS OF DATASETS####
final_demo_dataset <- final_demo_dataset |> 
  mutate(city =  fct_relevel(city,"BETHESDA","CHEVY CHASE","POTOMAC","SILVER SPRING","KENSINGTON","ROCKVILLE","OTHER AREAS"))








packing <- circleProgressiveLayout(x = final_demo_dataset$pct,sizetype = "area")









packing$radius <- packing$radius *.93


packing$radius_v02 <- packing$radius *.955






label_data <- cbind(final_demo_dataset,packing)









data_circles <- circleLayoutVertices(packing,npoints = 150)

data_circles_v02 <- circleLayoutVertices(packing[,c(1,2,4)],npoints = 150)





rm(Veronese)


#CREATE CIRCLE PACKING PLOT
ggplot()+
  geom_polygon(data = data_circles,aes(x = x,y = y,group = id, fill=as.factor(id)), 
               alpha =.95,
               show.legend = FALSE)+
  geom_polygon(data = data_circles_v02,aes(x = x,y = y,group = id,  fill=as.factor(id)), 
               alpha =.35,linewidth = .25,color ="#F0F0E9",
               show.legend = FALSE)+
  
  geom_text(data = label_data,aes(x = x,y = y,label = city), 
            color = "#F0F0E9",size = 8, family = "Inconsolata",fontface ="bold")+
  geom_text(data = label_data,aes(x = x,y = y,label = paste0(round(pct),sep ="%")), 
            color = "#F0F0E9",size = 8, family = "Inconsolata",
            nudge_y = -.25)+
  theme_fivethirtyeight()+
  theme(panel.grid.major =  element_blank())+
  theme(axis.text = element_blank())+
  scale_color_manual(values = met.brewer("Veronese" ,n = 7,direction = -1,type = "discrete"),.5)+
  scale_fill_manual(values = met.brewer("Veronese" ,n = 7,direction = -1,type = "discrete"))+
  theme(plot.background = element_rect(fill = "#F0F0E9", linewidth = 0, color = "#272727"))+
  theme(panel.background = element_rect(fill = "#F0F0E9",linewidth = 0))+
  
  #ADD TITLE, SUBTITLE AND SOURCE CAPTION
  labs(title = str_to_title("Out with the old. Infill with the new."))+
  labs(subtitle = "<b>90%</b> of demolish and move permits issued to redevelop single-family homes <br> 
       from Dec.2000 to Apr.2024 are located in six areas of Montgomery County,MD.",
       caption = "Source:Montgomery County, MD Open Data Portal | Visualization by @ETTS12.BSKY.SOCIAL")+
  
  
  
  #ADJUSTING TITLE, SUBTILE AND CAPTION
  theme(plot.title = element_markdown(size = 45,face ="bold",family = "Inconsolata", hjust = .5,
                                      margin = margin(b = 0)))+
  theme(plot.subtitle = element_markdown(size = 32, family = "Inconsolata",hjust = .5,
                                         margin = margin(t = 5,r = 0,b = 7,l = 0),lineheight = .45))+
  theme(plot.caption = element_markdown(size =15,family = "Inconsolata",hjust = 0,margin = margin(t =5,b =-9)))+
  
  
  
  coord_equal(expand = TRUE)
  






###EXPORT DATA####
ggsave(filename = "Demolition_Circle_Packing.png",plot = last_plot(),width =7.5 ,height = 5,units = "in",dpi = 300)







