library(tidyverse)
library(treemap)
library(treemapify)
library(MexBrewer)
library(MetBrewer)
library(showtext)
library(ggtext)
library(voronoiTreemap)

data("canada")



#READ IN DEMOLITION DATA####
demolition_data <- read.csv("Demolition_Permits_20240703.csv")










#ADD SHOWTEXT FUNCTION####
showtext_auto()







#ADD FONTS HERE####
font_add_google(name = "Roboto Mono" ,family ="Roboto Mono")
font_add_google(name = "Inconsolata",family = "Inconsolata")






#CONVERT COLUMNS WITH DATES TO DATE TYPE WITH LUBRIDATE PACKAGE####
demolition_data <- demolition_data |> mutate(Added_Date = lubridate::mdy(Added_Date),
                                             Issue_Date = lubridate::mdy(Issue_Date),
                                             Final_Date = lubridate::mdy(Final_Date),
                                             Issue_Year = lubridate::year(Issue_Date),
                                             Final_Year = lubridate::year(Final_Date),
                                             Month_Finalized = lubridate::month(Final_Date,label= TRUE))









#REMOVES ALL NA VALUES. HAS NO NA VALUES IN THE DATASET AFTER RUNNING THIS LINE OF CODE####
demo_data_clean <- drop_na(data = demolition_data)









#CREATE NEW DATASET WITH SINGLE FAMILY HOMES ONLY BY CITY WITH COLUMN SHOWING PERCENT BY CITY LOCATION####
demos_by_location <- demo_data_clean |> 
  filter(Use_Code == "SINGLE FAMILY DWELLING" & Status %in% c("Finaled","Completed")) |>  
  group_by(City) |>  
  summarise(total_by_city = n()) |> #get count by city
  arrange(desc(total_by_city)) |> #arrange the count by city in the descending order
  mutate(pct = round(total_by_city/sum(total_by_city)*100,2)) # create pct column with two decimal places 









#FILTER TOTAL COUNT TO AREAS IN THE COUNTY WHERE THE CITY HAS MORE THAN 4% OF THE TOTAL####
top_demolition_areas<- demos_by_location |> filter(pct>=4)








#AGGREGATE ALL AREAS WITH PCT LESS THAN 4%
other_areas <- demos_by_location |> filter(pct<4) |> #filter cities with less than 4 percent
  summarise(City = "OTHER AREAS", #create new variable named Other Areas
            "total_by_city" = sum(total_by_city),# get total for the new variable
            "pct" = sum(pct))#get pct for the new variable






#AGGREATE TOP DEMOLITION AREAS AND OTHER AREAS####
final_demo_dataset <- bind_rows(top_demolition_areas,other_areas)



final_demo_dataset <- final_demo_dataset |> 
  mutate(City =  fct(City,levels = c("BETHESDA","CHEVY CHASE","POTOMAC","SILVER SPRING","KENSINGTON",
                             "ROCKVILLE","OTHER AREAS")))


final_demo_dataset$point_location <- c(rep(1,length(final_demo_dataset$City)))

final_demo_dataset |> 
  ggplot(mapping = aes(y =point_location,x = total_by_city, size = total_by_city))+
  geom_point(alpha = .5)+
  theme_void()+
  scale_y_log10()+
  scale_size(range = c(3,15))

  
 



  #CREATE TREEMAP####
final_demo_dataset |> 
  ggplot(mapping = aes(area = pct, fill = City))+
  geom_treemap(start = "bottomleft",color = "#F0F0E9",size = 2.25)+
  geom_treemap_text(aes(label = paste0(City," ",round(pct),sep = "%")),size = 27.5,color = "#F0F0E9",
                        start = "bottomleft",
                        family ="Inconsolata",
                        fontface ="bold",
                        grow = FALSE,
                    place = "center",
                    padding.x = grid::unit(2.05,"mm"))+
  
  
  theme_void()+
  theme(legend.position = "none")+

  scale_fill_manual(values = met.brewer("Veronese" ,n = 7,direction = -1))+
  labs(title = str_to_title("Out with the old. Infill with the new."))+
  labs(subtitle = "From Dec.2000 to Apr.2024, <b>90%</b> of the 4,753 issued permits to demolish and construct  <br> infill single-family homes are located in six areas of Montgomery County,MD",
       caption = "Source:Montgomery County, MD Open Data Portal | Visualization by @ETTS_12")+
theme(plot.title = element_markdown(size = 45,face ="bold",family = "Inconsolata", hjust = .5,
                                      margin = margin(t=10,b = 0)))+
  theme(plot.subtitle = element_markdown(size = 25, family = "Inconsolata",hjust = .5,
                                         margin = margin(t = 5,r = 0,b = 5,l = 0),
                                         lineheight = .35))+
  theme(plot.caption = element_markdown(size =11.5,family = "Inconsolata",face = "bold",
                                        hjust = .005,margin = margin(b=.35)
                                        ))+
  theme(plot.background = element_rect(fill =  "#F0F0E9",linewidth = 0,color ="#282828"))+
  theme(plot.margin =  margin(r=5,l=5,b=.5,unit = "pt"))+
  coord_cartesian(expand = TRUE)
  
  

  
  




ggsave(filename = "Demolition_Treemap_Chart.png",plot = last_plot(),width =6.5 ,height = 5.5,units = "in",dpi = 300)

  

scales::percent()

MetBrewer::display_all()

MexBrewer::display_all()
