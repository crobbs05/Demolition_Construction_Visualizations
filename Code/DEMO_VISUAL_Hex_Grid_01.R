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
demolition_data <- drop_na(data = demolition_data)








#CONVERT TO SIMPLE FEATURES POINT DATASET OF DEMOLITION LOCATIONS
#CREATES NEW GEOM COLUMN IS LAT AND LONG AS A LIST
demolition_data <- demolition_data |> 
  st_as_sf(coords =c("Longitude","Latitude"),crs = 4326) |> st_transform(crs = 4326)
  








#VIEW DEMOLITION POINTS
demolition_data |> 
  ggplot()+
  geom_sf()+
  coord_sf(crs = 22418)









#CHECK TO ENSURE THE LOCAL CRS IS 22418
st_crs(demolition_data)

#############################CREATE MONTGOMERY COUNTY SF OBJECT




#GET MONTGOMERY COUNTY BOUNDARY AND SET THE CRS TO 4326
county_boundary <- read_sf("County_Boundary/County_Boundary.shp") |> 
  st_as_sf(crs = 4326)









#CHECK THE CRS TO ENSURE ITS 4326
sf::st_crs(county_boundary)









#VIEW THE COUNTY BOUNDARY
plot(st_geometry(county_boundary))








#READ IN COUNTY ZIPCODES####
county_zipcodes <- read_sf("Zip_Codes/Zip_Codes.shp")|> 
  st_as_sf(crs = 4326)







#CHECK THE CRS TO ENSURE ITS 4326
sf::st_crs(county_zipcodes)








#PLOT TO SEE COUNTY BOUNDARY
plot(st_geometry(county_zipcodes))



#CONVERT ZIPCODES TO NEW LOCAL CRS OF 22418####
moco_polygon_local_crs <- st_transform(county_zipcodes,crs = 22418) |> st_sf()




#VIEW POLYGON
plot(st_geometry(moco_polygon_local_crs))









#MAKE HEX GRID OF MONTOGOMERY COUNTY #HEX GRID WILL CREATE BOUNDARY BOX OF THE COUNTY#####

hex_grid <- moco_polygon_local_crs|> 
  st_make_grid(square = FALSE,flat_topped = FALSE,crs = 22418,cellsize = 800)   
  








#VIEW POLYGON
plot(st_geometry(hex_grid))










#SUBSET HEX GRID AND CREATE GRID SHOWING BOUNDARY FOR MONTGOMERY COUNTY#####
#Hex_Grid and Moco_Polygon_Local_CRS MUST HAVE THE SAME CRS
moco_hex_grid <- hex_grid[moco_polygon_local_crs] |> st_transform(crs = 22418)

  




  
#VIEW MOCO HEX GRID  
moco_hex_grid |> 
  ggplot()+
  geom_sf()+
  coord_sf(crs = 22418)









#ADD ID TO HEX GRID FOR JOIN. USING THE ID TO COUNT THE INTERSECTION BETWEEN POINT AND HEX DATA####
moco_hex_grid_sf <- st_sf(moco_hex_grid) |> 
  mutate(id = row_number())





#CHECK CRS TO ENSURE IT IS 22418
st_crs(moco_hex_grid_sf)









#ENSURE BOTH DATASET HAVE THE SAME CRS BY CREATING NEW SF FEATURE USING CRS FROM MOCO HEX GRID SF####
demolition_data_hex <-  st_transform(demolition_data,crs = st_crs(moco_hex_grid_sf))









#CHECK CRS TO ENSURE IT IS 22418
st_crs(demolition_data_hex)






#VIEW THE NEW OBJECT TO CONFIRM
demolition_data_hex |> 
  ggplot()+
  geom_sf()




#SUMMARIZE POINTS AND MOCO HEX GRID ARE JOINED TOGTHER####
#OUTPUT COLUMN CONTAINS ALL COLUMNS FROM X AND THE ID COLUMN FROM Y###
#ID COLUMN WILL HAVE DUPLICATES#. 
#SHOWS WHERE YOU FIND THE INTERSECTION BETWEEN POINTS AND THE MOCO HEX GRID
#THIS MEANS MORE THAN ONE POINT FROM THE DEMOLITION DATA IS LOCATED IN A HEXAGON#
joined_hex <- st_join(demolition_data_hex,moco_hex_grid_sf,left =FALSE)









#CREATE NEW OBJECT TO COUNT THE NUMBER OF POINTS THAT FALL INTO EACH HEXAGON#####
#WE ARE COMING BY ID#
#TOTAL COLUMN SHOWS THE ID COUNTY BY HEXAGON#
sum_joined_hex <- joined_hex |> 
  group_by(City,id) |> 
  count(name = "total")


 
#REMOVE GEOMETRY FROM DATASET. PREPARING DATA FOR JOIN TO MOCO HEX DATA####
less_geo_sum_join_hex <- sum_joined_hex |> as_tibble() |> select(-geometry)









#JOIN LESS GEO SUM JOIN TIBBEL WITH HEX. END RESULT WILL HAVE A COUNT FOR EACH HEX####
final_hex_join <- left_join(x = moco_hex_grid_sf,y = less_geo_sum_join_hex,by = "id")  
  #mutate(total = replace_na(total,replace = 0))


#CREATE CLASS INTERVALS FOR THE FINAL HEX####
final_hex_join$breaks <-  classInt::classify_intervals(var = final_hex_join$total,n = 5,style = "jenks")




#VIEW THE BREAKS COLUMNS
unique(final_hex_join$breaks)







#CREATE COUNTY BOUNDARY FOR HEXMAP
#converts the hex polygons to a single polyline with hex boundary
county_hex_boundary <- final_hex_join |>  st_union() 









#SEE THE OUTLINE OF THE COUNTY HEX BOUNDARY
plot(st_geometry(county_hex_boundary))









#CREATE SINGLE BOUNDARIES FOR AREAS YOU WANT TO HIGHLIGHT WITH A LABEL 
bethesda <- final_hex_join[final_hex_join$City == "BETHESDA",] |> drop_na() |> sf::st_union()

rockville <- final_hex_join[final_hex_join$City == "ROCKVILLE",] |> drop_na()|> sf::st_union()

kensington <- final_hex_join[final_hex_join$City == "KENSINGTON",]|> drop_na()|> sf::st_union()

potomac<- final_hex_join[final_hex_join$City == "POTOMAC",]|> drop_na()|> sf::st_union()


silver_spring<- final_hex_join[final_hex_join$City == "SILVER SPRING",]|> drop_na()|> sf::st_union()












#CREATE COLOR PALETTE FOR BREAKS
cols <- c("#778da9","#415a77","#1b263b","#0d1b2a")
cols_v02 <- c("#4C6C94FF","#435E7FFF","#2F415FFF","#232C43FF")
manu_kiwi <- c("#3e2926", "#634c54", "#8b7162", "#a9a196", "#cfae9f", "#d3bac0")
manu_kotare <- c("#214d65", "#287DAB", "#E5BF86", "#B09771", "#624B27", "#CACFD0")





#ADD SHOWTEXT FUNCTION####
showtext_auto()







#ADD FONTS HERE####
font_add_google(name = "Roboto Mono" ,family ="Roboto Mono")
font_add_google(name = "Inconsolata",family = "Inconsolata")










#VIEW FINAL HEX COUNTS
final_hex_join|> 
    ggplot()+
   
  
  geom_sf(aes(fill =breaks,group = City),linewidth =.250,alpha = .95,color  = "#F0F0E9")+ # FILL DATA
  geom_sf(data = county_hex_boundary,aes(),linewidth =.35, color = "#c4c4b6", fill = NA, alpha =.25)+ #BOUNDARY LINE 
  #geom_sf(data = bethesda,aes(),linewidth = .35, color = "#272727",fill = NA,linetype =1)+
  #geom_sf(data = bethesda,aes(),linewidth = .25, color = "#F0F0E9",fill = NA,linetype =1)+
  #geom_sf(data = rockville,aes(),linewidth = .85, color = "#FFFFFF",fill = NA)+
  #geom_sf(data = rockville,aes(),linewidth = .35, color = "#272727",fill = NA)+
  #geom_sf(data = potomac,aes(),linewidth = .85, color = "#FFFFFF",fill = NA)+
  #geom_sf(data = potomac,aes(),linewidth = .35, color = "#272727",fill = NA)+
  #geom_sf(data = silver_spring,aes(),linewidth =.85,color = "#FFFFFF",fill = NA)+
  #geom_sf(data = silver_spring,aes(),linewidth =.35,color = "#272727",fill = NA)+
  
  
  theme_void()+
    
    #ADJUSTING THE LEGEND
    scale_fill_manual(values = met.brewer("Hokusai2" ,n = 5,direction = 1),
    name ="Number of Permits Issued", 
    labels = c("1 - 8","9 - 29","30 - 66","67 - 117","> 117","No Permits Issued"),na.value = "grey50")+
    
    #MAKING THE LENGEND ONE ROW AND MOVING IT TO THE TOP
    guides(fill = guide_legend(nrow = 1,position = "top",color = NA, 
                               title.position ="top",title.hjust=0.5,
                               theme = theme(legend.key = element_rect(linewidth = 0))))+
    
  
  
  #ADD TITLE, SUBTITLE AND SOURCE CAPTION
  
  labs(title = str_to_title("Out with the old. Infill with the new."))+
  labs(subtitle = "<b>90%</b> of demolish and move permits issued to redevelop single-family homes <br>
        from Dec.2000 to Apr.2024 are concentrated in six areas of Montgomery County,MD.",
       caption = "Source:Montgomery County, MD Open Data Portal | Visualization by @ETTS12.BSKY.SOCIAL")+
  
  
  
  
  theme(plot.title = element_markdown(size = 45,face ="bold",family = "Inconsolata", hjust = .5,
                                      margin = margin(b = 0)))+
  theme(plot.subtitle = element_markdown(size = 32, family = "Inconsolata", hjust =.5,
                                         margin = margin(t = 5,r = 0,b = 10,l = 0),lineheight = .35))+
  theme(plot.caption = element_markdown(size =15,family = "Inconsolata",hjust = 0,margin = margin(t =5,b = 5,l=-50.5)))+
  
  
  
  
  
  
  
    
    #ADJUSTING THE LEGEND SYMBOL AND FONT SIZE AND FONT FAMILY
    theme(legend.key.size = unit(10,"pt"))+
    #theme(legend.key = element_rect(color = NULL, linewidth = 0))+
    theme(legend.text = element_text(family = "Roboto Mono",size = 18,face = "bold", color = "#272727"))+
    theme(legend.title = element_text(family = "Roboto Mono",size = 22,face = "bold",color = "#272727"))+
    
  
    #annotate(geom = "segment",x = 77.1,xend =  77.30,y = 9.0,yend = 3.33)+

    #ADJUSTING THE PLOT BACKGROUND COLOR AND ADDING MARKING SPACE AT THE TOP
    theme(plot.background = element_rect(fill = "#F0F0E9", linewidth = 0, color = "#272727"))+
    theme(plot.margin = margin(t = 10))+
    coord_sf(expand = TRUE,crs = 4326)
    






####EXPORT DATA####
ggsave(filename = "Demolition_Hex.png",plot = last_plot(),width =6.5 ,height = 5.5,units = "in",dpi = 300)

