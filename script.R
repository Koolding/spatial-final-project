# Installing and loading required packages
install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("htmlwidgets")
install.packages("scales")
install.packages("chron")
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(scales)
library(chron)

# Reading the data and filtering relevant columns, removing rows with missing values
crimes <- read.csv("data/NYPD_Complaint_Data_Historic.csv") %>%
  na.omit()

# reading shapefile
boroughNYC <- st_read("data/geo_export_90163112-d4d3-46d0-a209-4a8ca6aee26e.shp")

crimes$CMPLNT_FR_DT <- as.Date(crimes$CMPLNT_FR_DT, format = "%m/%d/%Y")

crimes$CMPLNT_FR_DT <- substr(crimes$CMPLNT_FR_DT, 1, 4)

crimes2015 <- crimes %>%
  filter(CMPLNT_FR_DT == 2015)

# Calculate total number of crimes per borough
total_crimes <- crimes2015 %>%
  group_by(BORO_NM) %>%
  summarize(total_crimes = n())

boroughNYC$boro_name <- toupper(as.character(boroughNYC$boro_name))

# Join the total number of crimes with the filtered shapefile
boroughNYC_with_crimes <- left_join(boroughNYC, total_crimes, by = c("boro_name" = "BORO_NM"))

# Print the resulting shapefile with total accidents
print(boroughNYC_with_crimes)

# Create a plot to visualize
borough_plot <- ggplot(boroughNYC_with_crimes, aes(x = boro_name, y = total_crimes)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Reported Crimes per NYC Borough in 2015", x = "Borough", y = "Count") +
  scale_y_continuous(breaks=pretty_breaks(n = 8), labels = comma)

# Save the plot as a png file
ggsave("output/crimes_per_borough.png", borough_plot)

typeof(crimes2015$CMPLNT_FR_TM)

crimes2015$CMPLNT_FR_TM <- strptime(crimes2015$CMPLNT_FR_TM, "%H:%M:%S")

crimes2015$CMPLNT_FR_TM <- format(as.POSIXct(crimes2015$CMPLNT_FR_TM,
                           format = '%Y/%m/%d %H:%M:%S'),
                format = '%H/%M/%S')

# Extract the hour
crimes2015$CMPLNT_FR_TM <- substr(crimes2015$CMPLNT_FR_TM, 1, 2)

# Group the crimes by hour and count the occurrences
hour_count <- crimes2015 %>%
  group_by(CMPLNT_FR_TM) %>%
  summarize(Count = n())

# Sorting the hourly_accident_count dataframe by count in descending order
sorted_hourly_crime_count <- hour_count %>%
  arrange(desc(Count))

# Creating a bar plot of the hourly accident count
plot_hourly_crime_count <- ggplot(sorted_hourly_crime_count, aes(x = CMPLNT_FR_TM, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Hourly Crime Count", x = "Hour", y = "Count") +
  scale_y_continuous(breaks=pretty_breaks(n = 8), labels = comma)

# Saving the plot as a figure
ggsave("output/hourly_crime_count.png", plot_hourly_crime_count)

# Group the crimes by hour and count the occurrences
hour_count_brooklyn <- crimes2015 %>%
  filter(BORO_NM == "BROOKLYN") %>%
  group_by(CMPLNT_FR_TM) %>%
  summarize(Count = n())

# Sorting the hourly_accident_count dataframe by count in descending order
sorted_hourly_crime_count_brooklyn <- hour_count_brooklyn %>%
  arrange(desc(Count))

# Creating a bar plot of the hourly accident count
plot_hourly_crime_count_brooklyn <- ggplot(sorted_hourly_crime_count_brooklyn, aes(x = CMPLNT_FR_TM, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Hourly Crime Count - Brooklyn", x = "Hour", y = "Count") +
  scale_y_continuous(breaks=pretty_breaks(n = 8), labels = comma)

# Saving the plot as a figure
ggsave("output/hourly_crime_count_brooklyn.png", plot_hourly_crime_count_brooklyn)

# Group the crimes by hour and count the occurrences
hour_count_manhattan <- crimes2015 %>%
  filter(BORO_NM == "MANHATTAN") %>%
  group_by(CMPLNT_FR_TM) %>%
  summarize(Count = n())

# Sorting the hourly_accident_count dataframe by count in descending order
sorted_hourly_crime_count_manhattan <- hour_count_manhattan %>%
  arrange(desc(Count))

# Creating a bar plot of the hourly accident count
plot_hourly_crime_count_manhattan <- ggplot(sorted_hourly_crime_count_manhattan, aes(x = CMPLNT_FR_TM, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Hourly Crime Count - Manhattan", x = "Hour", y = "Count") +
  scale_y_continuous(breaks=pretty_breaks(n = 8), labels = comma)

# Saving the plot as a figure
ggsave("output/hourly_crime_count_manhattan.png", plot_hourly_crime_count_manhattan)

# Group the crimes by hour and count the occurrences
hour_count_bronx <- crimes2015 %>%
  filter(BORO_NM == "BRONX") %>%
  group_by(CMPLNT_FR_TM) %>%
  summarize(Count = n())

# Sorting the hourly_accident_count dataframe by count in descending order
sorted_hourly_crime_count_bronx <- hour_count_bronx %>%
  arrange(desc(Count))

# Creating a bar plot of the hourly accident count
plot_hourly_crime_count_bronx <- ggplot(sorted_hourly_crime_count_bronx, aes(x = CMPLNT_FR_TM, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Hourly Crime Count - Bronx", x = "Hour", y = "Count") +
  scale_y_continuous(breaks=pretty_breaks(n = 8), labels = comma)

# Saving the plot as a figure
ggsave("output/hourly_crime_count_bronx.png", plot_hourly_crime_count_bronx)

# Group the crimes by hour and count the occurrences
hour_count_queens <- crimes2015 %>%
  filter(BORO_NM == "QUEENS") %>%
  group_by(CMPLNT_FR_TM) %>%
  summarize(Count = n())

# Sorting the hourly_accident_count dataframe by count in descending order
sorted_hourly_crime_count_queens <- hour_count_queens %>%
  arrange(desc(Count))

# Creating a bar plot of the hourly accident count
plot_hourly_crime_count_queens <- ggplot(sorted_hourly_crime_count_queens, aes(x = CMPLNT_FR_TM, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Hourly Crime Count - Queens", x = "Hour", y = "Count") +
  scale_y_continuous(breaks=pretty_breaks(n = 8), labels = comma)

# Saving the plot as a figure
ggsave("output/hourly_crime_count_queens.png", plot_hourly_crime_count_queens)

# Group the crimes by hour and count the occurrences
hour_count_si <- crimes2015 %>%
  filter(BORO_NM == "STATEN ISLAND") %>%
  group_by(CMPLNT_FR_TM) %>%
  summarize(Count = n())

# Sorting the hourly_accident_count dataframe by count in descending order
sorted_hourly_crime_count_si <- hour_count_si %>%
  arrange(desc(Count))

# Creating a bar plot of the hourly accident count
plot_hourly_crime_count_si <- ggplot(sorted_hourly_crime_count_si, aes(x = CMPLNT_FR_TM, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Hourly Crime Count - Staten Island", x = "Hour", y = "Count") +
  scale_y_continuous(breaks=pretty_breaks(n = 8), labels = comma)

# Saving the plot as a figure
ggsave("output/hourly_crime_count_si.png", plot_hourly_crime_count_si)

# Reprojecting the shapefile to WGS84 CRS
boroughNYC_with_crimes <- st_transform(boroughNYC_with_crimes, crs = 4326)  # Reproject to WGS84 CRS (EPSG:4326)

# Visualizing a sort of a heat map
# Creating the map and storing it in a variable to be able to save it
NYC_map <- leaflet() %>%
  setView(lng = -74.006, lat = 40.7128, zoom = 10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = boroughNYC_with_crimes,
    fillColor = ~colors,
    fillOpacity = 0.7,
    color = "#ffffff",
    weight = 1,
    smoothFactor = 0.5,
    label = ~boro_name
  )

# showing the map
NYC_map

# Saving the leaflet map as an HTML file
saveWidget(NYC_map, "output/NYC_crime_map.html")

crimes2015_map <- crimes2015 %>% 
  mutate(content = paste0('<b>Date:</b> ', RPT_DT, '<br>', '<b>Borough: </b>', BORO_NM, '<br>', '<b>Description:</b> ', OFNS_DESC, '<br>', '<b>Crime completed:</b> ', CRM_ATPT_CPTD_CD, '<br>')) -> crimes2015_map

map <- crimes2015_map %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude, label = ~OFNS_DESC, popup = ~content, clusterOptions = markerClusterOptions()) 

map

saveWidget(NYC_map, "output/NYC__map.html")
