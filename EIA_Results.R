#RESULTS FOR ECOLOGICAL IMPACT ASSESSMENT

#loading data: vertebrates
#install.packages("readxl")
library(readxl)

verts <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Vertebrates")

#unique vertebrate families at each site 
south_vertfam <- length(unique(verts$family[verts$site == "South"])) #14 unique families
north_vertfam <- length(unique(verts$family[verts$site == "North"])) #15 unique families 

#plotting vertebrate families per site 
library(ggplot2)

plot_data <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(south_vertfam, north_vertfam)
)

ggplot(plot_data, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of vertebrate families per site", 
       y = "Number of orders", 
       x = "Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#loading data: invertebrates, terrestrial

inverts_t <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_t")

#extracting necessary columns
inverts_subset <- inverts_t %>%
  select(site, order, individualCount)

#removing n/a values
invert_clean <- inverts_subset %>%
  filter(!is.na(individualCount) & !is.na(order))

#unique invertebrate orders at each site 
south_invertord <- length(unique(invert_clean$order[invert_clean$site == "South"])) #9 unique orders
north_invertord <- length(unique(invert_clean$order[invert_clean$site == "North"])) #8 unique orders 

#plotting invertebrate orders per site 
plot_datain <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(south_invertord, north_invertord)
)

ggplot(plot_datain, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of terrestrial invertebrate orders per site", 
       y = "Number of orders", 
       x = "Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#relative abundance of each order
#barplot of relative abundances for each order
ggplot(invert_clean, aes(x = order, y = individualCount, fill = site)) + 
  geom_bar(stat = "identity", position = "stack") +  #stack for stacked bars
  theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
  scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
  labs(fill="Site", title="Relative abundances of terrestrial invertebrate orders between sites") #changing legend title, adding graph title

#shannon index for invertebrate orders 
#install.packages("vegan")
library(vegan)

#reshaping the data so each site is a row, for ease of calculation
library(tidyr)
invertshan <- invert_clean %>%
  pivot_wider(
    names_from = order,
    values_from = individualCount,
    values_fill = list(individualCount = 0),
    values_fn = list(individualCount = sum)
  )

#calculating shannon diversity index
library(vegan)
shannon_div_in <- diversity(invertshan[, -1], index = "shannon") #removes site column 

shannon_results_in <- data.frame(
  site = invertshan$site,  
  Shannon_Index = shannon_div_in  
)

print(shannon_results_in)

#loading data: invertebrates, aquatic

inverts_a <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Inverts_a")

#extracting necessary columns
invertsa_subset <- inverts_a %>%
  select(site, order, individualCount)

#removing n/a values
inverta_clean <- invertsa_subset %>%
  filter(!is.na(individualCount) & !is.na(order))

#unique invertebrate orders at each site 
south_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "South"])) #12 unique orders
north_invertorda <- length(unique(inverta_clean$order[inverta_clean$site == "North"])) #13 unique orders 

#plotting invertebrate orders per site 
plot_dataina <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(south_invertorda, north_invertorda)
)

ggplot(plot_dataina, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Number of aquatic invertebrate orders per site", 
       y = "Number of orders", 
       x = "Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours

#relative abundance of each order
#barplot of relative abundances for each order
ggplot(inverta_clean, aes(x = order, y = individualCount, fill = site)) + 
  geom_bar(stat = "identity", position = "stack") +  #stack for stacked bars
  theme_grey() + xlab("Order") + ylab("Abundance (count)") + #grey theme, x and y axis labels 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #x labels angled to make legible 
  scale_fill_manual(values = c("lightblue3", "orange2")) + #colour blind friendly 
  labs(fill="Site", title="Relative abundances of aquatic invertebrate orders between sites") #changing legend title, adding graph title

#shannon index for invertebrate orders 
#install.packages("vegan")
library(vegan)

#reshaping the data so each site is a row, for ease of calculation
library(tidyr)
inverta_shan <- inverta_clean %>%
  pivot_wider(
    names_from = order,
    values_from = individualCount,
    values_fill = list(individualCount = 0),
    values_fn = list(individualCount = sum)
  )

#calculating shannon diversity index
library(vegan)
shannon_div_ina <- diversity(inverta_shan[, -1], index = "shannon") #removes site column 

shannon_results_ina <- data.frame(
  site = inverta_shan$site,  
  Shannon_Index = shannon_div_ina
)

print(shannon_results_ina)

#comparison of all data collected 
#load data for all 

all <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="All")

#unique invertebrate orders at each site 
all_orders_s <- length(unique(all$order[all$site == "South"])) #26 unique orders
all_orders_n <- length(unique(all$order[all$site == "North"])) #26 unique orders 

plot_data_all <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(all_orders_s, all_orders_n)
)

ggplot(plot_data_all, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Total number of orders per site", 
       y = "Number of orders", 
       x = "Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colour blind friendly colours
