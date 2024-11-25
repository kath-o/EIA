#RESULTS FOR ECOLOGICAL IMPACT ASSESSMENT

#graphical representations of biodiversity: number of orders present comparison 

#loading data: vertebrates
#install.packages("readxl")
library(readxl)

verts <- read_excel("~/Desktop/MSc EEB/WD/EIA/arran_res.xlsx",sheet="Vertebrates")

#unique vertebrate orders at each site 
south_vertord <- length(unique(verts$order[verts$site == "South"])) #8 unique orders
north_vertord <- length(unique(verts$order[verts$site == "North"])) #8 unique orders 

#plotting vertebrate orders per site 
library(ggplot2)

plot_data <- data.frame(
  Site = c("South", "North"),
  UniqueOrders = c(south_vertord, north_vertord)
)

ggplot(plot_data, aes(x = Site, y = UniqueOrders, fill = Site)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", width=0.5) + # stat = "identity" uses the provided y-values
  labs(title = "Unique vertebrate orders at each site", 
       y = "Number of Unique Orders", 
       x = "Site") +
  theme_grey() +
  scale_fill_manual(values = c("lightblue3", "orange2")) #colourblind friendly colours
