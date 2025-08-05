
library(readr)
library(dplyr)
library(ggplot2)

rm(list=ls())
getwd()

pref <- read.csv('GWEM_aquamaps.csv')
colnames(pref)[1] <- "species"
pref <- pref[, -((ncol(pref) - 7):ncol(pref))]

pref.out = data.frame()
for(i in 1:nrow(pref)){
  #i=2
  
  #type 0 base AQUAMAPS trapezoids
  abs.min = pref$temp_abs_min[i]
  pref.min = pref$temp_pref_min[i]
  pref.max = pref$temp_pref_max[i]
  abs.max = pref$temp_abs_max[i]
  pref.range = (pref.max-pref.min)
  
  #type 1 is large rectangle
  abs.min1 = abs.min
  pref.min1 = abs.min
  pref.max1 = abs.max
  abs.max1 = abs.max
  
  #type 2 is base trapezoid with narrow preference (triangle)
  pref.range2 = pref.range*.5
  abs.min2 = abs.min
  pref.min2 = pref.min+(pref.range-pref.range2)/2
  pref.max2 = pref.max-(pref.range-pref.range2)/2
  abs.max2 = abs.max
  
  #type 3 is narrow trapezoid inside base preferences
  abs.min3 = pref.min
  pref.min3 = pref.min2
  pref.max3 = pref.max2
  abs.max3 = pref.max
  
  #type 4 is constant
  abs.min4 = 0
  pref.min4 = 0
  pref.max4 = 40
  abs.max4 = 40
  
  #results
  tmp.out = data.frame(species = rep(pref$species[i], 5),
                       variable = rep('temperature', 5),
                       type = c(1, 0, 2, 3, 4),
                       abs.min = c(abs.min1,abs.min,abs.min2,abs.min3,abs.min4),
                       pref.min = c(pref.min1,pref.min,pref.min2,pref.min3,pref.min4),
                       pref.max = c(pref.max1,pref.max,pref.max2,pref.max3,pref.max4),
                       abs.max = c(abs.max1,abs.max,abs.max2,abs.max3, abs.max4))
  pref.out = rbind(pref.out,tmp.out)

}

#Add a column for shape_type and then 1 for response number
response_vals <- pref.out |> 
  mutate(shape_type = "Trapezoid") |> 
  mutate(responsefxn_number = rep(c(201,171,173,166,179,182,210,211,212,164,195), each = 5))

colnames(response_vals) <- c("species", "variable", 
                       "type", "abs_min", 
                       "pref_min", "pref_max",
                       "abs_max", "shape_type", "responsefxn_number")

write.csv(response_vals,"/Users/Nicoleluchau/Pref Func/response_vals.csv", row.names = FALSE)

#Order data for plotting 
#transpose
poly_points <- function(row) {
  data.frame(
    species = row[["species"]],
    type = row[["type"]],
    temp = c(row[["abs.min"]], row[["pref.min"]], row[["pref.max"]], row[["abs.max"]], row[["abs.min"]]),
    pref_value = c(0, 1, 1, 0, 0)
  )
}
poly_data <- data.frame()
for (i in 1:nrow(pref.out)) {
  poly_row <- poly_points(pref.out[i, ])
  poly_data <- rbind(poly_data, poly_row)  
}
poly_data <- filter(poly_data, type != 4)
poly_data$line_type_var <- factor(poly_data$type, levels = c(0,1,2,3))

poly_data$group_id <- interaction(poly_data$species, poly_data$type)

# Define factor levels for group_id including all species/types combinations:
all_species <- unique(poly_data$species)
all_types <- unique(poly_data$type)
desired_order <- interaction(rep(all_species, each = length(all_types)), all_types)
poly_data$group_id <- factor(poly_data$group_id, levels = desired_order)

########################### PLOT TYPE OPTIONS #############################
#Species Specific Plots 
#Option 1: solid filled

ggplot(subset(poly_data, species == "Amberjack"), aes(x = temp, 
                                                     y = pref_value, 
                                                     group = group_id, 
                                                     fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Amberjack") +
  scale_fill_manual(values = c(
    "#ECBBC1",  
    "#BECDDB",  
    "#BFDBB7",  
    "#A2909E"   
  )) + 
  guides(linetype = "none")

#---

#Option 2: transparent filled (alpha)
ggplot(subset(poly_data, species == "Menhaden"), aes(x = temp, 
                                                     y = pref_value, 
                                                     group = interaction(species, type), 
                                                     fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.2, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Menhaden") +
  scale_fill_brewer(palette = "Set1", name = "Type") + 
  guides(linetype = "none")


#---

#Option 3: colored lines 
ggplot(subset(poly_data, species == "Gag Grouper"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = interaction(species, type))) +
  geom_polygon(aes(color = factor(type)), fill = NA, size = 0.8) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Gag Grouper") 

#---
#Option 4: dash types
ggplot(subset(poly_data, species == "Gag Grouper"),aes( x = temp,
                                                        y = pref_value,
                                                        group = interaction(species, type))) +
  geom_polygon(aes(linetype = factor(type)), fill = NA, color = "black", size = 0.8) +
  scale_linetype_manual(values = c("0" = "solid","1" = "dashed", "2" = "dashed","3" = "dashed", "4" = "dashed")) +
  xlim(2, 33) +
  labs( x = "Temperature (C)",y = "Response", title = "Gag Grouper", linetype = "Type")


#I set the x limits at a fixed range so we can see how the pref function shift 
#from species to species. they are now at (2,33) to include grey trigger fish 
#that have that really cold tolerance. otherwise if we wanna zoom in and gray 
#trig can have their own scale, we should use (11,33) or (10,33))

###############################################################################
#Species plots 

ggplot(subset(poly_data, species == "Menhaden"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Menhaden") +
  scale_fill_manual(name = "Type",
    values = c(
    "0" = "#ECBBC1",  
    "1" = "#BECDDB",  
    "2" = "#BFDBB7",  
    "3" = "#A2909E"),
    labels = c("0" = "Reference", 
               "1" = "Rectangular", 
               "2" = "Narrow Pref Range", 
               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none") 
ggsave("menhaden_pref_fxn.png")
#--- 

ggplot(subset(poly_data, species == "Gag Grouper"), aes(x = temp, 
                                                      y = pref_value, 
                                                      group = group_id, 
                                                      fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Gag Grouper") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Gaggrouper_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "Red Grouper"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Red Grouper") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Redgrouper_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "King Mackerel"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "King Mackerel") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Kingmackerel_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "Red Snapper"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Red Snapper") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Redsnapper_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "Vermillion Snapper"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Vermillion Snapper") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Vermillionsnapper_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "Pink Shrimp"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Pink Shrimp") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Pinkshrimp_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "White Shrimp"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "White Shrimp") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) +  
  guides(linetype = "none")
ggsave("Whiteshrimp_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "Brown Shrimp"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Brown Shrimp") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Brownshrimp_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "Amberjack"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Amberjack") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) + 
  guides(linetype = "none")
ggsave("Amberjack_pref_fxn.png")

#---
ggplot(subset(poly_data, species == "Grey Triggerfish"), aes(x = temp, 
                                                        y = pref_value, 
                                                        group = group_id, 
                                                        fill = factor(type))) +
  geom_polygon(aes(linetype = line_type_var), alpha = 0.8, color = "black", size = 0.5) +
  xlim(2, 33) +
  labs(x = "Temperature (C)", y = "Response", title = "Grey Triggerfish") +
  scale_fill_manual(name = "Type",
                    values = c(
                      "0" = "#ECBBC1",  
                      "1" = "#BECDDB",  
                      "2" = "#BFDBB7",  
                      "3" = "#A2909E"),
                    labels = c("0" = "Reference", 
                               "1" = "Rectangular", 
                               "2" = "Narrow Pref Range", 
                               "3" = "Narrow Abs & Pref Range")
  ) +  
  guides(linetype = "none")
ggsave("Greytriggerfish_pref_fxn.png")

#indexing a value in the data frame
pref.out$abs.min[pref.out$species == "Menhaden" & pref.out$type == 3]
