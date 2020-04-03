library(tidyverse)
library(readxl)
library(dplyr)
library(circlize)
library(igraph)

# Read in data and clean
students <- read_xlsx("STUDENTS1.xlsx") %>% 
                    filter(!is.na(freshman_dorm), block_id != 0) %>% 
                    mutate(freshman_dorm = tolower(freshman_dorm)) %>%
                    select(id, freshman_dorm, block_id, link_id)
  
# Clear previous settings on circlos
circos.clear()

# Self join to identify blocking groups, grouping by ethnicity
joined <- full_join(students, students, c('block_id' = 'block_id')) %>%
          filter(id.x != id.y) %>%
          group_by(freshman_dorm.x, freshman_dorm.y) %>%
          summarise(weight = n())
# Tidy the data in preparation of Adjacency matrix
M2 <- spread(joined, freshman_dorm.y, weight, fill = 0)
rownames(M2) = M2$freshman_dorm.x
M2 <- M2 %>% ungroup() %>% select(-freshman_dorm.x)
M2[upper.tri(M2, diag = FALSE)] <- 0
from = rep(rownames(M2), times = ncol(M2))
to = rep(colnames(M2), each = nrow(M2))
values <- M2 %>% ungroup() %>% 
  gather("freshman_dorm.x", "value", "apley":"wigglesworth")
# Adjacency matrix
df <- data.frame(from, to, values$value, stringsAsFactors = FALSE) %>%
    filter(to != "freshman_dorm.x")

# Plot chord diagram. Symmetric = FALSE is key for showing blocking groups within same race

chordDiagram(df, transparency = 0.5, self.link = 2,
           annotationTrack = "grid", symmetric = FALSE,
           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))}, bg.border = NA) 

