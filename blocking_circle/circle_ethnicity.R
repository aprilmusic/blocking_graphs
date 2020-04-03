library(tidyverse)
library(readxl)
library(dplyr)
library(circlize)
library(igraph)

students <- read_xlsx("STUDENTS1.xlsx") %>% 
  filter(!is.na(ethnicity), block_id != 0, !(ethnicity %in% c("Prefer not to say", "Native American"))) %>% 
  mutate(ethnicity = recode(ethnicity, "Other/multracial" = "Other/multiracial")) %>%
  select(id, ethnicity, block_id, link_id)

circos.clear()

joined <- full_join(students, students, c('block_id' = 'block_id')) %>%
  filter(id.x != id.y) %>%
  group_by(ethnicity.x, ethnicity.y) %>%
  summarise(weight = n())
M2 <- spread(joined, ethnicity.x, weight, fill = 0)
rownames(M2) = M2$ethnicity.y

from = rep(rownames(M2), times = ncol(M2)-1)
to = rep(colnames(M2), each = nrow(M2))[6:30]
values <- M2 %>% ungroup() %>% 
  gather("ethnicity.x", "value", "Asian":"White")
df <- data.frame(from, to, values$value, stringsAsFactors = FALSE) %>%
  filter(to != "ethnicity.x")


g <- graph_from_data_frame(joined, directed = TRUE)
# Create adjacency matrix from graph
M <- as.matrix(as_adjacency_matrix(g), attr = "weight")
# #
chordDiagram(df, transparency = 0.5, self.link = 2,
             annotationTrack = "grid", symmetric = FALSE,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.3))}, bg.border = NA) 
