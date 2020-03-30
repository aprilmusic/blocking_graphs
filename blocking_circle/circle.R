library(tidyverse)
library(readxl)
library(dplyr)
library(circlize)
library(igraph)

students <- read_xlsx("STUDENTS1.xlsx") %>% 
                    filter(!is.na(freshman_dorm), block_id != 0) %>% 
                    mutate(freshman_dorm = tolower(freshman_dorm)) %>%
                    select(id, freshman_dorm, block_id, link_id)
circos.clear()

joined <- full_join(students, students, c('block_id' = 'block_id')) %>%
          filter(id.x != id.y) %>%
          group_by(freshman_dorm.x, freshman_dorm.y) %>%
          summarise(weight = n())
M2 <- spread(joined, freshman_dorm.y, weight, fill = 0)
rownames(M2) = M2$freshman_dorm.x

from = rep(rownames(M2), times = ncol(M2)-1)
to = rep(colnames(M2), each = nrow(M2))[18:306]
values <- M2 %>% ungroup() %>% 
  gather("freshman_dorm.x", "value", "apley":"wigglesworth", fill = 0)
df <- data.frame(from, to, values$value, stringsAsFactors = FALSE) %>%
    filter(to != "freshman_dorm.x")


g <- graph_from_data_frame(joined, directed = TRUE)
# Create adjacency matrix from graph
M <- as.matrix(as_adjacency_matrix(g), attr = "weight")
# #
chordDiagram(df, transparency = 0.5, self.link = 1,
           annotationTrack = "grid", 
           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))}, bg.border = NA) 
