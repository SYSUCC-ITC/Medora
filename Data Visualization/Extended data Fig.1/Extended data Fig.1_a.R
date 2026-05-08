library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(readxl)

d1 <- read_excel("\\Extended data Fig.1_a_1.xlsx")
d2 <- read_excel("\\Extended data Fig.1_a_2.xlsx")

colnames(d1) <- c("from", "to")
colnames(d2) <- c("from", "to")


edges <- rbind(d1, d2)


vertices <- data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to)))
)


vertices$group <- edges$from[match(vertices$name, edges$to)]


vertices$id <- seq_len(nrow(vertices))
vertices$angle <- 90 - 360 * vertices$id / nrow(vertices)

vertices$hjust <- ifelse(vertices$angle < -90, 1, 0)

vertices$angle <- ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)


mygraph <- graph_from_data_frame(edges, vertices = vertices)

custom_colors <- c(
  "Medical Record Generation & Summarization" = "#f97f4c", 
  "Diagnostic & Therapeutic Decision Support" = "#B2182B", 
  "Doctor-Patient Communication & Education" = "#A48AD3", 
  "Medical Quality Control and Management" = "#90C82B", 
  "Treatment Efficacy Assessment & Prognosis" = "#2189AC"  
)


p <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(colour = "grey") + 
  scale_edge_colour_distiller(palette = "RdPu") + 
  

  geom_node_text(
    aes(
      x = x, y = y,  
      filter = !leaf, 
      label = name,
      colour = group  
    ),
    size = 3.5,     
    alpha = 0.9,      
    fontface = "bold"  
  ) +
  
  geom_node_text(
    aes(
      x = x * 1.1, y = y * 1.1, 
      filter = leaf, 
      label = name,
      angle = angle,  
      hjust = hjust, 
      colour = group 
    ),
    size = 3, alpha = 0.9
  ) +
  
  geom_node_point(
    aes(
      filter = leaf, 
      x = x * 1.07, y = y * 1.07,
      colour = group  
    ),
    size = 3,      
    alpha = 0.5      
  ) +
  

  geom_node_point(
    aes(
      filter = !leaf,  
      x = x, y = y,    
      colour = group 
    ),
    size = 20,        
    alpha = 0.8      
  ) +
  

  scale_colour_manual(values = custom_colors) +
  
  scale_size_continuous(range = c(0.1, 10)) + 
  theme_void() + 
  theme(
    legend.position = "none", 
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    aspect.ratio = 1 
  ) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4))

p
ggsave("Extended data Fig.1_a.pdf", p, width = 8, height = 8)
