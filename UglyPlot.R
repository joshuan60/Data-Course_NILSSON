library('tidyverse')
library("ggimage")
buggs <- read.csv("insectgs_dat.csv")
# ggplot(buggs, aes(x = buggs$Order, y = buggs$Mbp)) +
#   geom_violin()
# ggplot(buggs, aes(x = Mbp)) +
#   geom_density()
# First, add your image path as a column in your data

buggs %>% 
  group_by(Order) %>% 
  ggplot(aes(x = reorder(Order, Mbp), y = Mbp, fill = Order)) +
  geom_boxplot() +
  scale_y_log10() + #This log transforms the Mbp by log10
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_jitter(width = 0.25, alpha = 0.05) +
  coord_flip() +
  labs(
    title = "Genome Size Variation Across Insect Orders",
    x = "Insect Order",
    y = "Genome Size (Mbp)"
  ) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90")
        )
# Ugly Plot
buggs$Hjelmen <- "dots.png"

uglybug <- buggs %>% 
  ggplot(aes(x = Order, y = Mbp, fill = Order)) +
  geom_boxplot() +
  geom_image(aes(image = Hjelmen),
             size = 0.02,        # controls image size
             position = position_jitter(width = 2)) +
  labs(
    title = "BUGs I gUeSS",
    x = "What cha want to eat?",
    y = "I think its in the genes ",
    fill = "That's kinda wack my dude"
  ) +
  theme(panel.background = element_rect(fill = "purple"),
        plot.background = element_rect(fill = "#4A412A"),
        panel.grid.major = element_line(color = "red"),
        panel.grid.minor = element_line(color = "pink"),
        legend.background = element_rect(fill = "green"), 
        axis.text.y = element_text(angle = 180, size = 6, color = "blue"),
        legend.title = element_text(color = "#F5F5DC", size = 3),
        plot.title = element_text(size = 6, angle = 180, colour = "purple"),
        axis.text.x = element_text(angle = 10, hjust = -0.5, color = "#00FF00")
        ) +
  scale_fill_discrete(labels = c("a", "b", "c", "d",
                                 "e", "f", "g", "h",
                                 "i", "j", "k", "l",
                                 "m", "n", "o", "p",
                                 "q", "r", "s", "t",
                                 "u", "v", "w", "x", "y"))
ggsave("uglybug.png", plot = uglybug, width = 10, height = 8, dpi = 300)

?element_text()
