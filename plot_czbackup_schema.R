uur_labels = c(0:25) %>% as.character
uur_breaks = seq(from = 0, to = 25, 1)

ggplot(sessions, aes(x = begin, y = sessienaam, fill = server)) +
  geom_segment(aes(x = begin, xend = eind, yend = sessienaam), colour = "#8C8C8C", size = 2, na.rm = TRUE) +
  geom_label(aes(x = begin, y = sessienaam, label = begin), na.rm = TRUE) +
  geom_label(aes(x = eind, y = sessienaam, label = eind), na.rm = TRUE) +
  scale_x_continuous(limits=c(0, 25), expand = c(0, 1), breaks = uur_breaks, labels = uur_labels) +
  scale_fill_manual(values = c("log-mac" = "#F37735", "uitzend-mac" = "#00AEDB")) +
  labs(title = "CZ backups", 
       x = "\nopgenomen uren",
       y = "") +
  theme(plot.title = element_text(size = rel(2)),         
        axis.text.x = element_text(size = 11, colour = "black"),
        axis.text.y = element_text(size = 11, colour="black"),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.key = element_rect(size = 0, colour = "white"),
        legend.key.size = unit(1, "lines"),
        legend.title = element_text(size  = 12),
        axis.ticks.y = element_line(size = 0),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=18)
  ) +
  facet_grid(dag ~ ., scales = "free")

