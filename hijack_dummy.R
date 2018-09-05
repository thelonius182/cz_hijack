mydata <- read.table(
  header = TRUE,
  text = "
  sessie            dag  uur  grens server  
  3-RoD-nachten      ma   18    b     b  
  3-RoD-nachten      ma   24    e     b  
  Live-uitzendingen  ma   10    b     a  
  Live-uitzendingen  ma   13    e     a  
  rod03              di   00    b     b  
  rod03              di   03    e     b  
  rod05              di   00    b     a  
  rod05              di   07    e     a  
  rod04              wo   17    b     a  
  rod04              wo   25    e     a  
  "
)

mydata$server = factor(mydata$server, levels = c("a", "b"), labels = c("log-mac", "uitzend-mac"))
mydata$dag = factor(mydata$dag, levels = c("wo", "di", "ma"), labels = c("woensdag", "dinsdag", "maandag"))
mydata$group = factor(mydata$dag)

uur_labels = c(0:25) %>% as.character
uur_breaks = seq(from = 0, to = 25, 1)

ggplot(mydata, aes(x = uur, y = sessie, fill = server)) +
  scale_x_continuous(limits=c(0, 25), expand = c(0, 1), breaks = uur_breaks, labels = uur_labels) +  
  scale_fill_manual(values = c("log-mac" = "#F37735", "uitzend-mac" = "#00AEDB")) +
  labs(title = "CZ backups", 
       x = "\nopgenomen uren",
       y = "") +
  theme(plot.title = element_text(size = rel(2)),         
        axis.text.x = element_text(size = 11, colour = "black"),
        axis.text.y = element_text(size = 11, colour="black"),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.key = element_rect(size = 0, colour = "white"),
        legend.key.size = unit(1, "lines"),
        legend.title = element_text(size  = 12),
        axis.ticks.y = element_line(size = 0),
        panel.grid.minor = element_blank()
  ) +
  geom_line(size = 2, colour = "#8C8C8C", na.rm = TRUE) +
  geom_label(aes(label = uur), na.rm = TRUE) +
  facet_grid(group ~ ., scales = "free") 
  
  