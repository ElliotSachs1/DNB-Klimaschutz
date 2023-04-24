library(ggplot2)

data <- read.csv('dnb2.csv', sep =';')
data$year <- as.factor(data$year)

year_table <- table(data$year)[names(table(data$year)) != "0000" & names(table(data$year)) != "9999" & !is.na(names(table(data$year)))]

year_df <- data.frame(year = as.numeric(gsub("\\[|\\]", "", names(year_table))), frequency = as.numeric(year_table))

jump <- 5

ggplot(year_df, aes(x = year, y = frequency)) +
  geom_bar(stat = "identity", fill = "#5F9EA0") +
  labs(x = "Publikationsjahr", y = "Publikationen", title = "Häufigkeit des Wortes 'Klimaschutz' in den Titeldaten der GND seit 1913") +
  scale_x_continuous(breaks = seq(1915, max(year_df$year, na.rm = TRUE), jump), labels = seq(1915, max(year_df$year, na.rm = TRUE), jump)) +
  theme_minimal() +
  coord_cartesian(xlim = c(1913, max(year_df$year, na.rm = TRUE)), ylim = c(0, max(year_df$frequency) * 1.1)) +
  ggtitle("Häufigkeit des Wortes 'Klimaschutz' in den Titeldaten der GND seit 1913 (Anzahl der Datensätze: 7840)") +
  theme(plot.title = element_text(size = 10, color = "#5F9EA0", face = "bold"), 
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        axis.line = element_line(colour = "black"))














