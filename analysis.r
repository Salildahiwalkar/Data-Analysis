library(readxl)
library(ggplot2)
library(tidyr)
library(gridExtra)
data <- read_excel("data.xlsx")
print(data)

#Exploratory Data Visualisation
A <- data$Points
#mean
print(paste("The mean Points acquired are: ", mean(A)))
#median
print(paste("The mean Points acquired are: ", median(A)))
#mode
mode <- function(x){
  ta <- table(x)
  tam <- max(ta)
  if (all(ta == tam)){
    mod <- NA
  }else {
    if (is.numeric(x))
      mod <- as.numeric(names(ta)[ta == tam])
    else
      mod <- names(ta)[ta == tam]
  }
}
print(paste("The Mode Pointes acquired are: ", mode(A)))
#standard deviation
print(paste("The Standard Deviation of Pointes acquired are: ", sd(A)))
#variance
print(paste("The variance of Pointes acquired are: ", var(A)))
#quartiles
print("The Inter Quartile range of Pointes acquired are: ")
print(quantile(A))

#Bar Graph comparing Clubs on basis of Points they secured
bar <- ggplot(data, aes(Club, Points, fill = Club)) +
  geom_bar(stat = "identity") +
  labs(title = "Points Secured by Club in Premier League 2022-23",
       x = "Clubs", y = "Points",
       caption = "Source: Premier League Website") +
  theme(plot.caption = element_text(hjust = 0, face = "italic"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(labels = label_wrap_gen(10))+
  scale_fill_manual(values = c("#EF0107",
                               "#670E36",
                               "#B50E12",
                               "#FFB400",
                               "#0057B8",
                               "#034694",
                               "#A7A5A6",
                               "#003399",
                               "#000000",
                               "#1D428A",
                               "#FDBE11",
                               "#C8102E",
                               "#6CABDD",
                               "#DA291C",
                               "#241F20",
                               "#DD0000",
                               "#FDB913",
                               "#132257",
                               "#7A263A",
                               "#FDB913"))

#Multiple Bar Graph comparing Club on their Performance
data1 <- data[, -c(7:10)]
data1 <- pivot_longer(data1, cols = c("Won", "Drawn", "Lost"),
                      names_to = "Performance", values_to = "Numbers")
print(n = 60, data1)
perf_colours <- c(Drawn = "blue", Lost = "red", Won = "green")
perf <- ggplot(data1, aes(Numbers, Club, fill = Performance)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance of Clubs in Premier League 2022-23",
       x = "Performance Numbers", y = "Clubs",
       caption = "Source: Premier League Website") +
  theme(plot.caption = element_text(hjust = 0, face = "italic"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = perf_colours) +
  scale_y_discrete(labels = label_wrap_gen(10))


grid.arrange(bar, perf, nrow = 2) 