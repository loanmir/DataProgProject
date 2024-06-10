library(tidyverse)
library(skimr)
library(reshape2)
library(ggcorrplot)
mtcars
skim(mtcars)

dataset <- read.csv("student_data.csv", header = T)
mean(dataset$G3)

dataset %>% select(Fjob,G3) %>% model.matrix(~0+., data=.) %>%
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = FALSE, type="full", lab=TRUE, lab_size = 2,lab_col = "white" ,ggtheme = ggplot2::theme_gray,
             colors = c("white","red","black"), outline.color = "white")


melted <- melt(dataset)

heatmap <- ggplot(melted, aes(absences,G1,G2,G3))+
  geom_tile(color="white")+
  scale_fill_gradient(low="white", high="black" ,mid="red",
                      midpoint = 0, limit = c(-1,1), space = "Lab",
                      name = "Correlation") +
  theme_minimal()+
  coord_fixed()+
  labs(title = "Correlation Heatmap")

dataset %>% select(Fjob,G3) %>%
  group_by(Fjob) %>% summarize(mG3 = mean(G3)) %>%
  ggplot(., aes(Fjob, mG3)) +
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Job vs. Grade (Father)",x = "Job", y = "G3")+
  theme_minimal()


#JOB vs. GRADE Mother and Father together facet_wrap()
dataset_long <- dataset %>%  
  pivot_longer(cols = c(Mjob,Fjob), names_to = "jobs",
               values_to = "value")
dataset_long %>% select(jobs,value, G3) %>% group_by(value,jobs) %>% summarize(mmG3 = mean(G3)) %>%
  ggplot(dataset_long,aes(value, mmG3)) +
  geom_bar(width = 0.8, stat = "identity") +
  facet_wrap(~jobs, scales = "free_y") +
  labs(x = "Age", y = "Mean", fill = "Age") 
dataset_long %>% select(jobs,value, G3) %>% group_by(value,jobs) %>% summarize(mmG3 = mean(G3))
#\\\\\\\\\\\\\\\\\\\

dataset %>% select(absences,G1,G2,G3) %>% group_by(absences,G1,G2,G3) -> df1
matrix(df1)
as_tibble(df1)
heatmap(as.matrix(df1))
