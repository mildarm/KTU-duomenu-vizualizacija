# 1 uzduotis


library(tidyverse)
data<-read_csv("https://github.com/mildarm/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
summary(data)
str(data)

data %>%
  filter(ecoActCode == 731100) %>%
  ggplot(aes(x=avgWage))+
  geom_histogram(bins = 200)
ggsave("1_uzduotis.png")

#2 uzduotis

data %>%
  filter(ecoActCode == 731100) %>%
  group_by(name) %>%
  summarise(avgWageGroup = mean(avgWage, na.rm = TRUE)) %>%
  arrange(desc(avgWageGroup)) %>%
  top_n(5) 

library(lubridate)

data %>%
  filter(code %in% c(2324126, 2062457, 2385489, 2024352, 729917)) %>%
  mutate(month = ym(month)) %>%
  ggplot(aes(x = month, y = avgWage, group = name, color = name))+
  geom_line()+
  geom_point()
ggsave("2_uzduotis.png")


# 3 uzduotis

data %>%
  filter(code %in% c(2324126, 2062457, 2385489, 2024352, 729917)) %>%
  group_by(name) %>%
  summarise(apdraustieji = max(numInsured, na.rm = TRUE)) %>%
  arrange(desc(apdraustieji)) %>%
  ggplot(aes(x = reorder(name,-apdraustieji), y = apdraustieji, fill = name))+
  geom_bar(stat = "summary")+
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.2))
ggsave("3_uzduotis.png")


