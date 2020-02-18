library(tidyverse)
library(janitor)
library(readxl)

raw_data <- read_excel("raw_data.xlsx") 

raw_data <- raw_data %>% 
  select(randomId, musicTraining, c("...37":"...58")) 

names_list <- NULL
for (i in 1:21) {
  names_list <- c(names_list, paste0("point_",i))
}

colnames(raw_data) <- c("ID", "music_training", "start", names_list)

# exclude anyone where we don't have their music training score
raw_data <- raw_data %>%
  mutate(music_training = recode(music_training, "10+" = "10")) %>%
  filter(!is.na(music_training))

tidy_data <- raw_data %>% 
  repair_names() %>% 
  separate("start", into = c("start_time", NA), sep = ";;") %>%
  mutate(start_time = as.integer(start_time)) %>%
  gather(key = "point", value = "value", names_list)

# remove error presses, presses beyond the end of the performance, baseline for
# start time, remove the string ";;finish" indicating last button press and 
# remove mispresses after the end of the piece
tidy_data_filtered <- tidy_data %>%
  filter(!str_detect(value, "error")) %>%
  mutate_all(~gsub(";;finish", "", .)) %>%
  mutate(start_time = as.integer(start_time)) %>%
  mutate(value = as.integer(as.integer(value) - start_time)) %>%
  mutate(time = as.integer(as.integer(value)/1000)) %>%
  filter(time < 5e+02)

# Plot histogram 
tidy_data_filtered %>%
  ggplot(aes(x = time)) +
  geom_histogram(bins = 200) +
  labs(title = "Number of button presses throughout the duration of the piece",
       x = "Time (s)", 
       y = "Number of presses") +
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))

tidy_data$music_training <- as.integer(tidy_data$music_training)
tidy_data_filtered$music_training <- as.integer(tidy_data_filtered$music_training)

tidy_data_filtered %>%
  mutate(time = as.integer(value)/1000) %>%
  ggplot(aes(x = time, y = reorder(ID, music_training), colour = music_training)) +
  geom_point(alpha = .2, size = 5) +
  theme(axis.text.y = element_blank()) +
  labs(x = "Time", y = NULL) +
  guides(colour = FALSE) +
  geom_vline(xintercept = 7.11, alpha = .75) +
  geom_vline(xintercept = 13.98) +
  geom_vline(xintercept = 15.7) +
  geom_vline(xintercept = 22.87) +
  geom_vline(xintercept = 23.855) +
  geom_vline(xintercept = 24.52) +
  geom_vline(xintercept = 33.385) +
  geom_vline(xintercept = 39.05) +
  geom_vline(xintercept = 47.585) +
  geom_vline(xintercept = 55.675) +
  geom_vline(xintercept = 65.295) +
  geom_vline(xintercept = 76.51) +
  geom_vline(xintercept = 88.485) +
  geom_vline(xintercept = 97.005) +
  geom_vline(xintercept = 114.915) +
  geom_vline(xintercept = 116.445) +
  geom_vline(xintercept = 123.73) +
  geom_vline(xintercept = 130.925) +
  geom_vline(xintercept = 142.17) +
  geom_vline(xintercept = 170.375) +
  geom_vline(xintercept = 177.665) 
  
# create long data frame based on 9 segments
ID <- unique(tidy_data_filtered$ID)

seg <- c("Seg1", "Seg2", "Seg3", "Seg4", "Seg5", "Seg6", "Seg7", "Seg8", "Seg9",
         "Seg10", "Seg11", "Seg12", "Seg13", "Seg14", "Seg15", "Seg16", "Seg17",
         "Seg18", "Seg19", "Seg20", "Seg21")

my_df <- expand_grid(ID, seg)

#my_df$press <- NA

music_data <- tidy_data_filtered %>%
  select(c(ID, music_training)) %>%
  distinct(ID, .keep_all = TRUE)
  
my_df <- left_join(my_df, music_data , by = "ID")

# name segments so the are of the range segment change +- 1 second

tidy_data_filtered$seg <- "0"
tidy_data_filtered[tidy_data_filtered$time > 7.10 & tidy_data_filtered$time < 9.12,]$seg <- "Seg1"
tidy_data_filtered[tidy_data_filtered$time > 13.97 & tidy_data_filtered$time < 15.99,]$seg <- "Seg2"
tidy_data_filtered[tidy_data_filtered$time > 15.69 & tidy_data_filtered$time < 17.71,]$seg <- "Seg3"
tidy_data_filtered[tidy_data_filtered$time > 22.86 & tidy_data_filtered$time < 24.88,]$seg <- "Seg4"
tidy_data_filtered[tidy_data_filtered$time > 23.845 & tidy_data_filtered$time < 25.865,]$seg <- "Seg5"
tidy_data_filtered[tidy_data_filtered$time > 24.51 & tidy_data_filtered$time < 26.53,]$seg <- "Seg6"
tidy_data_filtered[tidy_data_filtered$time > 33.375 & tidy_data_filtered$time < 35.395,]$seg <- "Seg7"
tidy_data_filtered[tidy_data_filtered$time > 39.04 & tidy_data_filtered$time < 41.06,]$seg <- "Seg8"
tidy_data_filtered[tidy_data_filtered$time > 47.575 & tidy_data_filtered$time < 49.595,]$seg <- "Seg9"
tidy_data_filtered[tidy_data_filtered$time > 55.665 & tidy_data_filtered$time < 57.685,]$seg <- "Seg10"
tidy_data_filtered[tidy_data_filtered$time > 65.285 & tidy_data_filtered$time < 67.305,]$seg <- "Seg11"
tidy_data_filtered[tidy_data_filtered$time > 76.5 & tidy_data_filtered$time < 78.52,]$seg <- "Seg12"
tidy_data_filtered[tidy_data_filtered$time > 88.475 & tidy_data_filtered$time < 90.495,]$seg <- "Seg13"
tidy_data_filtered[tidy_data_filtered$time > 96.995 & tidy_data_filtered$time < 98.015,]$seg <- "Seg14"
tidy_data_filtered[tidy_data_filtered$time > 114.905 & tidy_data_filtered$time < 116.925,]$seg <- "Seg15"
tidy_data_filtered[tidy_data_filtered$time > 116.435 & tidy_data_filtered$time < 118.455,]$seg <- "Seg16"
tidy_data_filtered[tidy_data_filtered$time > 123.72 & tidy_data_filtered$time < 125.74,]$seg <- "Seg17"
tidy_data_filtered[tidy_data_filtered$time > 130.915 & tidy_data_filtered$time < 132.935,]$seg <- "Seg18"
tidy_data_filtered[tidy_data_filtered$time > 142.16 & tidy_data_filtered$time < 144.18,]$seg <- "Seg19"
tidy_data_filtered[tidy_data_filtered$time > 170.365 & tidy_data_filtered$time < 172.385,]$seg <- "Seg20"
tidy_data_filtered[tidy_data_filtered$time > 177.655 & tidy_data_filtered$time < 179.675,]$seg <- "Seg21"

tidy_data_filtered$press <- 1

# Need to check whether more than one press in each segment for each participant - check
# total df size

tidy_data_filtered %>%
  filter(seg != 0) %>%
  group_by(seg, ID) %>%
  tally(press) 

joined_data <- left_join(my_df, select(tidy_data_filtered, 
                                       c("ID", "seg", "press")), by = c("ID", "seg")) 

joined_data[is.na(joined_data$press),]$press <- 0
joined_data$music_training <- as.integer(joined_data$music_training)

write_csv(joined_data, "music_training_plus2s.csv")

# look at each segment
# segment 1
model_seg1 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg1",], family = "binomial")
summary(model_seg1)

# segment 2
model_seg2 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg2",], family = "binomial")
summary(model_seg2)

# segment 3
model_seg3 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg3",], family = "binomial")
summary(model_seg3)

# segment 4
model_seg4 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg14",], family = "binomial")
summary(model_seg4)

# segment 5
model_seg5 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg5",], family = "binomial")
summary(model_seg5)

# segment 6
model_seg6 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg6",], family = "binomial")
summary(model_seg6)

# segment 7
model_seg7 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg7",], family = "binomial")
summary(model_seg7)

# segment 8
model_seg8 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg8",], family = "binomial")
summary(model_seg8)

# segment 9
model_seg9 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg9",], family = "binomial")
summary(model_seg9)

# segment 10
model_seg10 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg10",], family = "binomial")
summary(model_seg10)

# segment 11
model_seg11 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg11",], family = "binomial")
summary(model_seg11)

# segment 12 - p = .0753
model_seg12 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg12",], family = "binomial")
summary(model_seg12)

# segment 13
model_seg13 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg13",], family = "binomial")
summary(model_seg13)

# segment 14
model_seg14 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg14",], family = "binomial")
summary(model_seg14)

# segment 15
model_seg15 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg15",], family = "binomial")
summary(model_seg15)

# segment 16 - p = .0358
model_seg16 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg16",], family = "binomial")
summary(model_seg16)

# segment 17
model_seg17 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg17",], family = "binomial")
summary(model_seg17)

# segment 18
model_seg18 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg18",], family = "binomial")
summary(model_seg18)

# segment 19
model_seg19 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg19",], family = "binomial")
summary(model_seg19)

# segment 20
model_seg20 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg20",], family = "binomial")
summary(model_seg20)

# segment 21
model_seg21 <- glm(press ~ music_training, data = joined_data[joined_data$seg == "Seg21",], family = "binomial")
summary(model_seg21)

joined_data %>%
  group_by(seg, press) %>%
  count() %>%
  filter(press == 1) %>%
  ungroup() %>%
  add_row(seg = "Seg1", press = 1, n = 0) %>%
  add_row(seg = "Seg2", press = 1, n = 0) %>%
  add_row(seg = "Seg4", press = 1, n = 0) %>%
  add_row(seg = "Seg17", press = 1, n = 0) %>%
  mutate(seg, seg = factor(seg, levels = c("Seg1", "Seg2", "Seg3", "Seg4", "Seg5", "Seg6", "Seg7", "Seg8", "Seg9",
                                      "Seg10", "Seg11", "Seg12", "Seg13", "Seg14", "Seg15", "Seg16", "Seg17",
                                      "Seg18", "Seg19", "Seg20", "Seg21"))) %>%
  ggplot(aes(x = seg, y = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 15)) +
  labs(title = "Clicks per segment",
       x = "Segment Number", 
       y = "Number of Clicks")


