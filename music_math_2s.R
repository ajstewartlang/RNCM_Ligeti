library(tidyverse)
library(car)

maths <- read_csv("maths_training_plus2s.csv")
music <- read_csv("music_training_plus2s.csv")

music_math <- left_join(maths, music, by = c("ID", "seg"))

# look at each segment
# segment 1
model_seg1 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg1",], family = "binomial")
summary(model_seg1)
vif(model_seg1)

# segment 2
model_seg2 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg2",], family = "binomial")
summary(model_seg2)
vif(model_seg2)

# segment 3
model_seg3 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg3",], family = "binomial")
summary(model_seg3)
vif(model_seg3)

# segment 4
model_seg4 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg14",], family = "binomial")
summary(model_seg4)
vif(model_seg4)

# segment 5
model_seg5 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg5",], family = "binomial")
summary(model_seg5)
vif(model_seg5)

# segment 6
model_seg6 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg6",], family = "binomial")
summary(model_seg6)
vif(model_seg6)

# segment 7
model_seg7 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg7",], family = "binomial")
summary(model_seg7)
vif(model_seg7)

# segment 8
model_seg8 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg8",], family = "binomial")
summary(model_seg8)
vif(model_seg8)

# segment 9
model_seg9 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg9",], family = "binomial")
summary(model_seg9)
vif(model_seg9)

# segment 10
model_seg10 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg10",], family = "binomial")
summary(model_seg10)
vif(model_seg10)

# segment 11
model_seg11 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg11",], family = "binomial")
summary(model_seg11)
vif(model_seg11)

# segment 12 - p = .00279
model_seg12 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg12",], family = "binomial")
summary(model_seg12)
vif(model_seg12)

# segment 13
model_seg13 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg13",], family = "binomial")
summary(model_seg13)
vif(model_seg13)

# segment 14
model_seg14 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg14",], family = "binomial")
summary(model_seg14)
vif(model_seg14)

# segment 15
model_seg15 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg15",], family = "binomial")
summary(model_seg15)
vif(model_seg15)

# segment 16 
model_seg16 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg16",], family = "binomial")
summary(model_seg16)
vif(model_seg16)

# segment 17
model_seg17 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg17",], family = "binomial")
summary(model_seg17)
vif(model_seg17)

# segment 18
model_seg18 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg18",], family = "binomial")
summary(model_seg18)
vif(model_seg18)

# segment 19
model_seg19 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg19",], family = "binomial")
summary(model_seg19)
vif(model_seg19)

# segment 20
model_seg20 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg20",], family = "binomial")
summary(model_seg20)
vif(model_seg20)

# segment 21
model_seg21 <- glm(press.x ~ maths_training + music_training, data = music_math[music_math$seg == "Seg21",], family = "binomial")
summary(model_seg21)
vif(model_seg21)

