library(tidyverse)
library(agricolae)

Josh <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Projects\\Data\\Josh_data.csv", 
                 stringsAsFactors = TRUE)


# view(Josh)
glimpse(Josh)

Joshmale <-Josh %>% 
  filter(Sex=="Male")
mean(Joshmale$X4HNE)

attach(Josh)
unique(Sex)

Josh1 <- Josh %>% 
  filter(Sex == "Female")

min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

Josh1 <-Josh1 %>%
  mutate(
    X4HNEnom = min_max_normalize(X4HNE),
    VDACnom = min_max_normalize(VDAC)
  )

anov_4HNE <- aov(X4HNE~ Mouse_Type*Age_months, data = Josh1)
summary(anov_4HNE)

tukey_anov_4HNE <- HSD.test(anov_4HNE, trt = c("Mouse_Type", "Age_months"), group = TRUE)
print(tukey_anov_4HNE$groups)

tukey_anov_4HNE <- LSD.test(anov_4HNE, trt = c("Mouse_Type", "Age_months"), group = TRUE)
print(tukey_anov_4HNE$groups)
tukey_anov_4HNE$means
#v ---------------------


anov_VDAC <- aov(VDAC~ Mouse_Type*Age_months, data = Josh1)
summary(anov_VDAC)

tukey_anov_VDAC <- HSD.test(anov_VDAC, trt = c("Mouse_Type", "Age_months"), group = TRUE)
print(tukey_anov_VDAC$groups)
tukey_anov_VDAC$means
tukey_anov_VDAC$

# ______________________________ using the normalized data ________________---



anov_X4HNEnom <- aov(X4HNEnom~ Mouse_Type*Age_months, data = Josh1)
summary(anov_X4HNEnom)

tukey_X4HNEnom <- HSD.test(anov_X4HNEnom, trt = c("Mouse_Type", "Age_months"), group = TRUE)
print(tukey_X4HNEnom$groups)

#-------------------------
anov_VDACnom <- aov(VDACnom~ Mouse_Type*Age_months, data = Josh1)
summary(anov_VDACnom)

tukey_anov_VDACnom <- HSD.test(anov_VDACnom, trt = c("Mouse_Type", "Age_months"), group = TRUE)
print(tukey_anov_VDACnom$groups)


anov_VDACnom <- aov(VDACnom~ Mouse_Type*Age_months, data = Josh1)
summary(anov_VDACnom)

tukey_anov_VDACnom <- HSD.test(anov_VDACnom, trt = c("Mouse_Type", "Age_months"), group = TRUE)
print(tukey_anov_VDACnom$groups)


# Same result; no much difference


# ______________________________________________________________________________________________________






X4HNEplot <- Josh1 %>%
  select(X4HNE, Mouse_Type, Age_months) %>%
  mutate(
    Age_months = as.factor(Age_months),
    Mouse_Type = as.factor(Mouse_Type)
  ) %>%
  ggplot(aes(x = Age_months, y = X4HNE, fill = Mouse_Type)) +
  geom_bar(
    stat = "summary", fun = "mean", 
    position = position_dodge(width = 0.8),
    color = "black", width = 0.6, alpha = 0.6
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    width = 0.4, size = 0.7,
    position = position_dodge(width = 0.8)
  ) +
  geom_point(
    aes(color = Mouse_Type),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 3, alpha = 0.8
  ) +
  geom_hline(yintercept = 58644570, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("orange", "blue")) +
  scale_color_manual(values = c("orange", "blue")) +
  labs(x = "Age (months)", y = "4HNE (RFU/cell/Area)") +
  guides(
    color = guide_legend(title = " "),
    shape = "none",
    fill = "none"
  ) +
  theme(
    text = element_text(family = "Times New Roman", size = 24),
    legend.position = "none"
  ) +
  theme_classic()
print(X4HNEplot)

ggsave(plot= X4HNEplot, 
       file = "C:\\Users\\DELL\\Desktop\\X4HNEplot.jpg",
       height = 3, width= 5)
# ___________________


VDACplot <- Josh1 %>%
  select(VDAC, Mouse_Type, Age_months) %>%
  mutate(
    Age_months = as.factor(Age_months),
    Mouse_Type = as.factor(Mouse_Type)
  ) %>%
  ggplot(aes(x = Age_months, y = VDAC, color = Mouse_Type, fill = Mouse_Type)) +
  
  stat_summary(
    fun = mean, geom = "point", 
    shape = 21, size = 4, 
    position = position_dodge(width = 0.8),
    color = "black" # Outline color for mean dots
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    width = 0.4, size = 0.7,
    position = position_dodge(width = 0.8)
  ) +
  
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 2, alpha = 0.5
  ) +
  geom_hline(yintercept = 14524611, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("orange", "blue")) +
  scale_color_manual(values = c("orange", "blue")) +
  labs(x = "Age (months)", y = "VDAC (RFU/cell/Area)") +
  guides(
    color = guide_legend(title = " "),   
    shape = "none", 
    fill = "none"  
  )+
  theme(
    text = element_text(family = "Times New Roman", size = 24),
    legend.position = "none"
  ) +
  theme_classic()
print(VDACplot)

ggsave(plot= VDACplot, 
       file = "C:\\Users\\DELL\\Desktop\\VDACplot.jpg",
       height = 3, width= 5)
