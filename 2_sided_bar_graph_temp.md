soil<-read.table(file="clipboard",sep="\t", header = TRUE)
soil
str(soil)
head(soil)
# install.packages(c("tidyr","dplyr","ggplot2"))
library(tidyr)
library(dplyr)
library(ggplot2)

soil_long <- soil %>%
  pivot_longer(
    cols = c(pH, OM., TN., P2O5, K2O),
    names_to = "Parameter",
    values_to = "Value"
  )

# Define which variables go on left vs right axis
left_vars <- c("pH", "OM.")
right_vars <- c("TN.", "P2O5", "K2O")

# Scaling factor to bring right vars closer to left scale
scaleFactor <- 20   # Adjust until the bars align well visually

soil_long <- soil_long %>%
  mutate(ScaledValue = ifelse(Parameter %in% right_vars, Value/scaleFactor, Value))


ggplot(soil_long, aes(x = Treatment, y = ScaledValue, fill = Parameter)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(
    name = "pH, OM%",
    sec.axis = sec_axis(~.*scaleFactor, name = "TN%*100, P2O5 mg/kg, K2O mg/kg")
  ) +
  labs(title = "Soil Nutrient Content ASD Sunsari ") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

