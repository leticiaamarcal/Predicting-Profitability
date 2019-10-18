#------------------------------------------------------------------------------- 
#Goal: Fazer gráficos
#Description: visualizar meu dado e meu modelo 
#Developer: Letícia Marçal
#--------------------------------------------------------------------------------

#library
library(dplyr)

#linkar com o predictions
source("Scripts/Predictions.R")

#tirar o Profitmargin para acrescentar depois
newProducts <- newProducts[,-8]

#barras para mostrar a importancia do tipo de produto
newProducts %>%
  mutate(product_type = newData$ProductType,
         profit_m = newData$ProfitMargin,
         profit = VolumeRF*profit_m) %>%
  group_by(product_type) %>%
  summarise(mean_profit = mean(profit),
            count = n()) %>%
  ggplot() +
  geom_col(
    aes(x = reorder(product_type, mean_profit),
        y = mean_profit,
        fill = (product_type %in% c("PC", "Netbook", "Laptop","Smartphone"))
    )) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values = c("grey","darkred")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank())


