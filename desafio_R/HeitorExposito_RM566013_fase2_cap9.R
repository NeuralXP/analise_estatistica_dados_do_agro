#--> Nome: Heitor Exposito  
#--> RM: 566013 
#--> Fase: 2 
#--> Capítulo: 9

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

analise_agro <- read_excel("06_cursos_fiap/fase_2/R/analise_agro1.xlsx")
glimpse(analise_agro)

analise_agro <- analise_agro %>%
  mutate(
    Classe_Produtiva = factor(
      Classe_Produtiva,
      levels = c("Baixo", "Médio", "Alto"),
      ordered = TRUE
    )
  )

media <- mean(analise_agro$Produtividade_Milho_ton_ha, na.rm = TRUE)
mediana <- median(analise_agro$Produtividade_Milho_ton_ha, na.rm = TRUE)

moda <- function(x) {
  ux <- unique(x)
  ux[ which.max(tabulate(match(x, ux)))]
}

moda_pt <- moda(analise_agro$Produtividade_Milho_ton_ha)

cat("Media =", round(media, 3), "\n")
cat("Mediana =", mediana, "\n")
cat("Moda =", moda_pt, "\n\n")

variancia <- var(analise_agro$Produtividade_Milho_ton_ha, na.rm = TRUE)
desvio_padrao <- sd(analise_agro$Produtividade_Milho_ton_ha, na.rm = TRUE)
intervalo_int <- IQR(analise_agro$Produtividade_Milho_ton_ha, na.rm = TRUE)
coef_var <- desvio_padrao / media * 100

cat("Variância =", round(variancia, 3), "\n")
cat("Desvio-padrão =", round(desvio_padrao, 3), "\n")
cat("IQR =", intervalo_int, "\n")
cat("Coeficiente de Variacao =", round(coef_var, 2), "%\n\n")

quartis <- quantile(analise_agro$Produtividade_Milho_ton_ha,
                    probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
decis <- quantile(analise_agro$Produtividade_Milho_ton_ha,
                  probs = seq(0,1, by = 0.1), na.rm = TRUE)

print(quartis)
print(decis)

p1 <- ggplot(analise_agro, aes(x = Produtividade_Milho_ton_ha)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") + 
  labs(
    title = "Histogram de Produtividade (t/ha)",
    x = "Produtividade (t/ha)",
    y = "Frequencia") + theme_minimal()

p2 <- ggplot(analise_agro, aes(y = Produtividade_Milho_ton_ha)) +
  geom_boxplot(fill = "salmon") + labs(
    title = "Boxplot de Produtividade (t/ha)",
    y =  "Produtividade (t/ha)"
  ) + theme_minimal()


print(p1)
print(p2)