library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(scales)

caminho_pasta <- "G:/My Drive/POS DOC/LASIS - Claudio/Análises/Analise 16-05"
caminho_nova_pasta <- file.path(caminho_pasta, "dados_corrigidos")
# Verificar se a pasta existe; se não, cria a pasta
if (!dir.exists(caminho_nova_pasta)) {
  dir.create(caminho_nova_pasta)
}

arquivos <- list.files(path = caminho_pasta, pattern = "\\.txt$", full.names = TRUE)

for (arq in arquivos) {
  
# --- Importar os dados ---
# Substitua pelo seu arquivo, se necessário
df <- read.table(arq,
                 comment.char = "#",
                 sep = "\t",
                 dec = ".")

# Nomear colunas
colnames(df) <- c("Numero_de_onda", "Intensidade")

# --- Definir intervalo e referência ---
x_min <- 510
x_max <- 530
x_ref <- 520

# --- Encontrar pico máximo no intervalo ---
ajuste <- df %>%
  filter(Numero_de_onda >= x_min & Numero_de_onda <= x_max) %>%
  slice_max(order_by = Intensidade, n = 1)

# Se não houver pico no intervalo, pula o arquivo
if (nrow(ajuste) == 0) {
  message("Nenhum pico em ", arq, " dentro do intervalo ", x_min, "-", x_max)
  next
}

# Calcular a diferença entre o pico e x_ref
diferenca <- ajuste$Numero_de_onda - x_ref

# --- Corrigir eixo x ---
df_corrigido <- df %>%
  mutate(Numero_de_onda_corrigido = Numero_de_onda - diferenca)

# Nome do arquivo base sem extensão
nome_original <- basename(arq)
nome_base <- str_remove(nome_original, "\\.txt$")
nome_saida <- str_c(str_extract(nome_base, "^.*?LOC \\d+"), " Raman", sep = "") %>%
  str_replace_all("[^\\w\\s-]", "") %>%
  str_trim()

# --- Definir intervalo e referência ---
x_Raman1_min <- 370
x_Raman1_max <- 390

x_Raman2_min <- 395
x_Raman2_max <- 420

# --- Encontrar pico máximo no intervalo (distancia picos de Raman) ---
ajusteE2g <- df %>%
  filter(Numero_de_onda >= x_Raman1_min & Numero_de_onda <= x_Raman1_max) %>%
  slice_max(order_by = Intensidade, n = 1)

# --- Encontrar pico máximo no intervalo (distancia picos de Raman) ---
ajusteA1g <- df %>%
  filter(Numero_de_onda >= x_Raman2_min & Numero_de_onda <= x_Raman2_max) %>%
  slice_max(order_by = Intensidade, n = 1)

# Calcular a diferença entre o pico e x_ref
diferenca_picos <- ajusteA1g$Numero_de_onda - ajusteE2g$Numero_de_onda

# --- Salvar os dados corrigidos em um novo arquivo TXT ---
caminho_saida_corrigido <- file.path(caminho_nova_pasta, paste0(nome_saida, "_corrigido.txt"))

# Salvar o dataframe corrigido com as colunas selecionadas
write.table(df_corrigido, 
            file = caminho_saida_corrigido, 
            sep = "\t", 
            row.names = FALSE, 
            col.names = TRUE, 
            quote = FALSE)


# --- Plotar espectros original e corrigido ---
q <- ggplot() +
  geom_line(data = df_corrigido, aes(x = Numero_de_onda_corrigido, y = Intensidade), color = "red", size = 1, alpha = 0.6) +
  labs(title = nome_saida,
       subtitle = paste("Diferença entre os picos característicos (A1g e E2g):", round(diferenca_picos, 3)),
       x = "Número de onda (cm-1)", y = "Intensidade (u.a.)") +
  theme_minimal() +
  theme(axis.title.x = element_text(face = "bold"),  # Eixo X em negrito
        axis.title.y = element_text(face = "bold"),  # Eixo Y em negrito
        axis.text.y = element_blank(),  # Remove os valores do eixo Y
        axis.text.x = element_text(face = "bold")) + # Eixo X com os valores em negrito
  xlim(375, 415)

# Salvar o gráfico em formato PNG
ggsave(
  filename = file.path(caminho_nova_pasta, paste0(nome_saida, ".png")),
  plot = q, width = 8, height = 6, dpi = 300
)
}