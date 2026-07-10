# FAZ A LEITURA DOS RELATORIOS DO ESOCIAL/MENSAGERIA
# E DEIXA ELE MAIS FACIL DE COMPREENCAO

# DRIVER PARA CONEXAO COM O IRIS
# Sys.setenv(JAVA_HOME = "C://Program Files//Java//jdk-26.0.1")

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(rJava)
library(RJDBC)

# ⏱️ Início da contagem de tempo
tempo_inicio <- Sys.time()

# ==============================
# FUNÇÃO DE LOG
# ==============================
log_msg <- function(level, msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- paste0("[", timestamp, "] [", level, "] ", msg)

  cat(log_line, "\n")  # imprime no console
  write(log_line, file = "execucao.log", append = TRUE)  # salva em arquivo
}

# # ==============================
# # CARREGAR QUERIES
# # ==============================
# setwd("C:/Users/3894/Projetos R/DIGEPE/R/eSocial")

# source("queries.R")

# ==============================
# CONFIG JDBC
# ==============================
# driver_class <- "com.intersystems.jdbc.IRISDriver"
# jar_path <- "C://Program Files//DBeaver//intersystems-jdbc-3.10.3.jar"
# drv <- JDBC(driverClass = driver_class, classPath = jar_path)
# url <- "jdbc:IRIS://172.17.3.41:51773/TJSE"

# ==============================
# CONECTAR
# ==============================
# log_msg("INFO", "Iniciando conexão com banco")
#
# con <- tryCatch({
#   dbConnect(drv,
#             url,
#             user = "kleberson.pinto",
#             password = "tjse@!2023")
# }, error = function(e) {
#   log_msg("ERROR", paste("Erro na conexão:", e$message))
#   stop("Falha ao conectar ao banco.")
# })
#
# log_msg("INFO", "Conexão estabelecida com sucesso")

# ==============================
# ESCOLHA DA QUERY
# ==============================
# id_query <- "Q004" #   QUERY SELECIONADA NO ARQUIVO
# log_msg("INFO", paste("Query solicitada:", id_query))

# ==============================
# FALLBACK DE QUERY
# ==============================
# if (!id_query %in% names(queries)) {
#   log_msg("WARNING", paste("Query", id_query, "não encontrada. Usando fallback."))
#
#   # fallback padrão
#   query <- "SELECT TOP 10 * FROM RHCADSERVIDOR"
#
# } else {
#   query <- queries[[id_query]]
# }
#
# log_msg("INFO", paste("Query executada:", query))

# ==============================
# EXECUTAR QUERY
# ==============================
# df <- tryCatch({
#   dbGetQuery(con, query)
# }, error = function(e) {
#   log_msg("ERROR", paste("Erro ao executar query:", e$message))
#   return(NULL)
# })

# ==============================
# RESULTADO
# ==============================
# if (!is.null(df)) {
#   log_msg("INFO", paste("Query executada com sucesso. Linhas retornadas:", nrow(df)))
# } else {
#   log_msg("ERROR", "Nenhum resultado retornado devido a erro.")
# }

# ==============================
# FINALIZAR
# ==============================
# dbDisconnect(con)
# log_msg("INFO", "Conexão encerrada")

# Definir o caminho da pasta contendo os CSVs
caminho_pasta <- "C://Users//3894//Projetos R//eSDECODER//relatorios"

# Listar arquivos CSV
arquivos <- list.files(
  path = caminho_pasta,
  pattern = "^relat_.*\\.csv$",
  full.names = TRUE
)

# Função de leitura e tratamento
ler_tratar_csv <- function(arquivo) {
  read_csv(arquivo, show_col_types = FALSE) %>%
    mutate(
      # Tratamento da Ocorrência
      `Ocorrência` = if_else(
        str_length(`Ocorrência`) > 100,
        str_c(str_sub(`Ocorrência`, 1, 100), " (...)"),
        `Ocorrência`
      ),

      # Separação do campo Detalhe
      Matricula = str_trim(str_extract(`Detalhe`, "^[^-]+")),
      Nome = str_trim(str_replace(`Detalhe`, "^[^-]+ -\\s*", ""))
    ) %>%
    # Selecionar já na nova estrutura (Detalhe deixa de existir)
    select(`Período`, `Código Evento`, Matricula, Nome, `Ocorrência`)
}

# Consolidar dados
tibble_final <- arquivos %>%
  map_dfr(ler_tratar_csv) %>%
  arrange(`Código Evento`, `Ocorrência`) %>%  # importante ordenar antes
  group_by(`Código Evento`) %>%
  mutate(
    NumID = row_number()  # ✅ reset por grupo
  ) %>%
  ungroup() %>%
  select(NumID, `Período`, `Código Evento`, Matricula, Nome, `Ocorrência`)

# Obter o valor do período
periodo_valor <- tibble_final %>%
  distinct(`Período`) %>%
  pull() %>%
  .[1]

# Sanitizar nome do arquivo
# periodo_limpo <- str_replace_all(periodo_valor, "[^A-Za-z0-9]", "")
periodo_limpo <- str_replace(
  periodo_valor,
  "^(\\d{1,2})/(\\d{4})$",
  "\\2\\1"
)

# garantir zero à esquerda no mês
periodo_limpo <- ifelse(
  nchar(periodo_limpo) == 5,
  str_replace(periodo_limpo, "(\\d{4})(\\d)", "\\10\\2"),
  periodo_limpo
)

# Nome e caminho do arquivo
nome_arquivo_saida <- paste0("Periodo_", periodo_limpo, ".csv")
caminho_arquivo_saida <- file.path(caminho_pasta, nome_arquivo_saida)

# Gravar CSV
write_csv(tibble_final, file = caminho_arquivo_saida)

# ==============================
# CRUZAMENTO COM BASE DO BANCO
# ==============================
# log_msg("INFO", "Iniciando cruzamento entre CSV e base do banco")
#
# # Padronizar campos
# df <- df %>%
#   mutate(Matricula = as.character(Matricula))
#
# tibble_final <- tibble_final %>%
#   mutate(Matricula = as.character(Matricula))

# ------------------------------
# INNER JOIN (registros que casaram)
# ------------------------------
# tibble_intersec <- inner_join(
#   tibble_final,
#   df,
#   by = "Matricula",
#   suffix = c("_csv", "_bd")
# )

# ------------------------------
# RESULTADO FINAL
# ------------------------------
# tibble_resultado <- tibble_intersec %>%
#   transmute(
#     NumID = row_number(),
#     Periodo = `Período`,
#     Folha,
#     Matricula,
#     Nome = Nome_csv
#   )

# ------------------------------
# LOG DE REGISTROS QUE CASARAM
# ------------------------------
# qtd_casados <- nrow(tibble_resultado)
# log_msg("INFO", paste("Registros que casaram:", qtd_casados))

# ------------------------------
# TOTAL DE REGISTROS DE ENTRADA
# ------------------------------
# total_csv <- nrow(tibble_final)

# ------------------------------
# % DE COBERTURA
# ------------------------------
# perc_cobertura <- if (total_csv > 0) {
#   round((qtd_casados / total_csv) * 100, 2)
# } else {
#   0
# }
#
# log_msg(
#   "INFO",
#   paste0(
#     "Cobertura do cruzamento: ",
#     perc_cobertura, "% (",
#     qtd_casados, "/", total_csv, ")"
#   )
# )

# ------------------------------
# MATRÍCULAS NÃO ENCONTRADAS
# ------------------------------
# matriculas_nao_encontradas <- tibble_final %>%
#   anti_join(df, by = "Matricula") %>%
#   distinct(Matricula)
#
# qtd_nao_encontradas <- nrow(matriculas_nao_encontradas)
#
# log_msg(
#   "WARNING",
#   paste("Matrículas não encontradas no banco:", qtd_nao_encontradas)
# )

# ------------------------------
# SALVAR CSV PRINCIPAL
# ------------------------------
# arquivo_intersec <- file.path(
#   caminho_pasta,
#   paste0("resultado_intersec_", periodo_limpo, ".csv")
# )
#
# write_csv(tibble_resultado, arquivo_intersec)
#
# log_msg("INFO", paste("Arquivo gerado:", arquivo_intersec))

# ------------------------------
# SALVAR MATRÍCULAS NÃO ENCONTRADAS
# ------------------------------
# if (qtd_nao_encontradas > 0) {
#
#   arquivo_nao_encontradas <- file.path(
#     caminho_pasta,
#     paste0("matriculas_nao_encontradas_", periodo_limpo, ".csv")
#   )
#
#   write_csv(matriculas_nao_encontradas, arquivo_nao_encontradas)
#
#   log_msg(
#     "WARNING",
#     paste("Arquivo de não encontrados gerado:", arquivo_nao_encontradas)
#   )
# }

# ⏱️ Fim da contagem
tempo_fim <- Sys.time()

# Calcular duração
duracao <- tempo_fim - tempo_inicio

# Exibir tempo no console
cat("Tempo de execução:", duracao, "\n")

