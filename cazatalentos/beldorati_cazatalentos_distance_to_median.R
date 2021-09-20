

# inicializamos parametros de los jugadores
mejor <- 0.7
peloton <- (501:599) / 1000
jugadores <- c(mejor, peloton)


# genero objeto "scoreboard" con todo lo que necesito
# uso la posicion en el vector probs como ID de los jugadores, de manera implicita.
# nunca se reordena, por lo que el mejor siempre es probs[1].
#
# qc: quality control. booleano que dice quien queda en la fase = fase
# fase: numero de fase (1 tiro por jugador vivo por fase)
# tiros: numero de tiros acumulados
# player_avg: promedio de cada jugador
# global_avg: promedio de todos los jugadores
init_scoreboard <- function(jugadores) {
  
  n_jugadores <- length(jugadores)
  
  list(
    probs = jugadores,
    score = rep(0, n_jugadores),
    qc = rep(T, n_jugadores),
    fase = 0,
    tiros = 0,
    player_avg = rep(0, n_jugadores),
    #player_min = rep(0, n_jugadores),
    global_avg = 0,
    global_median = 0,
    #player_dist_to_median = rep(0, n_jugadores),
    initial_n_jugadores = n_jugadores,
    history = list(shot_hist = c(), qc_hist = c(), median = c())
  )
}

update_scoreboard <- function(scoreboard, history = F) {
  
  scoreboard$current_shot <- runif(scoreboard$initial_n_jugadores) < scoreboard$probs
  scoreboard$score <- scoreboard$score + (scoreboard$current_shot * scoreboard$qc)
  scoreboard$fase <- scoreboard$fase + 1
  scoreboard$tiros <- scoreboard$tiros + sum(scoreboard$qc)
  
  scoreboard$player_avg <- scoreboard$score / scoreboard$fase
  #scoreboard$player_min <- min(scoreboard$score[scoreboard$qc])
  
  # quiero la media/mediana condicionales a los jugadores que quedan vivos!
  scoreboard$global_avg <- mean(scoreboard$score[scoreboard$qc]) 
  scoreboard$global_median <- median(scoreboard$score[scoreboard$qc])

  #scoreboard$player_dist_to_median <- c(scoreboard$score - scoreboard$global_median)[scoreboard$qc]
  
  if (history) {
    scoreboard$history$median <- c(scoreboard$history$median, scoreboard$global_median)
    scoreboard$history$shot_hist <- c(scoreboard$history$shot_hist, scoreboard$current_shot)
  }
  
  scoreboard
}

initial_tiritos <- function(scoreboard, n_tiros, history = F) {
  # antes de los 50 tiros no elimino a nadie
  while (scoreboard$fase < n_tiros) {
    scoreboard <- update_scoreboard(scoreboard, history)
    if (history) {
      scoreboard$history$qc_hist <- c(scoreboard$history$qc_hist, rep(T, scoreboard$initial_n_jugadores))
    }
  }
  scoreboard
} 


filter_players <- function(scoreboard, history = F) {
  # si tengo mas de tres competidores, los elimino por menor distancia a la mediana global

  if (sum(scoreboard$qc) > 2) {
    # creo que aca tengo un error de implementacion, pero me esta costando razonar
    scoreboard$qc <- scoreboard$qc & (scoreboard$score != scoreboard$score[which.min(abs(scoreboard$score - scoreboard$global_median - 1))])
    # scoreboard$qc <- scoreboard$qc & (scoreboard$score != scoreboard$score[which.min(scoreboard$player_dist_to_median)])
  }
  
  if (history) {
    scoreboard$history$qc_hist <- c(scoreboard$history$qc_hist, scoreboard$qc)
  }
  
  scoreboard
}


tirito <- function(scoreboard, history = F) {
  # actualiza scoreboard y filtra jugadores
  scoreboard <- update_scoreboard(scoreboard, history)
  scoreboard <- filter_players(scoreboard, history)
  
  scoreboard
}


cazatalentos_rtm <- function(scoreboard, n_tiros = 70, history = F) {

  # tiros sin filtro para saltear las fluctuaciones
  scoreboard <- initial_tiritos(scoreboard, n_tiros, history)
  
  # importante la guarda con numero de tiros
  while (scoreboard$tiros < 13995) {
    scoreboard <- tirito(scoreboard, history)
  }
  # en caso de mas de un ganador, me quedo con el de mayor puntaje total
  if (sum(scoreboard$qc) > 1) {
    scoreboard$qc <- scoreboard$score == max(scoreboard$score)
  }
  # filtro el vector de jugadores (probabilidades) usando el vector logico qc
  scoreboard$winner <- scoreboard$prob[scoreboard$qc]
  
  if (history) {
    scoreboard$history$shot_hist <- matrix(scoreboard$history$shot_hist,
      ncol = scoreboard$initial_n_jugadores,
      byrow = T
    )

    scoreboard$history$qc_hist <- matrix(scoreboard$history$qc_hist,
      ncol = scoreboard$initial_n_jugadores,
      byrow = T
    )
  }

  scoreboard
}

################################################################################
# simulaciones
# pretty_out me permite usar la misma funcion para el intervalo de confianza
simulation <- function(jugadores, n, pretty_out = T) {
  best <- 0
  for (j in 1:n) {
    
    # inicializo lista antes de cada iteracion
    scoreboard <- init_scoreboard(jugadores)
    scoreboard <- cazatalentos_rtm(scoreboard)
    
    # como el primer elemento es nuestro jordan, cuento las veces que el algoritmo lo elige
    best <- best + scoreboard$qc[1]
  }
  
  prop_correct <- best / n
  
  if (pretty_out) {
    paste0("proporcion de jordans elegidos: ", prop_correct * 100, "%")
  } else {
    prop_correct
  }
}


# set.seed(473632)
simulation(jugadores, 1000)


# avisame cuando termina de correr, mario
# beepr::beep("coin")




# ################################################################################

# graficos de las trazas

library(dplyr)
library(ggplot2)
library(tidyr)

l <- cazatalentos_rtm(init_scoreboard(jugadores), history = T)


shot_h <- l$history$shot_hist
qc_h <- l$history$qc_hist
shot_h[!qc_h] <- NA

data.frame(shot_h) %>%
  tibble::rowid_to_column() %>%
  pivot_longer(-rowid, names_to = "player", values_to = "encesta") %>%
  mutate(clase = case_when(
    player == "X1" ~ "jordan",
    TRUE ~ "peloton"
  )) %>%
  group_by(player) %>%
  mutate(score = cumsum(encesta)) %>%
  ggplot(aes(rowid, score, group = player, color = clase)) +
  geom_line()


# ################################################################################
# # estimo intervalo de confianza de la performance del algoritmo
# # uso distribucion t, ya que desconozco el desvio poblacional
# 
# # ademas, uso foreach para paralelizar las simulaciones
# library(foreach)
# doParallel::registerDoParallel()
# 
# muestras <- 100
# 
# ic <- foreach(1:muestras, .combine = "c") %dopar% {
#   simulation(jugadores, 10000, pretty = F)
# }
# doParallel::stopImplicitCluster()
# 
# # calculo media, desvio y error (al alpha = 0.05)
# u <- mean(ic)
# s <- sd(ic)
# error <- qt(0.975, df = muestras - 1) * s / sqrt(muestras)
# 
# paste0("la proporcion de jordans elegidos por el algoritmo es de ", round(u, 3), " +/- ", round(error, digits = 5))
# #> "la proporcion de jordans elegidos por el algoritmo es de 0.999 +/- 8e-05"


# pruebas con menos jugadores

# mejor <- 0.7
# peloton <- (600:650) / 1000
# jugadores2 <- c(mejor, peloton)
# 
# simulation(jugadores2, 10000)
