# la gracia del asunto esta en eliminar a los mediocres. O como nos gusta a los
# biologos, individuos particularmente altos no aseguran descendencia alta.
# ya sea en personas o maiz. En otras palabras, regression to the mean!

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
    global_avg = 0,
    initial_n_jugadores = n_jugadores
  )
}

update_scoreboard <- function(scoreboard) {
  # en esta funcion sucede "el tiro".
  
  # el largo del vector de tiros tiene que ser igual al de numero de jugadores
  # multiplicar por el qc me asegura que solo se actualice el score de los jugadores vivos
  scoreboard$score <- scoreboard$score + (runif(scoreboard$initial_n_jugadores) < scoreboard$probs * scoreboard$qc)
  scoreboard$fase <- scoreboard$fase + 1
  
  # el numero total de tiros es la suma de los jugadores que quedan parados en 
  # la fase (ya que en cada fase tiran un solo tiro)
  scoreboard$tiros <- scoreboard$tiros + sum(scoreboard$qc)
  
  scoreboard$global_avg <- sum(scoreboard$score) / scoreboard$tiros
  scoreboard$player_avg <- scoreboard$score / scoreboard$fase
  
  scoreboard
}

initial_tiritos <- function(scoreboard, n_tiros = 50) {
  # antes de los 50 tiros no elimino a nadie
  while (scoreboard$fase < n_tiros) {
    scoreboard <- update_scoreboard(scoreboard)
  }
  scoreboard
} 


# primero defini "una region de mediocridad" pero esto no funciono tan bien, usando
# como filtro este intervalo. Hice un poco de optimizacion sobre los limites
# y me sorprendi encontrando que 0 era lo mejor.
#
# scoreboard$player_avg < scoreboard$global_avg + 0.05) |
# (scoreboard$player_avg < scoreboard$global_avg - 0.1))

filter_players <- function(scoreboard) {
  # actualiza variable de quality control del scoreboard
  # el criterio usado es eliminar a los mediocres (cuando un tipo malo pero con suerte
  # tira mas tiros, no le va a ir tan bien)
  
  scoreboard$qc <- scoreboard$qc & (scoreboard$player_avg != scoreboard$global_avg)
  scoreboard
}


tirito <- function(scoreboard) {
  # actualiza scoreboard y filtra jugadores
  scoreboard <- update_scoreboard(scoreboard)
  scoreboard <- filter_players(scoreboard)
  
  scoreboard
}


cazatalentos_rtm <- function(scoreboard, n_tiros = 50) {

  # importante la guarda con numero de tiros
  while (sum(scoreboard$qc) > 1 & scoreboard$tiros < 14000) {
    scoreboard <- initial_tiritos(scoreboard, n_tiros)
    scoreboard <- tirito(scoreboard)
  }

  # en caso de mas de un ganador, me quedo con el de mayor puntaje total
  if (sum(scoreboard$qc > 1)) {
    scoreboard$qc <- scoreboard$score == max(scoreboard$score)
  }
  # filtro el vector de jugadores (probabilidades) usando el vector logico qc
  scoreboard$winner <- scoreboard$prob[scoreboard$qc]
  
  # devuelvo el scoreboard entero. NO preserva la historia!
  # para graficar trazas hay que modificar el codigo
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


set.seed(473632)
simulation(jugadores, 10000)
#> "proporcion de jordans elegidos: 99.86%"

# ~18 sec las 10000 iteraciones

# avisame cuando termina de correr, mario
# beepr::beep("coin")

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

mejor <- 0.7
peloton <- (600:650) / 1000
jugadores2 <- c(mejor, peloton)

simulation(jugadores2, 10000)
