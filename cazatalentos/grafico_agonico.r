#Visualizar una realidad muchas veces ayuda al cerebro humano a encontrar soluciones
#color     ROJO  el jugador 0.7
#color  AMARILLO el jugador 0.59  ( el mejor del peloton )


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

require("ggplot2")
require("gganimate")
require("gifski")
require("transformr")



tb_journey  <- data.table(  prob=numeric(), tiempo=integer(), tiros=integer(), aciertos=integer())

rondas  <- 500
set.seed(706787)  #semilla elegida con perversidad


  #creo un dataset con los tiros libres de todos los jugadores
  for( p in  c( 700, 501:599 ) )
  {
    tbl  <- list( rep( paste0("P",p), rondas), 1:rondas, 1:rondas,  cumsum( runif(rondas) < p/1000 ) )
    tb_journey  <- rbind( tb_journey, tbl )
  }



  g  <- ggplot(tb_journey[ prob!="P700" & prob!="P599"], aes( tiros, aciertos, group=prob ) ) +
               geom_line( colour= "black") +
               geom_point(size=3) +
               geom_line( data=tb_journey[prob=="P599"], aes( tiros, aciertos, group=prob ), color="yellow", size=1.1 ) +
               geom_line( data=tb_journey[prob=="P700"], aes( tiros, aciertos, group=prob ), color="red", size=1.2 ) +
               theme( panel.grid.minor = element_blank()) +
               transition_reveal(tiempo) +
               ease_aes('linear')


  animate( g, width=1000, height=700, fps=2, renderer = gifski_renderer() )

  anim_save( "M:\\work\\cazatalentos_evo_agonica.gif"  )
