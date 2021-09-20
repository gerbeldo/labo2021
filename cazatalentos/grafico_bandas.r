#Visualizar una realidad muchas veces ayuda al cerebro humano a encontrar soluciones
#color     ROJO  el jugador 0.7
#Color     naranja  TRES desvios estandar de la binomial de p=0.7
#color  AMARILLO el jugador 0.59  ( el mejor del peloton )


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

require("ggplot2")
require("gganimate")
require("gifski")
require("transformr")


rondas  <- 500

#Con la semilla  200177  ---------------------------------------------
  set.seed(200177)
  tb_journey  <- data.table(  prob=numeric(), tiempo=integer(), tiros=integer(), aciertos=integer())

  #creo un dataset con los tiros libres de todos los jugadores
  for( p in  c( 700, 501:599 ) )
  {
    tbl  <- list( rep( paste0("P",p), rondas), 1:rondas, 1:rondas,  cumsum( runif(rondas) < p/1000 ) )
    
    banda_inf  <- list( rep("banda_inf", rondas), 1:rondas, 1:rondas,  trunc( (1:rondas)*0.7 - 3*sqrt( 0.7*(1-0.7)*(1:rondas)))  )  
    banda_sup  <- list( rep("banda_sup", rondas), 1:rondas, 1:rondas,  trunc( (1:rondas)*0.7 + 3*sqrt( 0.7*(1-0.7)*(1:rondas)))  )  
    tb_journey  <- rbind( tb_journey, tbl, banda_inf, banda_sup )
  }


  g1  <- ggplot(tb_journey[ prob!="P700" & prob!="P599"], aes( tiros, aciertos, group=prob ) ) +
                geom_line( colour= "black") +
                geom_point(size=3) +
                geom_line( data=tb_journey[prob=="P599"], aes( tiros, aciertos, group=prob ), color="yellow", size=1.1 ) +
                geom_line( data=tb_journey[prob=="P700"], aes( tiros, aciertos, group=prob ), color="red", size=1.2 ) +
                geom_line( data=tb_journey[prob=="banda_inf"], aes( tiros, aciertos, group=prob ), color="orange", size=1.1 ) +
                geom_line( data=tb_journey[prob=="banda_sup"], aes( tiros, aciertos, group=prob ), color="orange", size=1.1 ) +
                theme( panel.grid.minor = element_blank()) +
                transition_reveal(tiempo) +
                ease_aes('linear')

  animate( g1, width=1000, height=700, fps=2, renderer = gifski_renderer() )
  anim_save( "M:\\work\\cazatalentos_bandas_01.gif"  )



#Con la perversa semilla  706787  ---------------------------------------------
  set.seed(706787)
  tb_journey  <- data.table(  prob=numeric(), tiempo=integer(), tiros=integer(), aciertos=integer())
  #creo un dataset con los tiros libres de todos los jugadores
  for( p in  c( 700, 501:599 ) )
  {
    tbl  <- list( rep( paste0("P",p), rondas), 1:rondas, 1:rondas,  cumsum( runif(rondas) < p/1000 ) )
    
    banda_inf  <- list( rep("banda_inf", rondas), 1:rondas, 1:rondas,  trunc( (1:rondas)*0.7 - 3*sqrt( 0.7*(1-0.7)*(1:rondas)))  )  
    banda_sup  <- list( rep("banda_sup", rondas), 1:rondas, 1:rondas,  trunc( (1:rondas)*0.7 + 3*sqrt( 0.7*(1-0.7)*(1:rondas)))  )  
    tb_journey  <- rbind( tb_journey, tbl, banda_inf, banda_sup )
  }


  g2  <- ggplot(tb_journey[ prob!="P700" & prob!="P599"], aes( tiros, aciertos, group=prob ) ) +
                geom_line( colour= "black") +
                geom_point(size=3) +
                geom_line( data=tb_journey[prob=="P599"], aes( tiros, aciertos, group=prob ), color="yellow", size=1.1 ) +
                geom_line( data=tb_journey[prob=="P700"], aes( tiros, aciertos, group=prob ), color="red", size=1.2 ) +
                geom_line( data=tb_journey[prob=="banda_inf"], aes( tiros, aciertos, group=prob ), color="orange", size=1.1 ) +
                geom_line( data=tb_journey[prob=="banda_sup"], aes( tiros, aciertos, group=prob ), color="orange", size=1.1 ) +
                theme( panel.grid.minor = element_blank()) +
                transition_reveal(tiempo) +
                ease_aes('linear')

  animate( g2, width=1000, height=700, fps=2, renderer = gifski_renderer() )
  anim_save( "M:\\work\\cazatalentos_bandas_02.gif"  )

