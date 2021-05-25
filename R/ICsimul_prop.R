ICsimul_prop <- function(Nsim, n, p,conflevel=0.95){
  require(dplyr)
  require(highcharter)
  int <- matrix(0, Nsim,3)
  color2 <- c()
  color3 <- c()

    for (i in 1:Nsim){
      Val <- rnorm(1,n*p, sqrt(n*p*(1-p)))

    PT <- prop.test(Val, n, p,
              alternative = c("two.sided"),
              conf.level = conflevel)
    int[i,1] <- PT$conf.int[1]
    int[i,2] <- PT$conf.int[2]
    int[i,3] <- PT$estimate
    if (PT$conf.int[2] < p | PT$conf.int[1]  > p){
      color2[i] <- '#BD4423'
    } else {
      color2[i] <- '#23BD38'
    }

    if (PT$conf.int[2] < p | PT$conf.int[1]  > p){
      color3[i] <- '#822828'
    } else {
      color3[i] <- '#245679'
    }

    }
  Int <- data.frame(int, color2, color3)
  hchart(pointWidth=0,type = 'columnrange',Int,name='Intervalo',
         hcaes(x=1:length(X2),high=X2, low=X1,color=color2))%>%
    hc_add_series(Int, type='scatter', hcaes(x=1:length(X2), y=X3,color=color3), name='p estimado', color='#289B9C',
                  tooltip = list(pointFormat = "<br>{point.y}"))%>%
    hc_xAxis(title=list(text=('Simulaciones')))%>%
    hc_yAxis(title=list(text=('Intervalos de confianza')),
             plotLines = list(list(
               value = p,
               color = '#DAE0EA',
               width = 3,
               zIndex = 4,
               label = list(text = "",
                            style = list( color = '#DAE0EA', fontWeight = 'bold' )))))

}

