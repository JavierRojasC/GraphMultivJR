
ACMgraph <- function(data){
  require(ca)
  require(readr)
  require(stringr)
  require(dplyr)
  require(highcharter)
  AC <- mjca(data)
  AC.SUM <- summary(AC)
  AC.SUM1 <- AC.SUM$columns
  Coord <- data.frame(AC.SUM1[,5],AC.SUM1[,8])/1000

  Nombres <- data.frame(AC.SUM1$name)
  xs <- str_split(Nombres$AC.SUM1.name, ":")
  length(xs)
  XSDF <- as.data.frame(xs[1:55])
  XSDF_t <- as.data.frame(t(XSDF))
  Nombres <- XSDF_t$V1
  Nombrecorto <- XSDF_t$V2
  Coord <- data.frame(Coord,Nombrecorto,Nombres)



  highchart()%>%
    hc_title(text="")%>%
    hc_subtitle(text="Análisis de correspondencia múltiple")%>%
    hc_subtitle(text="")%>%
    hc_add_series(Coord, type='scatter', hcaes(x=AC.SUM1...5., y=AC.SUM1...8., name=Nombrecorto, group=Nombres),
                  dataLabels=list(format="{point.name}",enabled=TRUE),
                  tooltip = list(pointFormat = "{point.name}"))%>%
    hc_xAxis(
      title = list(text = "Dim 1"),
      plotLines = list(list(
        value = 0,
        color = '#1D4B5E',
        width = 3,
        zIndex = 4,
        label = list(text = "",
                     style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
    hc_yAxis(
      title = list(text = "Dim 2"),
      plotLines = list(list(
        value = 0,
        color = '#1D4B5E',
        width = 3,
        zIndex = 4,
        label = list(text = "",
                     style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
    hc_exporting(enabled = TRUE,
                 filename = "")%>%
    hc_credits(
      enabled = TRUE,
      text = "",
      href = ""
    )%>%
    hc_subtitle(text="Análisis de correspondencia múltiple")

}
