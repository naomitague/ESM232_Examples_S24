#' Solar PV Output
#'
#' Computes electricity from a photovoltaic system given incoming solar radiation
#' @param area area of panel (m2)
#' @param eff solar panel efficiency (0-1) default 0.8
#' @param PR performance ratio (system related) (0-1) default is 0.75
#' @param solar  array with the following columns day month year Kdown_direct Kdown_diffuse (kJ/m2/day)
#' @param eunits energy output: J results in kJ/m2/year or W then assume KWh, default = J
#' @param ethresh threshold radiation (kJ/m2) below which efficiency fall to 0
#' @param g TRUE/FALSE  graph results default=TRUE
#' @param clr colour of grph default "blue"
#' @param etype "both" uses both direct and diffuse, "direct' direct only, "diffuse" diffuse only default="both"
#' @author Naomi
#' @return annual (power for each year), avg (average power) (see eunits for units)

solarpv = function(area, eff=0.8, PR=0.75, solar, clr="blue", eunits="J", etype="both",g=TRUE, ethresh=10000) {

  
  
  
  # find energy to use depending on use options
  if (etype=="diffuse") {
      solar$total = solar$Kdown_diffuse
  }
  else {
      if (etype=="direct") {
        solar$total=solar$Kdown_direct
      }
    else {
        solar$total = solar$Kdown_direct+solar$Kdown_diffuse
    }
  }

  # apply efficiency scaled to 0 when below threshold
  adjusteff = function(x, ethresh,eff) 
  { result = ifelse((x > ethresh), eff*x, x*eff*(max(0, x/ethresh)))
    return(result)
  }
 
  solar  = solar %>%  mutate(Kadj = adjusteff(total, ethresh=ethresh, eff=eff))
                    
  
  # total annual radiation
  annualsolar = solar %>% group_by(year) %>% dplyr::summarize(Kadj=sum(Kadj))
  
  
  # compute electricity
  annualsolar$elect = area*PR*annualsolar$Kadj

  ylbs="kJ/yr"
  # unit conversion if needed
  if (eunits=="W") {
    annualsolar$elect = annualsolar$elect * 0.278
    ylbs = "Wh/yr"
  }

  # plot if users requested
  if (g)
      barplot(annualsolar$elect, names=annualsolar$year, col=clr, ylab=ylbs, xlab="Year")

  return(list(annual=annualsolar[,c("year","elect")], mean=mean(annualsolar$elect)))

}
