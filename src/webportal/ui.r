library(plotly)
library(shinythemes)

fluidPage(theme = shinytheme("cosmo"),
          navbarPage("GigaPack Monitor",
                     tabPanel("Market Heat Map",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("more_plots", "Select Visualization",
                                              choices = c("Market HeatMap","")),
                                  selectInput("universe", "Select Universe",
                                              choices = c("XL liquidity","L liquidity","M liquidity","S liquidity","XS liquidity")),
                                  selectInput("gigafield", "Select Gigafield",
                                              choices = c('imbalance_vol.median','val_s.obstats.q80','val_s.obstats.sd','val_s.obstats.skew','val_s.obstats.sum','val_s.tradestats.acf1','val_s.tradestats.diff','val_s.tradestats.ibs','val_s.tradestats.kurtosis','val_s.tradestats.mean','val_s.tradestats.median','val_s.tradestats.q20','val_s.tradestats.q80','val_s.tradestats.sd','val_s.tradestats.skew','val_s.tradestats.sum','vol.acf1','vol.diff','vol.ibs','vol.kurtosis','vol.mean','vol.median','vol.q20','vol.q80','vol.sd','vol.skew','vol.sum','vol_b.obstats.acf1','vol_b.obstats.diff','vol_b.obstats.ibs','vol_b.obstats.kurtosis','vol_b.obstats.mean','vol_b.obstats.median','vol_b.obstats.q20','vol_b.obstats.q80','vol_b.obstats.sd','vol_b.obstats.skew','vol_b.obstats.sum','vol_b.tradestats.acf1','vol_b.tradestats.diff','vol_b.tradestats.ibs','vol_b.tradestats.kurtosis','vol_b.tradestats.mean','vol_b.tradestats.median','vol_b.tradestats.q20','vol_b.tradestats.q80','vol_b.tradestats.sd','vol_b.tradestats.skew','vol_b.tradestats.sum','vol_s.obstats.acf1','vol_s.obstats.diff','vol_s.obstats.ibs','vol_s.obstats.kurtosis','vol_s.obstats.mean','vol_s.obstats.median','vol_s.obstats.q20','vol_s.obstats.q80','vol_s.obstats.sd','vol_s.obstats.skew','vol_s.obstats.sum','vol_s.tradestats.acf1','vol_s.tradestats.diff','vol_s.tradestats.ibs','vol_s.tradestats.kurtosis','vol_s.tradestats.mean','vol_s.tradestats.median','vol_s.tradestats.q20','vol_s.tradestats.q80','vol_s.tradestats.sd','vol_s.tradestats.skew','vol_s.tradestats.sum','volume','vwap_b.acf1','vwap_b.diff','vwap_b.ibs','vwap_b.kurtosis','vwap_b.mean','vwap_b.median','vwap_b.q20','vwap_b.q80','vwap_b.sd','vwap_b.skew','vwap_b.sum','vwap_b_1mio.acf1','vwap_b_1mio.diff','vwap_b_1mio.ibs','vwap_b_1mio.kurtosis','vwap_b_1mio.mean','vwap_b_1mio.median','vwap_b_1mio.q20','vwap_b_1mio.q80','vwap_b_1mio.sd','vwap_b_1mio.skew','vwap_b_1mio.sum','vwap_s.acf1','vwap_s.diff','vwap_s.ibs','vwap_s.kurtosis','vwap_s.mean')), width=2),
                                mainPanel(uiOutput("myplot"), height="500%", width=8)
                              ),
                              
                     )
          ))
