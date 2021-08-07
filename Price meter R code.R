# calculation ----
library(pricesensitivitymeter) 
library(readxl) # To read excell files
library(ggplot2) # To create plots

my_data <- data.frame(read_excel("/Users/komori/Downloads/MG403 Individual Take-Home Assignment Documents 2021-20210504/IQOS Pricing Meter Data 2020-21.xlsx", 
                                 sheet = "Cleaned"))

# Select the respective columns
output <- psm_analysis(toocheap = my_data[,12], 
                       cheap =  my_data[,14], 
                       expensive =  my_data[,13],
                       tooexpensive =  my_data[,11], 
                       interpolate = TRUE
)
summary(output)

# plotting  ----

library(ggplot2)

# all plot elements without any labels 
psmplot <-  ggplot(data = output$data_vanwestendorp, aes(x = price)) +
  annotate(geom = "rect", # shaded background area for range of acceptable prices
           xmin = output$pricerange_lower,
           xmax = output$pricerange_upper,
           ymin = 0, ymax = Inf,
           fill="grey50", alpha = 0.3) +
  geom_line(aes(y = ecdf_toocheap, # line: too cheap
                colour = "too cheap",
                linetype = "too cheap"),
            size= 1) +
  geom_line(aes(y = ecdf_tooexpensive, # line: too expensive
                colour = "too expensive",
                linetype = "too expensive"),
            size = 1) + 
  geom_line(aes(y = ecdf_not_cheap, # line: not cheap
                colour = "not cheap",
                linetype = "not cheap"),
            size = 1) +
  geom_line(aes(y = ecdf_not_expensive, # line: not expensive
                colour = "not expensive",
                linetype = "not expensive"),
            size = 1) + 
  annotate(geom = "point", # Indifference Price Point (intersection of "cheap" and "expensive")
           x = output$idp, 
           y = output$data_vanwestendorp$ecdf_not_cheap[output$data_vanwestendorp$price == output$idp],
           size = 5,
           shape = 18,
           colour = "#009E73") + 
  annotate(geom = "point", # Optimal Price Point (intersection of "too cheap" and "too expensive")
           x = output$opp, 
           y = output$data_vanwestendorp$ecdf_toocheap[output$data_vanwestendorp$price == output$opp],
           size = 3,
           shape = 17,
           colour = "#009E73")


# Labels and Colours
psmplot +
  labs(x = "Price",
       y = "Share of Respondents (0-1)",
       title = "IQOS Price Sensitivity Meter Plot",
       caption = "Shaded area: range of acceptable prices\nData: IQOS Pricing Meter Data 2020-21")  + 
  scale_colour_manual(name = "Legend",
                      values = c("too cheap" = "pink",
                                 "not cheap" = "#009E73",
                                 "not expensive" = "#D55E00",
                                 "too expensive" = "brown")) + 
  scale_linetype_manual(name="Legend",
                        values = c("too cheap" = "dotted",
                                   "not cheap" = "solid",
                                   "not expensive" = "solid",
                                   "too expensive" = "dotted")) + 
  annotate(geom = "text", # Label of Indifference Price Point
           x = output$idp + 1.5, 
           y = output$data_vanwestendorp$ecdf_not_cheap[output$data_vanwestendorp$price == output$idp],
           label = paste("IDP: ", output$idp)) + 
  annotate(geom = "text", # Label of Optimal Price Point
           x = output$opp + 1.5,
           y = output$data_vanwestendorp$ecdf_toocheap[output$data_vanwestendorp$price == output$opp],
           label = paste("OPP: ", output$opp)) +
  theme_minimal()
