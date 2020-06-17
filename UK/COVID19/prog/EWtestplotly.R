library(dplyr)
library(plotly)

fig2_data <- read.csv("https://app.box.com/shared/static/ln50t25nxt3rzh35n74jgs2koqa7uqu0.csv")

# simple plot on one subgroup
# we can work out exactly what plots we want later
test_data <- fig2_data %>%
  filter(sex == "Female" & age == 85)

# get first out of sample date
in_sample <- filter(test_data, out_of_sample == TRUE)
train_stop <- in_sample[["week"]][1]

# add model traces
# For now just plot mean and 95% CI and the q500 line
fig <- plot_ly(test_data, x= ~week, y = ~q500, type = "scatter", mode = "line", name = "q500") %>%
  add_trace(y = ~q025, type = "scatter", mode = "lines", hoverinfo = "skip", line = list(color = "rgba(0, 0, 0, 0.6)", width = 0.5)) %>%
  add_trace(y =~q975, type = "scatter", mode = "lines", hoverinfo = "skip", fill = "tonexty", fillcolor = "rgba(0, 0, 0, 0.6)", line = list(color = "rgba(0, 0, 0, 0.6)", width = 0.5))

# add data traces
fig <- fig %>%
  add_trace(y = ~non_covid_deaths, type = 'scatter', mode = "none", fill = "tozeroy", fillcolor = "rgba(3, 28, 27, 0.4)") %>%
  add_trace(y = ~deaths, mode = "none", fill = "tonexty", fillcolor = "rgba(162, 17, 17, 0.4)")

# add start of out of sample line
fig <- fig %>%
  add_segments(x = train_stop, xend = train_stop, y = 0, yend = max(test_data$deaths), hoverinfo = "skip", line = list(color = "rgba(0, 0, 0, 0.3)", width = 0.5, dash = 'dash'))

# labels
fig <- fig %>%
  layout(hovermode = "x unified",
         xaxis = list(
           title = "",
           ticks = "outside"
         ),
         yaxis = list(
           title = "deaths"
         ),
         showlegend = FALSE,
         updatemenus = list(
          list(
            y=0.7,
                  buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, TRUE, TRUE)),
               label = "All"),

          list(method = "restyle",
              args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE)),
              label = "None")))
         )
  )

fig

# htmlwidgets::saveWidget(fig, "covid85_test.html")
