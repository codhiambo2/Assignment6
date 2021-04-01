# Assignment6

title: "COVID-19 Dashboard- France, Italy and Spain"
author: "Collins Odhiambo"
output:
flexdashboard::flex_dashboard:
orientation: columns
vertical_layout: fill
#------------------ Packages ------------------
library(flexdashboard)
library(coronavirus)
data(coronavirus)
`%>%` = magrittr::`%>%`
confirmed_color = "brown"
active_color = "#1f77b4"
recovered_color= "forestgreen"
death_color= "red"
#------------------ Data ------------------
df= coronavirus %>%
   dplyr::filter(country == "France"|
    country == "Italy" |
    country == "Spain") %>%
  dplyr::group_by(country, type) %>%
  dplyr::summarise(total = sum(cases)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%

  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(-confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(country == "United Arab Emirates", "UAE", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))

df_daily = coronavirus %>%
  dplyr::filter(country == "France"|
    country == "Italy" |
    country == "Spain") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active)
  )

df1= coronavirus %>% dplyr::filter(date == max(date))
Summary
Row {data-width=400}
confirmed {.value-box}

valueBox(
  value = paste(format(sum(df$confirmed), big.mark = ","), "", sep = " "),
  caption = "Total confirmed cases",
  icon = "fas fa-user-md",
  color = confirmed_color
)
death {.value-box}

valueBox(
  value = paste(format(sum(df$death, na.rm = TRUE), big.mark = ","), " (",
    round(100 * sum(df$death, na.rm = TRUE) / sum(df$confirmed), 1),
    "%)",
    sep = ""
  ),
  caption = "Death cases (death rate)",
  icon = "fas fa-heart-broken",
  color = death_color
)
Row
Daily cumulative cases by type
plotly::plot_ly(data = df_daily) %>%
  plotly::add_trace(
    x = ~date,
    y = ~confirmed_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "Confirmed",
    line = list(color = active_color),
    marker = list(color = active_color)
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~death_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "Death",
    line = list(color = death_color),
    marker = list(color = death_color)
  ) %>%
   plotly::add_annotations(
    x = as.Date("2020-03-11"),
    y = 3,
    text = paste("First death"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-18"),
    y = 14,
    text = paste(
      "Lockdown"
    ),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -90
  ) %>%
  plotly::layout(
    title = "",
    yaxis = list(title = "Cumulative number of cases"),
    xaxis = list(title = "Date"),
    legend = list(x = 0.1, y = 0.9),
    hovermode = "compare"
  )
Comparison
Column {data-width=400}
Daily new confirmed cases
daily_confirmed <- coronavirus %>%
  dplyr::filter(type == "confirmed") %>%
  dplyr::filter(date >= "2020-02-29") %>%
  dplyr::mutate(country = country) %>%
  dplyr::group_by(date, country) %>%
  dplyr::summarise(total = sum(cases)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = country, values_from = total)

#----------------------------------------
# Plotting the data

daily_confirmed %>%
plotly::plot_ly() %>%
 plotly::add_trace(
  x = ~date,
  y = ~France,
  type = "scatter",
   mode = "lines+markers",
  name = "France"
  ) %>%
  plotly::add_trace(
   x = ~date,
   y = ~Spain,
   type = "scatter",
   mode = "lines+markers",
   name = "Spain"
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~Italy,
    type = "scatter",
    mode = "lines+markers",
    name = "Italy"
  ) %>%
  plotly::layout(
    title = "",
    legend = list(x = 0.7, y = 0.9),
    yaxis = list(title = "New confirmed cases"),
    xaxis = list(title = "Date"),
    hovermode = "compare",
    margin = list(
      b = 10,
      t = 10,
      pad = 2
    )
  )
Cases by type
df_EU = coronavirus %>%
  # dplyr::filter(date == max(date)) %>%
  dplyr::filter(country == "France" |
    country == "Italy" |
    country == "Spain") %>%
  dplyr::group_by(country, type) %>%
  dplyr::summarise(total = sum(cases)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(country == "United Arab Emirates", "UAE", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))

plotly::plot_ly(
  data = df_EU,
  x = ~country,
  y = ~ confirmed,
  type = "bar",
  name = "Confirmed",
  marker = list(color = active_color)
) %>%
  plotly::add_trace(
    y = ~death,
    name = "Death",
    marker = list(color = death_color)
  ) %>%
  plotly::layout(
    barmode = "stack",
    yaxis = list(title = "Total cases"),
    xaxis = list(title = ""),
    hovermode = "compare",
    margin = list(
      # l = 60,
      # r = 40,
      b = 10,
      t = 10,
      pad = 2
    )
  )
Map
World map of cases
library(leaflet)
library(leafpop)
library(purrr)
cv_data_for_plot = coronavirus %>%
  dplyr::filter(cases > 0) %>%
  dplyr::group_by(country, province, lat, long, type) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::mutate(log_cases = 2 * log(cases)) %>%
  dplyr::ungroup()
cv_data_for_plot.split = cv_data_for_plot %>% split(cv_data_for_plot$type)
pal <- colorFactor(c("orange", "red", "green"), domain = c("confirmed", "death", "recovered"))
map_object <- leaflet() %>% addProviderTiles(providers$Stamen.Toner)
names(cv_data_for_plot.split) %>%
  purrr::walk(function(df) {
    map_object <<- map_object %>%
      addCircleMarkers(
        data = cv_data_for_plot.split[[df]],
        lng = ~long, lat = ~lat,
        color = ~ pal(type),
        stroke = FALSE,
        fillOpacity = 0.8,
        radius = ~log_cases,
        popup = leafpop::popupTable(cv_data_for_plot.split[[df]],
          feature.id = FALSE,
          row.numbers = FALSE,
          zcol = c("type", "cases", "country", "province")
        ),
        group = df,
          labelOptions = labelOptions(
          noHide = F,
          direction = "auto"
        )
      )
  })

map_object %>%
  addLayersControl(
    overlayGroups = names(cv_data_for_plot.split),
    options = layersControlOptions(collapsed = FALSE)
  )
Trend
COVID-19 Dashboard- France, Italy and Spain

# Importing raw data
confirmedraw = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deathsraw = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveredraw = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
library(tidyr)
library(dplyr)
library(ggplot2)
confirmed = confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths = deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered = recoveredraw %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
summary(confirmed)

#combining all three
country= full_join(confirmed, deaths) %>% full_join(recovered)

# Fix date variable and convert from character to date
country$date= country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
country= country %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)

# Aggregate at world level
world = country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)

countryselection= country %>% filter(Country.Region==c("France", "Italy", "Spain"))
ggplot(countryselection, aes(x=days, y=confirmed, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by Country", x= "Days", y= "Daily confirmed cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

ggplot(countryselection, aes(x=days, y=confirmed, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by Country", x= "Days", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous()
