Homework 03: Use `dplyr`/`ggplot2` to manipulate and explore the `gapminder` data
================
**Shuxian Fan**
Sep 28th, 2018

Initial Package Setup
---------------------

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(kableExtra))
```

Exploration Tasks
-----------------

### Get the maximum and minimum of GDP per capita for all continents.

``` r
gapminder %>%
  group_by(continent) %>%
  summarize(max.gdp = max(gdpPercap), min.gdp = min(gdpPercap))%>%
  knitr::kable("html")%>%
  kable_styling(bootstrap_options = "striped", full_width = F)%>%
  row_spec(0, bold = T, color = "black", background = "#5F9EA0" )
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;font-weight: bold;color: black;background-color: #5F9EA0;">
continent
</th>
<th style="text-align:right;font-weight: bold;color: black;background-color: #5F9EA0;">
max.gdp
</th>
<th style="text-align:right;font-weight: bold;color: black;background-color: #5F9EA0;">
min.gdp
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Africa
</td>
<td style="text-align:right;">
21951.21
</td>
<td style="text-align:right;">
241.1659
</td>
</tr>
<tr>
<td style="text-align:left;">
Americas
</td>
<td style="text-align:right;">
42951.65
</td>
<td style="text-align:right;">
1201.6372
</td>
</tr>
<tr>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
113523.13
</td>
<td style="text-align:right;">
331.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
49357.19
</td>
<td style="text-align:right;">
973.5332
</td>
</tr>
<tr>
<td style="text-align:left;">
Oceania
</td>
<td style="text-align:right;">
34435.37
</td>
<td style="text-align:right;">
10039.5956
</td>
</tr>
</tbody>
</table>
Get the maximum and minimum of GDP per capita for all continents.

Look at the spread of GDP per capita within the continents.

Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population. Just try something other than the plain vanilla mean.

How is life expectancy changing over time on different continents?

Report the absolute and/or relative abundance of countries with low life expectancy over time by continent: Compute some measure of worldwide life expectancy – you decide – a mean or median or some other quantile or perhaps your current age. Then determine how many countries on each continent have a life expectancy less than this benchmark, for each year.

Find countries with interesting stories. Open-ended and, therefore, hard. Promising but unsuccessful attempts are encouraged. This will generate interesting questions to follow up on in class.

Layout stretch goal: get table and figure side-by-side.

Table stretch goal: there are some really nice fancy table helper packages.

Reference and Sources
---------------------

**1. Named Colors and Hex Equivalents** <https://css-tricks.com/snippets/css/named-colors-and-hex-equivalents/>
