library(plotly)
#Path to folder
filepath <- "~/language-research"

setwd(paste(filepath, "/intermediate", sep = ""))
df <- read.csv("industry.df.bigdata.csv")

for(i in 1:50){
  str.hover = ""
  for(j in 3:44){
    if(df[i,j] > 0){
      str.hover <- paste(str.hover, paste(colnames(df)[j], df[i,j], sep = " : "), "<br>")
    }
  }
  df$hover[i] <- str.hover
}

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_ly(df, z = total, text = hover, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = total, colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "Total Job Postings")) %>%
  layout(title = '2016 US Job Postings by State<br>(Hover for breakdown)', geo = g)