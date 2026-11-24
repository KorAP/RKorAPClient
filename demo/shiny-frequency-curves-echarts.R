# Requires RKorAPClient >= 1.6.0 or a current development build, and echarts4r.

# The same application as in demo("shiny-frequency-curves"), drawn with ECharts
options(rkorap.frequency_curves = "echarts")
shiny::runApp(system.file("shiny-apps", "frequency_curves", package = "RKorAPClient"))
