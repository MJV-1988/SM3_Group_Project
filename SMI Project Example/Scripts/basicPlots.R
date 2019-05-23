# Define some grpahing functions

# Univariate
continuousPlot <- function(data, variable, variableLabel)
{
  ggplot(data, aes(variable)) +
    geom_histogram(fill = "#599ad3") + labs(x = variableLabel, y = "Frequency") +
    theme_minimal()
}

discretePlot <- function(data, variable, variableLabel)
{
  ggplot(data, aes(variable)) +
    geom_bar(fill = "#599ad3") + labs(x = variableLabel, y = "Frequency") +
    theme_minimal()
}

# Bivariate
boxPlot <- function(data, variableX, variableY, variableXLabel, variableYLabel)
{
    ggplot(data, aes(x = fct_reorder(variableX, variableY), y = variableY, fill = variableX)) +
      scale_y_continuous(limits = range(variableY)+c(-1,1)) +
      geom_jitter(stat = "identity", shape = 21, colour ="#000000", fill = "#32CD32") +
      stat_boxplot(geom = "errorbar") +
      geom_boxplot(alpha = 0.5, outlier.size = -10) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 270, hjust = 0)) +
      labs(x = "", y = variableYLabel) + 
      theme(legend.position = "none")
}

scatterPlot <- function(data, variableX, variableY, variableXLabel, variableYLabel, unitsX = "", unitsY = "")
{
  ggplot(data, aes(x = variableX, y =  variableY)) + 
    geom_point(stat = "identity", shape = 21, fill = "#599ad3") + 
    theme_minimal() +
    labs(x = paste(variableXLabel, unitsX, sep =" "), y = paste(variableYLabel, unitsY, sep = " "))
}