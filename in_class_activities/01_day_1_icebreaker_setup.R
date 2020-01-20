require(data.table)
require(ggplot2)



n_tables = 10
mean_plot_size = 4
pine_density = c(10, 33)
oak_density  = c(55, 12)

data = data.table()

data[, plot_area := rpois(n_tables, mean_plot_size)]
data[, pine_counts := rpois(n_tables, pine_density)]
data[, oak_counts := rpois(n_tables, oak_density)]
data[, pine_density := pine_counts / plot_area]                      
data[, oak_density := oak_counts / plot_area]                      


data[, hist(pine_counts)]
data[, hist(pine_density)]
data[, dotchart(pine_counts, xlab = "Number of pines", ylab = "Sample Plot ID", labels = 1:nrow(data))]
data[, dotchart(pine_density, xlab = "Pines per hectare", ylab = "Sample Plot ID", labels = 1:nrow(data))]




