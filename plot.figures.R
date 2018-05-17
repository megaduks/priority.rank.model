library(ggplot2)

df1 <- read.csv(file = 'synthetic.network.results.pvalues.csv')

p1 <- ggplot(data = df1, aes(x=variable, y=value, fill=model)) + geom_bar(stat = 'identity', position = position_dodge()) + geom_hline(yintercept = 0.05, color='red', linetype='dashed')
p1 <- p1 + xlab('centrality measure p-value')
p1
ggsave(filename = 'synthetic-networks-pvalues.png', plot = p1)

df2 <- read.csv(file = 'synthetic.network.results.csv')

p2 <- ggplot(data = df2, aes(x=variable, y=error, fill=model)) + geom_bar(stat = 'identity', position = position_dodge()) 
p2 <- p2 + xlab('absolute relative error')
p2
ggsave(filename = 'synthetic-networks-errors.png', plot = p2)
