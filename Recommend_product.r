# Recommend products based on past purchases
#
# Recommender based on collaborative filtering
#
# (c) 2015, Doesburg

library(plyr)

# Data preparation
eae = read.csv("test.csv", sep=";")
eae$Orderdatum <- as.Date(eae$Orderdatum, format = "%d-%m-%Y")
eae$Nettoprijs <- as.numeric(sub(",",".", eae$Nettoprijs))

# Select unique customers who bought product X
peer_customers <- function(product)
{
    unique(eae[grep(product, eae$Artikel), c('Debiteur', 'Bedrijfsnaam')])
}

#select TOP N most popular product(s) in a group of customers
top_product <- function(peer_group, N)
{
    bought_items <- subset( eae, Debiteur =  peer_group, select=c(Artikel, Omschrijving, Debiteur, Bedrijfsnaam))
    popularity <- ddply(bought_items, .(bought_items$Artikel), nrow)
    arrange(popularity, desc(V1))
    popularity[1:N,1]
}

#recommend N products based on a selected product
recommend <- function(selected_product, N)
{
    top_product(peer_customers(selected_product), N)
}
