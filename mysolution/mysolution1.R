# Zadanie 1 - grafy losowe

# Ustawienia wstępne (dla powtarzalności wyników)
set.seed(42)
library(igraph)

# 2. Generowanie sieci Erdos-Renyi o 100 wierzchołkach i p-o krawędzi 0.05
n <- 100
p <- 0.05
g <- erdos.renyi.game(n=n, p.or.m=p, directed=FALSE)

# 3. Podsumowanie grafu
summary(g)

# Czy graf jest ważony?
is_weighted_first <- is_weighted(g)
# Odpowiedź: 
cat("Is graph weighted (first one)?:", is_weighted_first, "\n\n")
# Wykorzystano tutaj funkcję is_weighted, która sprawdza czy graf jest
# ważony i otrzymano wartość FALSE

# 4. Wylistowanie wszystkich wierzchołków i krawędzi
cat("Vertices:")
print(V(g))
cat("Edges:")
print(E(g))

# 5. Ustawienie wag wszystkich krawędzi na losowe z zakresu [0.01; 1]
E(g)$weight <- runif(length(E(g)), min=0.01, max=1)

# 6. Podsumowanie grafu
summary(g)

# Czy graf jest ważony?
is_weighted_second <- is_weighted(g)
# Odpowiedź: 
cat("Is graph weighted (second one)?:", is_weighted_second, "\n\n")
# Ponownie ykorzystano tutaj funkcję is_weighted, która sprawdza czy graf jest
# ważony i otrzymano tym razem wartość TRUE, co jest wynikiem ustawienia wag
# w punkcie 5. Dodatkowo w podsumowaniu (summary) pojawił się nowy
# atrybut (weight)

# 7. Jaki jest stopień każdego węzła?
deg <- degree(g)
# Odpowiedź:
cat("Degrees:\n")
print(deg)
# stopień każdego węzła prezentuje wypisana lista stopni, gdzie każdy indeks
# w liście odpowiada danemu węzłowi, a liczba przypisana do tego indeksu
# opisuje stopień tego węzła

# Histogram stopni węzłów
hist(
  deg,
  breaks=seq(0, max(deg), by=1),
  main=paste("Histogram stopni wierzchołków (n=", n, ", p=", p, ")", sep=""),
  xlab="Stopień (degree)",
  ylab="Liczba węzłów"
)

# 8. Klastry w grafie (connected components)
comp <- components(g)

# Ile jest klastrów w grafie?
num_comp <- comp$no
# Odpowiedź: 
cat("Number of connected components:", num_comp, "\n")
# w stworzonym grafie powstał tylko 1 klastr

# 9. wizualizacja grafu
# PageRank
pr <- page.rank(g)$vector

plot(g, vertex.size=pr*300, vertex.label=NA, edge.arrow.size=.2,
     main="Wizualizacja grafu z rozmiarami węzłów wg PageRank")
