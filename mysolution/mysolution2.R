# Zadanie 2 - grafy preferential attachment (Barabasi-Albert)

# 2. Wygenerowanie gafu wedle modelu Barabasi-Albert z tysiącem węzłów
n <- 1000
g <- barabasi.game(n)

# 3. wizualizacja grafu layoutem Fruchterman & Reingold
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2, vertex.label=NA, edge.arrow.size=.2,
     main=paste("Barabasi-Albert (n=", n, ")", sep=""))

# 4. znalezienie najbardziej centralnego węzła według miary betweenness
btw <- betweenness(g)
max_btw_val <- max(btw)
max_btw_idx <- which.max(btw)
cat("Maksymalna wartość betweenness:", max_btw_val, "\n")

# Jaki ma numer?
# Odpowiedź:
cat("Węzeł o największej wartości (indeks):", max_btw_idx, "\n")
print(V(g)[max_btw_idx])
# Najbardziej centralny węzeł ma numer 13

# 5. Jaka jest średnica grafu?
# najpierw sprawdzono czy graf jest spójny
is_conn <- is.connected(g)
is_conn
# graf jest spójny, więc policzono średnicę grafu
g_diameter <- diameter(g)

# Odpowiedź:
g_diameter
# Graf ma średnicę o wartości 11 - najdłuższa najkrótsza ścieżka pomiędzy
# dwoma dowolnymi wierzchołkami

# 6. Rożnice między grafami Barabasi-Albert i Erdos-Renyi:
# * mechanizm tworzenia krawędzi:
#   a) graf Barabasi-ALbert:
#     - sieć rośnie stopniowo, dodaje się nowe węzły jeden po drugim
#     - każdy nowy węzeł preferencyjnie łączy się z tymi, ktorzy już mają
#     dużo połączeń
#   (analogia: im więcej ma się znajomych, tym bardziej nowi ludzie mogą 
#   chcieć się z tobą zaprzyjaźnić)
#   b) Erdos-Renyi:
#     - krawędzie dodawane są losowo i każda para węzłow ma takie samo
#     prawdopodobieństwo połączenia
# * wygląd sieci:
#   a) Barabasi-Albert:
#     - większość węzłów ma mało połączeń, natomiast tylko kilka staje się
#     bardzo silnie połączonymi hubami
#   b) Erdos Renyi:
#     - liczba połączeń jest czysto losowa, nie ma żadnych hubów jak w
#     przypadku grafu BA
# * rozkład stopni
#   a) Barabasi-Albert:
#     - kilka węzłów ma bardoz wysoki stopień, natomiast większość raczej
#     minimalny
#   b) Erdos Renyi:
#     - na podstawie przeprowadzonego zadania 1 można zauważyć, że rozkład
#     stopni zbliżony jest do rozkładu normalnego

