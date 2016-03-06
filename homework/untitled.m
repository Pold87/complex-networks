A = load('7.txt')
B = graph(A(:, 1), A(:, 2));

ds = 1:100;
pks = expo(ds, 0.2);
plot(ds, pks)
histogram(degree(B))