
# simulation
seed(123)

N = 1000

UA = rnorm(N)

UY = rnorm(N)

A = rnorm(N)

L = rnorm(N)

Y = rnorm(N)

As = rnorm(N, L + A + UA)

Ys = rnorm(N, L + Y + UY)


summary ( lm(UA ~ UY + As + Ys) )


summary ( lm(UA ~ UY + A + Y) )


summary ( lm(UA ~ UY + As + Ys + L ))


