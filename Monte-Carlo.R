r <- 0.8;
no <- 1000;

random_x <- runif(no, min = -1, max = 1);
random_z <- runif(no, min = -1, max = 1);
plot(random_x,random_z)

n <- 1;
i <- 1;
ij <- 1;

for(n in 1:length(random_x)){
y <- sqrt((random_x[n]*random_x[n]) + (random_z[n]*random_z[n]));
if(y < r){
points(random_x[n],random_z[n],col="green");
ij = ij + 1;
}
else{
points(random_x[n],random_z[n],col="red");
}
i <- i+1

}

outin <- (ij/length(random_x)) * 4;

print(outin)
Fläche <- r*r*3.14
print(Fläche)