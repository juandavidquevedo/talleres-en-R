rm(list=ls(all=TRUE))

#--------------------------------------------
######   TALLER   #######
#######################################33
#1.
sumaparcial<-function(k){
  suma = 0
  for(i in 1:k){
    #print(suma)
    suma = suma + 1/(i)^2
  }
  return(suma)
}
a<-sumaparcial(1000)
a

#2.
reiman<-function(k){
  base = 0
  punto = 1/k
  suma=0
  for(i in 1:k){
    base = base + 1/k
    #print(suma)
    altura = exp(-1*base)
    suma = suma + punto*altura
  }
  return(suma)
}

r<-reiman(100000)
r

#3.

cribaEratostenes<- function(n){
  if(n<1){
    print("no hay primos")
    return(0)
  }
  if(n>1){
    lista<-2
    #print(2)
  }
  for (i in 2:n){
    p=1
    for(j in 2:((i+1)%/%2)){
      if( i%%j == 0){
        p=0
        break;
      }      
    }
    if(p)
      #print(i)
      lista <- cbind(lista,i)
    
  }
  return(lista)
}
p<-cribaEratostenes(100)

#####discriminante
raices<- function(a,b,c){
  discriminante<-(b^2-4*a*c)
  #print(discriminante)
  #print(double(discriminante))
  if(all(double(discriminante) != 0)){
    print((-1*b)/2*a)
  }
  else{
    raiz1 = (-1*b+sqrt(discriminante))/2*a
    raiz2 = (-1*b-sqrt(discriminante))/2*a
    print(raiz1)
    print(raiz2)
  }
    
}
 
raices(1,3,2) 

# ultimo
descomoposicionD <- function(A,n){
  D = matrix(0,nrow = n,ncol = n)
  L = matrix(0,nrow = n,ncol = n)
  U = matrix(0,nrow = n,ncol = n)
  solucion = c(D,L,U)
  for(i in 1:n){
    if(A[i,i]==0){
      D[i,i] = -1
      L[i,i] = 1
    }else{
      D[i,i] = A[i,i]
    }
  }
  for(j in 1:n){
    for(k in 1:n){
      if(k>j){
        U[j,k] = A[j,k]
      }
      if(k<j){
        L[j,k] = A[j,k]
      }
    }
  }
  aux=L
  L=t(U)
  U=t(aux)
  return(D)
}
descomoposicionL <- function(A,n){
  D = matrix(0,nrow = n,ncol = n)
  L = matrix(0,nrow = n,ncol = n)
  U = matrix(0,nrow = n,ncol = n)
  solucion = c(D,L,U)
  for(i in 1:n){
    if(A[i,i]==0){
      D[i,i] = -1
      L[i,i] = 1
    }else{
      D[i,i] = A[i,i]
    }
  }
  for(j in 1:n){
    for(k in 1:n){
      if(k>j){
        U[j,k] = A[j,k]
      }
      if(k<j){
        L[j,k] = A[j,k]
      }
    }
  }
  aux=L
  L=t(U)
  U=t(aux)
  return(L)
}
descomoposicionU <- function(A,n){
  D = matrix(0,nrow = n,ncol = n)
  L = matrix(0,nrow = n,ncol = n)
  U = matrix(0,nrow = n,ncol = n)
  solucion = c(D,L,U)
  for(i in 1:n){
    if(A[i,i]==0){
      D[i,i] = -1
      L[i,i] = 1
    }else{
      D[i,i] = A[i,i]
    }
  }
  for(j in 1:n){
    for(k in 1:n){
      if(k>j){
        U[j,k] = A[j,k]
      }
      if(k<j){
        L[j,k] = A[j,k]
      }
    }
  }
  aux=L
  L=t(U)
  U=t(aux)
  return(U)
}
A = matrix(1:9,nrow = 3,ncol = 3)
D<-descomoposicionD(A,3)
U<-descomoposicionU(A,3)
L<-descomoposicionL(A,3)
print(D+U+L)

Tj<-function(U,D,L){
  invD = solve(D)
  t_j = invD %*% (L+U)
  return(t_j)
}
t_j <- Tj(U,D,L)

x=c(1,0,0)
b=c(1,4,7)
iteraciones=100
for(i in 1:iteraciones){
  invD=solve(D)
  mat <- invD%*%b
  re <- t_j%*%x
  x <- mat-re
}
x


