#1
grade = matrix(sample(0:100, 200, replace = T), nrow = 50)

#a

total = c()
for(i in 1:50){
  total[i] = sum(grade[i,c(1,2,3,4)])
}
total

#b

range = c()
for(i in 1:50){
  MAX = max(grade[i, c(1,2,3,4)])
  MIN = min(grade[i, c(1,2,3,4)])
  range[i] = MAX - MIN
}
range

#c

MIN = c()
for(i in 1:50){
  MIN[i] = min(grade[i, c(1:4)])
}
MIN

#d

exam.min = c()
for(i in 1:4){
  exam.min[i] = min(grade[c(1:50), i])
}

student = c()
k = 0
for(i in 1:4){
  for(j in 1:50){
    if(grade[j, i] == exam.min[i]){
      k = k + 1
      student[k] = j
    }
  }
}
student

#e

mean1 = mean(grade[c(1:50), 1])
mean2 = mean(grade[c(1:50), 2])
mean3 = mean(grade[c(1:50), 3])
mean4 = mean(grade[c(1:50), 4])
Mean = c(mean1, mean2, mean3, mean4)

test1 = c()
test2 = c()
test3 = c()
test4 = c()

for(i in 1:50){
  test1[i] = 1*(grade[i, 1] >= Mean[1])
  test2[i] = 1*(grade[i, 2] >= Mean[2])
  test3[i] = 1*(grade[i, 3] >= Mean[3])
  test4[i] = 1*(grade[i, 4] >= Mean[4])
}
sum((test1+test2+test3+test4) >= 2)

#f

mean.score = c()
for(i in 1:50){
  mean.score[i] = mean(grade[i, c(1:4)])
}
score = sort(mean.score)

id = c()
k = 0
for(i in 41:50){
  for(j in 1:50){
    if(score[i] == mean.score[j]){
      k = k + 1
      id[k] = j
      break
    }
  }
}

for(i in 10:1){
  print(grade[id[i], c(1:4)])  
}

#g

weigh.score = c()
for(i in 1:50){
  weigh.score[i] = 0.15*grade[i,1] + 0.15*grade[i,2] + 0.3*grade[i,3] + 0.4*grade[i,4]
}
weigh.score

#2

a = floor(runif(20, -5, 5))
b = round(runif(20, -5, 5), 1)

x = c()
y = c()
z = c()
w = c()

for(i in 1:20){
  x[i] = a[i]/b[i]
  y[i] = b[i]*b[i] - 4*a[i]
  z[i] = (2 + sqrt(abs(a[i]))) / (-1 + sqrt(abs(b[i])))
  if(i%%2 == 0){
    w[i] = a[i]*a[i]
  }else{
    w[i] = (-1)*b[i]
  }
}
matrix(c(x, y, z, w), nrow=20, ncol=4)

#3

#a

fsun = function(x, n){
  a = 1
  sum = a
  for(k in 1:n){
    sum = sum + (x^k/factorial(k))
  }
  return(sum)
}

#b

fsun = function(x, n){
  k = 0
  a = 1
  sum = a
  repeat{
    k = k + 1
    sum = sum + (x^k)/factorial(k)
    if(k == n){break}
  }
  return(sum)
}

#c

x = runif(1, -1, 1) 

T = 0
repeat{
  T = T + 1
  if(abs(fsun(x, T)-exp(x)) < 0.001){break}
}
T

#4

number = runif(1000, -2, 2)

#a

n = 0
for(i in 1:1000){
  if(number[i] > 1){
    n = n + 1
  }
  if(n == 15){
    print(i)
    break
  }
}

#b

n = 0
i = 0
while(n < 15){
  i = i + 1
  if(number[i] > 1){
    n = n + 1
  }
}
i






























