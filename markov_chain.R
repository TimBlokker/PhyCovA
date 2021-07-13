install.packages("markovchain")
library(markovchain)
x<-read.csv("output/transition_distances_batRABV.MCC.keep.target.heights.treesMPTRUE.csv")
head(x)
mat<-matrix(rep(0, 17*17), ncol=17)
mat[upper.tri(mat, diag = F)]<-x$Transitions[1:136]
mat[lower.tri(mat, diag = F)]<-x$Transitions[137:length(x$Transition)]
names_mat<-sort(unique(unlist(strsplit(x$Key, split = "->"))))

mat_no_diag<-matrix(x$Transitions, byrow = F, ncol = 17)

vec<-0
for(j in seq(1,17,1)){
  for(i in seq(1,16,1)){
    if(i==1 & j==1) vec<-0
    if(j==i & j!=1) vec<-append(vec, 0)
    vec<-append(vec,mat_no_diag[i,j])
  }
}
vec<-append(vec,0)
mat<-matrix(vec, byrow = F, ncol=17)
rownames(mat)<-names_mat
colnames(mat)<-names_mat


mat_scaled<-t(scale(mat, center=F,scale=apply(mat, 1, sum)))
plot(as(mat_scaled, "markovchain"), edge.arrow.size=0.2)

