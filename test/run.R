input_file='/Users/jimmy/Dropbox/processing-program/sustc-iris/vdjTools/2.segments.wt.V.txt'
df<-read.table(input_file, header=T, sep="\t", comment="")
head(df)
vcols<-4:51
df[, vcols] <- apply(df[, vcols], 2, as.numeric)
x <- as.matrix(df[, vcols])
rownames(x)=df[,1]
vdj_usage_ht(x,'v-usage-heatmap.pdf')

file_in='/Users/jimmy/Dropbox/processing-program/sustc-iris/vdjTools/5.fancyvj.wt.txt'
temp <- read.table(file_in, sep="\t", comment="")
n <- nrow(temp)
m <- ncol(temp)
rn = as.character(temp[2:n,1])
cn = apply(temp[1,2:m], 2 , as.character)
mat <- matrix(apply(temp[2:n, 2:m], 1:2, as.numeric), n - 1, m-1)
rownames(mat)=rn
colnames(mat)=cn
v_j_circle(mat,'v-j-circle.pdf')


