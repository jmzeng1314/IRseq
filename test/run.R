input_file='test/input_files/V-usage-all-samples.txt'
df<-read.table(input_file, header=T, sep="\t", comment="")
head(df)
vcols<-4:51
df[, vcols] <- apply(df[, vcols], 2, as.numeric)
x <- as.matrix(df[, vcols])
rownames(x)=df[,1]
vdj_usage_ht(x,'test/input_files/v-usage-heatmap.pdf')

input_file='test/input_files/J-usage-all-samples.txt'
df<-read.table(input_file, header=T, sep="\t", comment="")
head(df)
vcols<-4:16
df[, vcols] <- apply(df[, vcols], 2, as.numeric)
x <- as.matrix(df[, vcols])
rownames(x)=df[,1]
vdj_usage_ht(x,'test/input_files/j-usage-heatmap.pdf')


file_in='test/input_files/V-J-usage-for-a-sample.txt'
temp <- read.table(file_in, sep="\t", comment="")
n <- nrow(temp)
m <- ncol(temp)
rn = as.character(temp[2:n,1])
cn = apply(temp[1,2:m], 2 , as.character)
mat <- matrix(apply(temp[2:n, 2:m], 1:2, as.numeric), n - 1, m-1)
rownames(mat)=rn
colnames(mat)=cn
v_j_circle(mat,'test/input_files/v-j-circle.pdf')


