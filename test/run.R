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

v_j_combination(mat,'circle','test/input_files/v-j-circle.pdf')
v_j_combination(mat,'bubble','test/input_files/v-j-bubble.pdf')

file_in='test/input_files/top10-CDR3-aa-length-distribution.txt'
df <- read.table(file_in, sep ="\t", header = TRUE, comment="", quote="")
df[, 1:ncol(df)] <- apply(df[, 1:ncol(df)], 2, as.numeric)
barplot_cdr3aa(df,'test/input_files/cdr3aa-usage-cdr3aa-length.pdf','CDR3 amino acid','F')

file_in='test/input_files/top10-v-usage-CDR3-aa-length-distribution.txt'
df <- read.table(file_in, sep ="\t", header = TRUE, comment="", quote="")
df[, 1:ncol(df)] <- apply(df[, 1:ncol(df)], 2, as.numeric)
barplot_cdr3aa(df,'test/input_files/v-usage-cdr3aa-length.pdf','varibale segments','F')

file_in='test/input_files/cdr3-aa-stats.txt'
df <- read.table(file_in, sep ="\t", header = TRUE, comment="", quote="")
cdr3_stat_plot(df,'test/input_files/cdr3-aa-stats.pdf')

file_in='test/input_files/cdr3_paired_comparison_matrix.txt'
df <- read.table(file_in, sep ="\t", header = TRUE, comment="", quote="")
sample1_cdr3=df[,c(4,15)];
sample2_cdr3=df[,c(4,16)];
cdr3_paired_comparison(sample1_cdr3,sample2_cdr3,'case','control','test/input_files/cdr3_paired_comparison.pdf')




