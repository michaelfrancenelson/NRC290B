dat = read.csv("roster_feb_4.csv")
head(dat)
nrow(dat)
names = as.character(sample(dat$FullName))
names_1 = strsplit(names, ",")
names_final = sapply(names_1, function(x) paste(x[2], x[1]))

seq(1, 7)

seq(1, length(names_final), length.out = 8)


(index_start = seq(1, 37, len = 7))
(index_end   = c(head(index_start, -1) + 5, 43))
(indices = data.frame(start = index_start, end = index_end, size = index_end - index_start + 1))

sum(indices$size)


groups_string = c()

for (i in 1:nrow(indices))
{
groups_string = c(groups_string, paste0("Group ", i), names_final[indices[i, 1]:indices[i, 2]])  
}

groups_string

clipr::write_clip(groups_string)

