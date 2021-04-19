tree.dim <- read.csv("data/tree dimensions.csv")
colnames(tree.dim) <- paste(colnames(tree.dim), tree.dim[1, ])
tree.dim <- tree.dim[-1, ]
tree.dim <- tree.dim[!(grepl("D", tree.dim[,1])), ]

tra.dif <- data.frame(trunk.radius = tree.dim[, "Trunk.2 Radius (cm)"],
                      crown.radius = tree.dim[, "Average.radius with trunk (cm)"], 
                      litter = l.dat[, "Litter.."])
tra.dif <- data.frame(apply(tra.dif, 2, as.numeric))
tra.dif <- split(tra.dif, l.dat[, "Tree.pairs"])
tra.dif <- lapply(tra.dif, function(x) x[2, ] - x[1, ])
tra.dif <- do.call(rbind, tra.dif)

env.dif <- data.frame(rocks = l.dat[, "Big.rocks.."] + l.dat[, "Small.rocks.."],
                      rocks.big = l.dat[, "Big.rocks.."],
                      rocks.small = l.dat[, "Small.rocks.."],
                      light = l.dat[, "Light...average"]
                      )
env.dif <- data.frame(apply(env.dif, 2, as.numeric))
env.dif <- split(env.dif, l.dat[, "Tree.pairs"])
env.dif <- lapply(env.dif, function(x) x[2, ] - x[1, ])
env.dif <- do.call(rbind, env.dif)

com.dif <- split(com, l.dat[, "Tree.pairs"])
com.dif <- lapply(com.dif, function(x) x[2, ] - x[1, ])
com.dif <- do.call(rbind, com.dif)

tra.dif.d <- dist(tra.dif)
com.dif.d <- dist(com.dif)
env.dif.d <- dist(env.dif)

ind.env.dif.d <- lapply(split(as.matrix(env.dif), col(env.dif)), dist)
names(ind.env.dif.d) <- colnames(env.dif)

vegan::mantel(tra.dif.d, env.dif.d)
vegan::mantel(env.dif.d, com.dif.d)
vegan::mantel(tra.dif.d, com.dif.d)

adonis(env.dif.d ~ trunk.radius + crown.radius + litter, 
        data = tra.dif, by = "margin", perm = 9999)
adonis(com.dif.d ~ light + rocks.big + rocks.small, 
        data = env.dif, by = "margin", perm = 9999)

summary(lm(rocks.big ~ crown.radius + litter, data = data.frame(env.dif, tra.dif)))

com.dif.nms <- nmds(com.dif.d)
com.dif.ord <- min(com.dif.nms, 2)
com.dif.vec <- envfit(com.dif.ord, data.frame(env.dif, tra.dif))

pdf("results/lichen_dif_ord.pdf")
plot(com.dif.ord, xlab = "Axis 1", ylab = "Axis 2")
plot(com.dif.vec, add = TRUE)
dev.off()
