

tree.dim <- read.csv("data/tree dimensions.csv")
colnames(tree.dim) <- paste(colnames(tree.dim), tree.dim[1, ])
tree.dim <- tree.dim[-1, ]
tree.dim <- tree.dim[!(grepl("D", tree.dim[,1])), ]

traits <- data.frame(trunk.radius = tree.dim[, "Trunk.2 Radius (cm)"],
                      crown.radius = tree.dim[, "Average.radius with trunk (cm)"], 
                      litter = l.dat[, "Litter.."])
traits <- data.frame(apply(traits, 2, as.numeric))
tra.dif <- split(traits, l.dat[, "Tree.pairs"])
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

l.com.dif <- split(com, l.dat[, "Tree.pairs"])
l.com.dif <- lapply(l.com.dif, function(x) x[2, ] - x[1, ])
l.com.dif <- do.call(rbind, l.com.dif)

v.com.dif <- split(v.com, l.dat[, "Tree.pairs"])
v.com.dif <- lapply(v.com.dif, function(x) x[2, ] - x[1, ])
v.com.dif <- do.call(rbind, v.com.dif)

l.ard <- t(apply(com, 1, function(x) c(abundance = sum(x), 
                                     richness = sum(sign(x)), 
                                     diversity = diversity(x))))
v.ard <- t(apply(v.com, 1, function(x) c(abundance = sum(x), 
                                     richness = sum(sign(x)), 
                                     diversity = diversity(x))))
l.ard.dif <- apply(l.ard, 2, 
                   function(x, f) 
                       tapply(x, f, diff), 
                   f = l.dat[, "Tree.pairs"])
v.ard.dif <- apply(v.ard, 2, 
                   function(x, f) 
                       tapply(x, f, diff), 
                   f = l.dat[, "Tree.pairs"])

tra.dif.d <- dist(tra.dif)
l.com.dif.d <- dist(l.com.dif)
v.com.dif.d <- dist(v.com.dif)
env.dif.d <- dist(env.dif)

ind.env.dif.d <- lapply(split(as.matrix(env.dif), col(env.dif)), dist)
names(ind.env.dif.d) <- colnames(env.dif)

set.seed(12345)
adonis2(v.com.ds ~ Moth, data = l.dat, strata = l.dat[, "Tree.pairs"], 
        perm = 9999)

set.seed(12345)
adonis2(v.com.ds ~ Litter.. + Big.rocks.. + Small.rocks.. + Light...average, 
        data = l.dat, strata = l.dat[, "Tree.pairs"], perm = 9999, 
        by = "margin")

vegan::mantel(tra.dif.d, env.dif.d)
vegan::mantel(env.dif.d, com.dif.d)
vegan::mantel(tra.dif.d, com.dif.d)
vegan::mantel(v.com.dif.d, l.com.dif.d)
vegan::mantel(vegdist(v.com.ds), vegdist(com.ds))

adonis2(com.ds ~ Moth, data = l.dat, strata = l.dat[, "Tree.pairs"], perm = 999)
adonis2(v.com.ds ~ Moth, data = l.dat, strata = l.dat[, "Tree.pairs"], perm = 999)

adonis2(traits ~ Moth, data = l.dat, strata = l.dat[, "Tree.pairs"], perm = 999)

do.call(rbind, lapply(apply(tra.dif, 2, t.test), unlist))[, c(1, 2, 6, 3)]

adonis(env.dif.d ~ trunk.radius + crown.radius + litter, 
        data = tra.dif, by = "margin", perm = 9999)
adonis(com.dif.d ~ litter+ trunk.radius + crown.radius, 
        data = tra.dif, by = "margin", perm = 9999)
adonis(com.dif.d ~ light + rocks.big + rocks.small, 
        data = env.dif, by = "margin", perm = 9999)

summary(lm(rocks.big ~ crown.radius + litter, data = data.frame(env.dif, tra.dif)))
summary(lm(rocks.big ~ crown.radius + litter, data = data.frame(env.dif, tra.dif)))
summary(lm(litter ~ trunk.radius + crown.radius, data = tra.dif))
summary(lm(light ~ crown.radius + trunk.radius + litter, 
           data = data.frame(env.dif, tra.dif)))

l.com.dif.nms <- nmds(com.dif.d)
l.com.dif.ord <- min(l.com.dif.nms, 2)
l.com.dif.vec <- envfit(l.com.dif.ord, data.frame(env.dif, tra.dif))

v.com.dif.nms <- nmds(v.com.dif.d)
v.com.dif.ord <- min(v.com.dif.nms, 2)
v.com.dif.vec <- envfit(v.com.dif.ord, data.frame(env.dif, tra.dif))

do.call(rbind, lapply(apply(l.ard.dif, 2, t.test), unlist))[, c(1, 2, 6, 3)]
do.call(rbind, lapply(apply(v.ard.dif, 2, t.test), unlist))[, c(1, 2, 6, 3)]

summary(lm(abundance ~ rocks.big * litter * light, 
           data = data.frame(l.ard.dif, tra.dif, env.dif)))
summary(lm(richness ~ rocks.big * litter * light, 
           data = data.frame(l.ard.dif, tra.dif, env.dif)))
summary(lm(diversity ~ rocks.big * litter * light, 
           data = data.frame(l.ard.dif, tra.dif, env.dif)))

summary(lm(abundance ~ rocks.big * litter * light, 
           data = data.frame(v.ard.dif, tra.dif, env.dif)))
summary(lm(richness ~ rocks.big * litter * light, 
           data = data.frame(v.ard.dif, tra.dif, env.dif)))
summary(lm(diversity ~ rocks.big * litter * light, 
           data = data.frame(v.ard.dif, tra.dif, env.dif)))


pdf("results/lichen_dif_ord.pdf")
plot(l.com.dif.ord, xlab = "Axis 1", ylab = "Axis 2")
plot(l.com.dif.vec, add = TRUE)
dev.off()

pdf("results/vegn_dif_ord.pdf")
plot(v.com.dif.ord, xlab = "Axis 1", ylab = "Axis 2")
plot(v.com.dif.vec, add = TRUE)
dev.off()
