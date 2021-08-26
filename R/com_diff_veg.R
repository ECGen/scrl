
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
