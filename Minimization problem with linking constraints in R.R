

f.obj <-  c(22,20.5,21,19,21,21,18,17,22,24,22,23,22,16,17,17.5,1500,1700,1500,1400)
n <- length(f.obj)
names(f.obj)[seq(n - 4)] <- paste0('X[', rep(1:4, 4), ', ', rep(1:4, each = 4), ']')
names(f.obj)[-seq(n - 4)] <- paste0('Y[', 1:4, ']')
binary.vec <- seq(n, n - 3)
f.obj <- c(f.obj, numeric(8))
n <- length(f.obj)
names(f.obj)[seq(n - 7, n)] <- paste0('z[', rep(1:4, 2), ', ', rep(1:2, each = 4), ']')
tail(f.obj, 12)
#################
## Constraints ##
#################
cons1 <- numeric(n)
names(cons1) <- names(f.obj)
cons12 <- cons11 <- cons10 <- cons9 <- cons8 <- cons7 <- cons6 <- cons5 <- cons4 <- cons3 <- cons2 <- cons1
cons1[1:4] <- c(2, 1.8, 2.3, 2.1)
cons2[5:8] <- c(2.8, 2.3, 2.2, 2.6)
cons3[9:12] <- c(1.7, 1.75, 1.6, 1.9)
cons4[13:16] <- c( 2.4, 1.9, 2.6, 2.4)
#X[i, j] - Cy[i] * Y[i] <= 0
cons5[c('X[1, 1]', 'Y[1]')] <- c(1, -1500)
cons6[c('X[2, 1]', 'Y[2]')] <- c(1, -2000)
cons7[c('X[3, 1]', 'Y[3]')] <- c(1, -1500)
cons8[c('X[4, 1]', 'Y[4]')] <- c(1, -1800)
cons9[c('X[1, 2]', 'Y[1]')] <- c(1, -500)
cons10[c('X[2, 2]', 'Y[2]')] <- c(1, -500)
cons11[c('X[3, 2]', 'Y[3]')] <- c(1, -500)
cons12[c('X[4, 2]', 'Y[4]')] <- c(1, -500)
#Pseudo-constraints
## Start with C * z[i, j] > x[i, j] for some very large C (here i choose 1e12, but could be any number large enough so X[i, j] does not exceed it!)
pseudo_z_1 <- numeric(n)
names(pseudo_z_1) <- names(f.obj)
pseudo_z_8 <- pseudo_z_7 <- pseudo_z_6 <- pseudo_z_5 <- pseudo_z_4 <- pseudo_z_3 <- pseudo_z_2 <- pseudo_z_1
C <- 1e12
pseudo_z_1[c('X[1, 1]', 'z[1, 1]')] <- c(-1, C)
pseudo_z_2[c('X[2, 1]', 'z[2, 1]')] <- c(-1, C)
pseudo_z_3[c('X[3, 1]', 'z[3, 1]')] <- c(-1, C)
pseudo_z_4[c('X[4, 1]', 'z[4, 1]')] <- c(-1, C)
pseudo_z_5[c('X[1, 2]', 'z[1, 2]')] <- c(-1, C)
pseudo_z_6[c('X[2, 2]', 'z[2, 2]')] <- c(-1, C)
pseudo_z_7[c('X[3, 2]', 'z[3, 2]')] <- c(-1, C)
pseudo_z_8[c('X[4, 2]', 'z[4, 2]')] <- c(-1, C)
## Next create pseudo constraints for Y[i] <= 0.5 * Z[i, 1] + 0.5 * Z[i, 2]
pseudo_y_1 <- numeric(n)
names(pseudo_y_1) <- names(f.obj)
pseudo_y_4 <- pseudo_y_3 <- pseudo_y_2 <- pseudo_y_1
pseudo_y_1[c('Y[1]', 'z[1, 1]', 'z[1, 2]')] <- c(1, -0.5, -0.5)
pseudo_y_2[c('Y[2]', 'z[2, 1]', 'z[2, 2]')] <- c(1, -0.5, -0.5)
pseudo_y_3[c('Y[3]', 'z[3, 1]', 'z[3, 2]')] <- c(1, -0.5, -0.5)
pseudo_y_4[c('Y[4]', 'z[4, 1]', 'z[4, 2]')] <- c(1, -0.5, -0.5) 
#Combine them all together
cons <- c(paste0('cons', 1:12), paste0('pseudo_z_', 1:8), paste0('pseudo_y_', 1:4))
cons.mat <- do.call(rbind, mget(cons))

cons.dir <- c(rep('>=', 4), rep('<=', 8), rep('>=', 8), rep('<=', 4))
names(cons.dir)[1:24] <- cons
cons.rhs <- c(750, 800, 1000, 300, rep(0, 8), rep(0, 8), rep(0, 4))
binary.vec <- c(binary.vec, seq(n, n - 7))
res <- lpSolve::lp('min', objective.in = f.obj, 
            const.mat = cons.mat,
            const.dir = cons.dir, 
            const.rhs = cons.rhs, 
            binary.vec = binary.vec)
names(res$solution) <- names(f.obj)
round(res$solution[res$solution > 0], 3)
