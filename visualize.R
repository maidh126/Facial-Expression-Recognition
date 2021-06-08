

data1A <- as.data.frame(scale(data1A,scale = FALSE,center = TRUE))


data1A <- data.frame(scale(data1B)) 

x <- data1A %>% select(ends_with('x'))
y <- data1A %>% select(ends_with('y'))
#z <- data1A %>% select(ends_with('z'))


# 
# x <- data.frame(t(x[1,]))
# x <- x %>% rename("x" = "X1")
# 
# y <- data.frame(t(y[1,]))
# y <- y %>% rename("y" = "X1")
# 
# xy <- cbind(x, y)
# 
# ggplot(xy, aes(x, y)) + geom_point()



x <- data.frame(x[1,])
y <- data.frame(y[1,])

x <- as.numeric(rbind(unlist(x)))
y <- as.numeric(rbind(unlist(y)))


xy <- data.frame(x, y)
ggplot(xy, aes(x, y)) + geom_point()





# x <- data.frame(x)
# y <- data.frame(y)
# 
# x <- as.numeric(rbind(unlist(x)))
# y <- as.numeric(rbind(unlist(y)))
# #z <- as.numeric(rbind(unlist(z)))
# 
# # xy <- data.frame(timestamp = rep(data1A[,1], 100), x, y)
# 
# xy <- data.frame(x, y)
# ggplot(xy, aes(x, y)) + geom_point()



# 
# plot <- ggplot(xy, 
#        aes(x, y, frame = timestamp))+
#   geom_point()+
#   labs(x="X",y="Y")
# 
# gganimate(plot)
# 
# 
# # Make a ggplot, but add frame=year: one image per year
# plot <- ggplot(xy, aes(x, y)) +
#   geom_point() +
#   scale_x_log10() +
#   theme_bw() +
#   # gganimate specific bits:
#   transition_time(timestamp)
# 
# 
# # Save at gif:
# gganimate(plot)

