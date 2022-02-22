pdf("outputs/paper/Figure_1.pdf")
locs <- c(0, 110, 194)
vaccs <- c(0, 10, 14)
plot(c(locs[1], locs[3]), c(vaccs[1], vaccs[3]), type = "n",
     xaxt = 'n', yaxt = 'n', bty  = 'n',
     xlab = "Number of countries included",
     ylab = "Number of pathogens included")

rect(locs[1], vaccs[1], locs[2], vaccs[2], col = 'royalblue')
rect(locs[2], vaccs[1], locs[3], vaccs[2], col = 'salmon3')
rect(locs[1], vaccs[2], locs[3], vaccs[3], col = 'mediumseagreen')

axis(side = 2, at = vaccs)
axis(side = 1, at = locs)

text(locs[2] / 2, vaccs[2] / 2, "Group 1:\nVIMC countries\nVIMC pathogens", col = 'white')
text((locs[3] - locs[2]) / 2 + locs[2], vaccs[2] / 2, "Group 2:\nAdditional countries\nVIMC pathogens", col = 'white')
text(locs[3] / 2, (vaccs[3] - vaccs[2]) / 2 + vaccs[2], "Group 3:\nAll countries\nAdditional pathogens", col = 'white')
dev.off()