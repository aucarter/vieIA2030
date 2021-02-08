locs <- c(0, 98, 194)
vaccs <- c(0, 10, 14)
plot(c(locs[1], locs[3]), c(vaccs[1], vaccs[3]), type = "n",
     xaxt = 'n', yaxt = 'n', bty  = 'n',
     xlab = "Number of countries included",
     ylab = "Number of VPDs included")

rect(locs[1], vaccs[1], locs[2], vaccs[2], col = 'royalblue')
rect(locs[2], vaccs[1], locs[3], vaccs[2], col = 'salmon3')
rect(locs[1], vaccs[2], locs[3], vaccs[3], col = 'mediumseagreen')

axis(side = 2, at = vaccs)
axis(side = 1, at = locs)

text(locs[2] / 2, vaccs[2] / 2, "VIMC locations\nVIMC VPDs", col = 'white')
text((locs[3] - locs[2]) / 2 + locs[2], vaccs[2] / 2, "Additional locations\nVIMC VPDs", col = 'white')
text(locs[3] / 2, (vaccs[3] - vaccs[2]) / 2 + vaccs[2], "All locations\nAdditional VPDs", col = 'white')
