
model.std = stdReg::stdGlm(model, data = nhefs.nmv, X = "qsmk", x = c(0, 1))
summary(model.std, contrast = "difference", reference = 0)
summary(model.std, contrast = "ratio", reference = 0)
