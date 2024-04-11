
## Multi-output model (coupled system)
ARMAX <- marima("Tinner ~ AR(1) + Touter(1) + MA(1)",
                "Touter ~ AR(1) + Tinner(1) + MA(1)", data=X,
                penalty=0)
summary(ARMAX)
validate(ARMAX)