library(httr)
acceso.api <- 'https://pubsbapi.smartbike.com/oauth/v2/token?client_id={1569_3q712quy66uc0g4cw8k48s4wks00wsk8sgog4ss40kk4sc8kc8}&client_secret={5m12s2mn98w80w8ogsw4gc48gskggk8cwoo8s8c0s4s0sw8g0g}&grant_type=client_credentials'
token <- GET(acceso.api)
token
