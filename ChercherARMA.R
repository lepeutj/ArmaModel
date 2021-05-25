#### fonction chercherARMA ####
chercherArma <- function(X,pMax,qMax,crit) {

param<-c(0,0)


if ( kpss.test(X-mean(X))$p.val >0.05 | adf.test(X-mean(X))$p.val<0.05  )
{
if (crit == "lv") {

	init<-arima(X-mean(X),c(0,0,0))
 		for (i in 0:pMax) { 
			for (j in 0:qMax) {
				approx <- arima(X-mean(X),c(i,0,j))   
				if( init$loglik < approx$loglik ) 
					{ init <-approx
					  param<-c(i,j) 
					 }
		   		    }
 				  }
			}
if (crit == "aic") 
 	{ 
		init <-arima(X-mean(X),c(0,0,0))
		for (i in 0:pMax) { 
				for (j in 0:qMax) {
						approx <- arima(X-mean(X),c(i,0,j))    
						if( init$aic > approx$aic ) 
						{ init <-approx
						  param<-c(i,j) 
						 }
		   				     	}
					}
      }

return(param)


}
 else warning ("la série n'est pas stationnaire")

}
