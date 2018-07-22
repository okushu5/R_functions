# Delete everything
rm(list=ls())


# glm_object, gam_obj, etc.  get the name fo the response variable.
all.vars(object$terms)[attr(object$terms,"response")]

all.vars(object$terms)[-attr(object$terms,"response")]

# One way to measure importance/contribution of variables in a model.
contribution = function(GLM_or_GAM_obj){
o <- apply(predict(GLM_or_GAM_obj, type='terms'),2,sd)
return(o/sum(o))
}

Contribution2 = function(GLMobj, EvalFunction = sd){
	# Change one variable at a time and see how predications change, while holding other variables constant.
	# This version works also if pmin(x1, 0.8) and as.factor are in the model.
	# Works also for mcv:gam objects.
	# MARS models (earth package) form would work , too: y~pmin(0,x1-2) + pmin(0,2-x1) etc.
	xVars = all.vars(GLMobj$terms)[-attr(GLMobj$terms,"response")]
	# If the data was updated after gam was run, then this will return the updated version.
	Data = copy(eval(GLMobj$call$data))
	Data = as.data.table(na.omit(subset(x=Data, select=xVars)))
	Metric <- Data[1,] ; Metric[,] <- NA

	for (var1 in xVars){
	PredData <- copy(Data)
		for (v1 in xVars){
			if (v1!=var1){ PredData[[v1]] = PredData[[v1]][1] }
			}
	if (all(class(GLMobj=='lm'){
		PredData$p = predict(GLMobj, newdata=PredData)
		} else {
		PredData$p = predict(GLMobj, newdata=PredData, type='link')
		}
	Metric[[var1]] = EvalFunction(PredData$p)
	}
	return(Metric/sum(Metric))
}


