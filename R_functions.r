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