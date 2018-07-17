# glm_object, gam_obj, etc.  get the name fo the response variable.
all.vars(object$terms)[attr(object$terms,"response")]

all.vars(object$terms)[-attr(object$terms,"response")]
