# defining classes

setClass("ahpObj",representation(weight = "numeric", saaty_inconsistency = "numeric"))

setClass("ahmatrixObj",representation(ahp_matrix="matrix"))

setClass("pipelineObj", representation(ahp_plot = 'matrix', weight_plot = 'list', rainbow_plot = 'list', 
  ahp_weights = 'list', simulation = 'numeric', saaty_inconsistency = 'list'))

setClass("geoAggreg",representation(AIJ="matrix", GCR = "numeric", CI = "numeric", ICR = 'numeric', IP = 'matrix'))

setClass("ariAggreg",representation(AIP="numeric", ICR = 'numeric', IP = 'matrix'))

setClass("ratingObj",representation(weight="matrix", saaty_inconsistency = 'numeric', RM = "matrix"))

