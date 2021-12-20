(define gensym
  (case-lambda
  [() (xiao.lang.Procedures/gensym)]
  [(prefix) (xiao.lang.Procedures/gensym prefix)]))