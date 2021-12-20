(define gensym
  (case-lambda
  [() (xiao.lang2.Procedures/gensym)]
  [(prefix) (xiao.lang2.Procedures/gensym prefix)]))