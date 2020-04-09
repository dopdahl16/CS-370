(scheme-value
 '((lambda a (x)
     ((lambda b x
        ((lambda c (x)
           ((lambda d (w . x)
              ((lambda e (x)
                 ((lambda c (x)
                    ((lambda f (x)
                       ((lambda x
                          x)
                        (:: a x) (:: b x) (:: c x) (:: d x) (:: e x) (:: f x) x))
                     'one))
                  'two))
               'three))
            'four1 'four2 'four3 'four4))
         'five))
      'six))
   'seven))
