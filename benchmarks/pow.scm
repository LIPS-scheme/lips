#!/usr/bin/env -S lips --debug -t

(load "./helpers.scm")


(suite (add "pow: op" (lambda ()
                        (--> (new Array 200)
                             (fill 0)
                             (map (lambda ()
                                     (** 1000 1000))))))
       (add "pow: exp" (lambda ()
                        (--> (new Array 200)
                             (fill 0)
                             (map (lambda ()
                                    (Math.pow 1000 1000)))))))