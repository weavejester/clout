(ns clout.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [clout.core-test]))

(doo-tests 'clout.core-test)
