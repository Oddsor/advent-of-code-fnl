(local test-data "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(local {: split : slurp : take-last : last : sum : sort} (require :utils))

(fn split-bags [text]
  (icollect [_ bag (ipairs (split "\n\n" text))]
    (accumulate [sum 0 x (string.gmatch bag "[^\r\n]+")]
      (+ sum (tonumber x)))))

(fn part-1 [bags] (last bags))
(fn part-2 [bags] (->> bags (take-last 3) sum))

(print :Part :Answer)

(local test-bags (->> test-data split-bags sort))

(print "Test 1" (part-1 test-bags))
(print "Test 2" (part-2 test-bags))

(local bags (->> (slurp ...) split-bags sort))

(print "Real 1" (part-1 bags))
(print "Real 2" (part-2 bags))
