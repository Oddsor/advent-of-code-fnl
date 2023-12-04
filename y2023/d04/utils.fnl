(fn take-last [n xs]
  (fcollect [i (- (length xs) n) (length xs)]
    (. xs i)))

(fn first [xs] (. xs 1))
(fn last [xs] (. xs (length xs)))

(fn slurp [file]
  (let [f (io.open file)] (f:read :*a)))

(fn sort [xs]
  (doto (icollect [_ x (ipairs xs)] x)
    table.sort))

(fn split [pattern text]
  (var txt text)
  (local acc [])
  (while (txt:find pattern)
    (let [(start end) (string.find txt pattern)]
      (table.insert acc (string.sub txt 1 (- start 1)))
      (set txt (string.sub txt (+ end 1) (length txt)))))
  ;; Can't find any more things to split on, add last piece of text
  (table.insert acc txt)
  acc)

(fn sum [xs]
  (accumulate [sum 0 _ x (ipairs xs)]
    (+ sum x)))

    (fn keep [fun xs]
  (icollect [_ x (ipairs xs)] (fun x)))

(fn filter [fun xs]
  (icollect [_ x (ipairs xs)] (if (fun x) x nil)))

(fn remove [fun xs]
  (icollect [_ x (ipairs xs)] (if (fun x) nil x)))

(fn empty? [x] (= 0 (length x)))

{: take-last : first : last : slurp : sort : split : sum : filter : remove : empty? : keep}
