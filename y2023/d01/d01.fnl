;; Import and create helper functions due to sparse core library

(local {: split : last : slurp : sum} (require :utils))

(fn keep [fun xs]
  (icollect [_ x (ipairs xs)] (fun x)))

(fn filter [fun xs]
  (icollect [_ x (ipairs xs)] (if (fun x) x nil)))

(fn remove [fun xs]
  (icollect [_ x (ipairs xs)] (if (fun x) nil x)))

(fn empty? [x] (= 0 (length x)))

;; Actual domain-stuff:

(local test-data "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")

(local test-data2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(local string-to-number {:one :1
                         :two :2
                         :three :3
                         :four :4
                         :five :5
                         :six :6
                         :seven :7
                         :eight :8
                         :nine :9})

;; Given a piece of text, find all numbers
(fn collect-digits [line]
  (let [digits (icollect [digit (string.gmatch line "%d")] digit)]
    (tonumber (.. (. digits 1) (last digits)))))

;; Given a piece of text, build a list of substrings (compensate for lack of fancy regex)
(fn substrings [x]
  (local acc [])
  (for [i 1 (length x)]
    (table.insert acc (string.sub x i (length x))))
  acc)

;; Given a piece of text, build substrings, then traverse those to find all "digits" (one, two, three, etc), and decimal numbers
(fn find-digits [text]
  (keep (fn [x]
          (let [first-character (string.sub x 1 1)]
            (if (tonumber first-character) first-character
                (. (icollect [k v (pairs string-to-number)]
                     (if (let [(s e) (string.find x k)] (= 1 s)) v nil))
                   1)))) (substrings text)))

;; Given a list of digits, build a new number from the first and last ones
(fn first-and-last [digits]
  (tonumber (.. (. digits 1) (last digits))))

(print :Task :Answer)
(print "Test 1" (->> test-data
                     (split "\n")
                     (keep collect-digits)
                     sum))

(print "Test 2" (->> test-data2
                     (split "\n")
                     (keep find-digits)
                     (keep first-and-last)
                     sum))

(local real-data (slurp :input.txt))
(print "Real 1" (->> real-data
                     (split "\n")
                     (remove empty?)
                     (keep collect-digits)
                     sum))

(print "Test 2" (->> real-data
                     (split "\n")
                     (remove empty?)
                     (keep find-digits)
                     (keep first-and-last)
                     sum))

