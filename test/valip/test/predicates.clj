(ns valip.test.predicates
  (:use valip.predicates
        valip.java.predicates)
  (:use clojure.test))

(deftest test-present?
  (is (not (present? nil)))
  (is (not (present? "")))
  (is (not (present? " ")))
  (is (present? "foo")))

(deftest test-matches
  (is ((matches #"...") "foo"))
  (is (not ((matches #"...") "foobar"))))

(deftest test-max-length
  (is ((max-length 5) "hello"))
  (is ((max-length 5) "hi"))
  (is (not ((max-length 5) "hello world"))))

(deftest test-max-length
  (is ((min-length 5) "hello"))
  (is ((min-length 5) "hello world"))
  (is (not ((min-length 5) "hi"))))

(deftest test-email-address?
  (is (email-address? "foo@example.com"))
  (is (email-address? "foo+bar@example.com"))
  (is (email-address? "foo-bar@example.com"))
  (is (email-address? "foo.bar@example.com"))
  (is (email-address? "foo@example.co.uk"))
  (is (not (email-address? "foo")))
  (is (not (email-address? "foo@bar")))
  (is (not (email-address? "foo bar@example.com")))
  (is (not (email-address? "foo@foo_bar.com"))))

(deftest test-valid-email-domain?
  (is (valid-email-domain? "example@google.com"))
  (is (not (valid-email-domain? "foo@example.com")))
  (is (not (valid-email-domain? "foo@google.com.nospam")))
  (is (not (valid-email-domain? "foo"))))

(deftest test-url?
  (is (url? "http://google.com"))
  (is (url? "http://foo"))
  (is (not (url? "foobar")))
  (is (not (url? ""))))

(deftest test-digits?
  (is (digits? "01234"))
  (is (not (digits? "04xa")))
  (is (not (digits? ""))))

(deftest test-integer-string?
  (is (integer-string? "10"))
  (is (integer-string? "-9"))
  (is (integer-string? "0"))
  (is (integer-string? "  8  "))
  (is (not (integer-string? "10,000")))
  (is (not (integer-string? "foo")))
  (is (not (integer-string? "10x")))
  (is (not (integer-string? "1.1")))
  (is (not (integer-string? ""))))

(deftest test-decimal-string?
  (is (decimal-string? "10"))
  (is (decimal-string? "-9"))
  (is (decimal-string? "0"))
  (is (decimal-string? "  8  "))
  (is (decimal-string? "1.1"))
  (is (decimal-string? "3.14159"))
  (is (not (decimal-string? "N")))
  (is (not (decimal-string? "M")))
  (is (decimal-string? "3N"))
  (is (decimal-string? "3M"))
  (is (not (decimal-string? "3.N")))
  (is (decimal-string? "3.M"))
  (is (not (decimal-string? "3.14159N")))
  (is (decimal-string? "3.14159M"))
  (is (not (decimal-string? "foo")))
  (is (not (decimal-string? "10x")))
  (is (not (decimal-string? ""))))

(deftest test-gt
  (is ((gt 10) "11"))
  (is (not ((gt 10) "9")))
  (is (not ((gt 10) "10")))
  (is (not ((gt 10) ""))))

(deftest test-gte
  (is ((gte 10) "11"))
  (is ((gte 10) "10"))
  (is (not ((gte 10) "9")))
  (is (not ((gte 10) ""))))

(deftest test-lt
  (is ((lt 10) "9"))
  (is (not ((lt 10) "11")))
  (is (not ((lt 10) "10")))
  (is (not ((lt 10) ""))))

(deftest test-lte
  (is ((lte 10) "9"))
  (is ((lte 10) "10"))
  (is (not ((lte 10) "11")))
  (is (not ((lte 10) ""))))

(deftest comparing-bigint-bigdec
  (are [input is-lt is-lte is-gte is-gt]
    (and (= is-lt  (boolean ((lt  10) input)))
         (= is-lte (boolean ((lte 10) input)))
         (= is-gte (boolean ((gte 10) input)))
         (= is-gt  (boolean ((gt  10) input))))
    "9"      true  true  false false
    "9N"     true  true  false false
    "9M"     true  true  false false
    "9."     true  true  false false
    "9.M"    true  true  false false
    "9.99"   true  true  false false
    "9.99M"  true  true  false false
    "10"     false true  true  false
    "10N"    false true  true  false
    "10M"    false true  true  false
    "10."    false true  true  false
    "10.M"   false true  true  false
    "10.0"   false true  true  false
    "10.0M"  false true  true  false
    "10.01"  false false true  true
    "10.01M" false false true  true
    "11"     false false true  true
    "11N"    false false true  true
    "11M"    false false true  true))

(deftest test-over
  (is (= over gt)))

(deftest test-under
  (is (= under lt)))

(deftest test-at-least
  (is (= at-least gte)))

(deftest test-at-most
  (is (= at-most lte)))

(deftest test-between
  (is ((between 1 10) "5"))
  (is ((between 1 10) "1"))
  (is ((between 1 10) "10"))
  (is (not ((between 1 10) "0")))
  (is (not ((between 1 10) "11")))
  (is (not ((between 1 10) ""))))
