(ns object-system.core-test
  (:use [object-system.core] :reload-all)
  (:use [clojure.test]))

(deftest variable-test
  (let [x (variable)
	y (variable 3)
	z (variable {:a 1 :b 2})]
    (is (nil? (x)))
    (is (= 3 (y)))
    (is (= 3 (y :value)))
    (is (= 4 (y := 4)))
    (is (= 4 (y)))
    (is (= 5 (y := (+ 1 (y)))))
    (is (= 5 (y)))
    (is (= {:a 1 :b 2} (z)))
    (is (= {:a 1 :b 2 :c 3} (z := (assoc (z) :c 3))))
    (is (= {:a 1 :b 2 :c 3} (z)))
    (is (= {:a 1 :b 2 :c 3} (z :value)))
    ))

(deftest find-class-test
  (let [object (find-class 'OBJECT)]
    (is (object :has? :superclass))
    (is (object :has? :class))
    (is (= (object :superclass)
	   None))
    (is (= (object :class)
	   'OBJECT))))

(deftest implementation=>object-test
  (let [implementation-map {:class (variable 'OBJECT)
			    :name (variable "Chanwoo")
			    :introduce (fn [] (str "Hello. I'm " (self :name) "."))}
	object (implementation=>object implementation-map) ]
    (is (= (object :name)
	   "Chanwoo"))
    (is (= (object :introduce)
	   "Hello. I'm Chanwoo."))))

(deftest find-property-test
  (is (= ((find-property :superclass 'OBJECT))
	 None))
  (is (= ((find-property :class 'OBJECT))
	 'OBJECT))
  (is (fn? (find-property :has? 'OBJECT)))
  )

(deftest find-property-test-2
  (let [implementation-map {:class (variable 'OBJECT)
			    :name (variable "Chanwoo")
			    :introduce (fn [] (str "Hello. I'm " (self :name) "."))}
	object (implementation=>object implementation-map) ]
    (is (object :has? :name))
    (is (object :has? :introduce))
    (is (object :has? :class))))

(deftest object-test
  (is (OBJECT :has? :superclass))
  (is (OBJECT :has? :class))
  (is (= (OBJECT :superclass)
	 None))
  (is (= (OBJECT :class)
	 'OBJECT)))
    
(deftest set!-test
  (let [implementation-map {:class (variable 'OBJECT)
			    :name (variable "Chanwoo")
			    :introduce (fn [] (str "Hello. I'm " (self :name) "."))}
	object (implementation=>object implementation-map) ]
    (is (= (object :set! :name "Sory")
	   "Sory"))
    (is (= (object :name)
	   "Sory"))
    (is (= (object :introduce)
	   "Hello. I'm Sory."))))

(deftest understand?-test
  (let [implementation-map {:class (variable 'OBJECT)
			    :name (variable "Chanwoo")
			    :introduce (fn [] (str "Hello. I'm " (self :name) "."))}
	object (implementation=>object implementation-map) ]
    (is (object :understand? :understand?))
    (is (object :understand? :class))
    (is (object :understand? :set!))
    (is (object :understand? :introduce))
    (is (object :understand? :name))
    (is (not (object :understand? :as-your-wish)))
    (is (OBJECT :understand? :understand?))
    (is (OBJECT :understand? :class))
    (is (OBJECT :understand? :set!))
    (is (OBJECT :understand? :has?))
    (is (OBJECT :understand? :superclass))
    (is (OBJECT :understand? :properties))
    (is (not (OBJECT :understand? :study-hard)))
    ))

(deftest attributes=>variable-test
  (let [result (attributes=>variable {:class 'Person
				      :instance-variable-names [:name :age :gender]
				      :greet (fn []
					       "hello")
				      :introduce (fn []
						   (str "Hello. I'm " (self :name) "."))
				      :add (fn [a b]
					     (+ a b)) } ) ]
    (is (= ((result :class) :value)
	   'Person))))

;example of defining class
(def Person (OBJECT :subclass {:class 'Person
			       :instance-variable-names [:name :age :gender]
			       :greet (fn []
					"hello")
 			       :introduce (fn []
					    (str "Hello. I'm " (self :name) "."))
			       :add (fn [a b]
				      (+ a b))
			       :super-instance-variable-names (fn []
								(super :instance-variable-names))
			       } ))

(deftest subclass-test
  (is (= (Person :instance-variable-names)
	 [:name :age :gender]))
  (is (= (Person :class)
	 'Person))
  (is (= (Person :superclass)
	 'OBJECT))
  (is (= (Person :add 1 2)
	 3))
  (is (= (Person :greet)
	 "hello"))
  )

;example of instance creation
(deftest new-test
  (let [object (Person :new {:name "Chanwoo"
			     :age 31
			     :gender "male"}) ]
    (is (= (object :introduce)
	   "Hello. I'm Chanwoo."))
    (is (= (object :age)
	   31))
    (is (= (object :gender)
	   "male"))
    (is (= (object :instance-variable-names)
	   [:name :age :gender]))
    (is (object :understand? :greet))
    (is (object :understand? :superclass))
    (is (object :understand? :class))
    (is (object :has? :age))
    (is (not (object :has? :instance-variable-names)))
    (is (= (object :set! :age 28)
	   28))
    (is (= (object :age)
	   28))
    ))

(def Animal (OBJECT :subclass {:class 'Animal
			       :instance-variable-names [:race]
			       :sound (fn [] "grrrr...")}))

(deftest super-test
  (let [object (Person :new {:name "Chanwoo"
			     :age 31
			     :gender "male"}) ]
    (is (= (object :super-instance-variable-names)
	   []))))

(deftest find-class-test-2
  (let [object (Animal :new {:race "cat"})]
    (is (= (object :sound)
	   "grrrr..."))
    (is (object :understand? :properties))
    ))
	   




    
       