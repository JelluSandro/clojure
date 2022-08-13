(defn constant [x] (fn [mp] (+ x)))
(defn variable [x] (fn [mp] (mp x)))
(defn forge [op] (fn ([& x]
                      (fn [mp]
                        (apply op
                               (mapv (fn [b] (b mp)) x))))))
(defn divide-no-exception ([xx & x] (reduce (fn [a b] (/ (double a) (double b))) xx x))
  ([xx] (/ 1 (double xx))))
(def add (forge +))
(def subtract (forge -))
(def multiply (forge *))
(def divide (forge divide-no-exception))

(def negate subtract)
(defn aver [& x] (/ (apply + x) (count x)))
(defn mean [& x] (fn [mp] (apply aver (mapv (fn [b] (b mp)) x))))
(defn square [x] (* x x))

(defn varn [& x] (fn [mp] (let [input (mapv (fn [b] (b mp)) x)]
                            (let [av (apply aver input)]
                              (apply aver (mapv (fn [b] (square (- b av))) input))))))

;Homework10


(load-file "proto.clj")
(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))
(def _val (field :val))
(def _var (field :var))
(def _args (field :args))
(declare zero)
(def Constant (constructor
                (fn [this x] (assoc this :val x))
                {:evaluate (fn [this mp] (_val this))
                 :diff     (fn [this x] zero)
                 :toString #(format "%.1f" (double (_val %)))}))
(def zero (Constant 0))
(def one (Constant 1))
(def Variable (constructor
                (fn [this x] (assoc this :var x))
                {:evaluate (fn [this mp] (mp (_var this)))
                 :diff     (fn [this x] (if (= x (_var this)) one zero))
                 :toString #(_var %)}))
;(defn cal [op] (fn [this mp] (apply op (mapv (fn [b] (evaluate b mp)) (_args this)))))
(defn Forge [cal sym dif]
  (constructor
    (fn [this & args] (assoc this :args args))
    {:evaluate (fn [this mp] (apply cal (mapv (fn [b] (evaluate b mp)) (_args this))))
     :diff     dif
     :toString #(str "(" sym
                     (if (= 0 (count (_args %)))
                       " "
                       (apply str (mapv (fn [b] (str " " (toString b))) (_args %))))
                     ")")}))
(defn fabric [this x clas] (apply clas (mapv (fn [b] (diff b x)) (_args this))))
(def Add (Forge + "+" (fn[this x] (fabric this x Add))))
(def Subtract (Forge - "-" (fn[this x] (fabric this x Subtract))))
(def Negate (Forge - "negate" (fn[this x] (fabric this x Negate))))
(def Multiply
  (Forge * "*"
         (fn [this x]
           (let [vec (_args this)]
             (let [arg-fir (first vec)]
               (if (= 1 (count vec))
                 (diff arg-fir x)
                 (let [arg-res (apply Multiply (rest vec))]
                   (Add (Multiply (diff arg-fir x) arg-res)
                        (Multiply arg-fir (diff arg-res x))))))))))
(def Squre (Forge (fn [vec] (* vec vec))
                  "Squre"
                  (fn [this x] (diff (Multiply (first (_args this)) (first (_args this))) x))))
(def Divide
  (Forge divide-no-exception "/"
         (fn [this x]
           (let [vec (_args this)]
             (let [arg-fir (first vec)]
               (if (= 1 (count vec))
                 (Divide (Negate (diff arg-fir x)) (Squre arg-fir))
                 (let [arg-res (apply Multiply (rest vec))]
                   (Divide (Subtract (Multiply (diff arg-fir x) arg-res)
                                     (Multiply arg-fir (diff arg-res x)))
                           (Squre arg-res)))))))))

(def Sign (Forge (fn [ans]
                   (cond
                     (< ans 0) -1
                     (> ans 0) 1
                     :else 0))
                 "Sign"
                 (fn [this x] zero)))
(def Abs (Forge (fn [ans] (Math/abs ans))
                "abs"
                (fn [this x] (let [ans (first (_args this))]
                               (Multiply (Sign ans) (diff ans x))))))

(def ArithMean (Forge (fn [& args]
                        (divide-no-exception
                          (apply + args)
                          (count args)))
                      "arith-mean"
                      (fn [this x]
                        (let [vec (_args this)]
                          (diff
                            (Divide
                              (apply Add vec)
                              (Constant (count vec))) x)))))
(def GeomMean (Forge (fn [& args]
                         (Math/pow
                           (Math/abs (apply * args))
                           (divide-no-exception (count args))))
                     "geom-mean"
                     (fn [this x]
                       (let [vec (_args this)]
                         (let [len (count vec)]
                           (apply Divide
                                  (diff (Abs (apply Multiply vec)) x)
                                  (Constant len)
                                  (repeat (- len 1) this)))))))
(def HarmMean (Forge (fn [& args]
                         (divide-no-exception
                           (count args)
                           (apply + (mapv (fn [b] (divide-no-exception 1 b)) args))))
                     "harm-mean"
                     (fn [this x]
                       (let [vec (_args this)]
                         (diff
                           (Divide
                             (Constant (count vec))
                             (apply Add (mapv (fn [b] (Divide one b)) vec))) x)))))

(def obj-oper {'+          Add
               '-          Subtract
               '*          Multiply
               '/          Divide
               'negate     Negate
               'arith-mean ArithMean
               'geom-mean  GeomMean
               'harm-mean  HarmMean
               })
(def oper {'+      add
           '-      subtract
           '*      multiply
           '/      divide
           'negate negate
           'mean   mean
           'varn   varn
           })
(defn forge-parse [arr-op con vari]
  (fn pp [input] (if (coll? input)
                   (apply (arr-op (first input)) (mapv pp (rest input)))
                   (if (number? input) (con input) (vari (name input))))))
(def parse-obj (forge-parse obj-oper Constant Variable))
(def parse-exp (forge-parse oper constant variable))
(defn parseObject [input] (parse-obj (read-string input)))
(defn parseFunction [input] (parse-exp (read-string input)))



;Homework 11

