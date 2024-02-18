;;; lisp/hy.el -*- lexical-binding: t; -*-

(defconst hy-font-lock--hy-builtins
  '("*map"
    "accumulate"
    "assoc"
    "butlast"
    "calling-module-name"
    "chain"
    "coll?"
    "combinations"
    "comp"
    "complement"
    "compress"
    "constantly"
    "count"
    "cut"
    "cycle"
    "dec"
    "defmain"
    "del"
    "disassemble"
    "distinct"
    "doto"
    "drop"
    "drop-last"
    "drop-while"
    "empty?"
    "even?"
    "every?"
    "filter"
    "first"
    "flatten"
    "float?"
    "fraction"
    "gensym"
    "get"
    "group-by"
    "identity"
    "inc"
    "instance?"
    "integer"
    "integer-char?"
    "integer?"
    "interleave"
    "interpose"
    "is"
    "is-not"
    "islice"
    "iterable?"
    "iterate"
    "iterator?"
    "juxt"
    "keyword"
    "keyword?"
    "last"
    "macroexpand"
    "macroexpand-1"
    "merge-with"
    "multicombinations"
    "name"
    "neg?"
    "none?"
    "nth"
    "numeric?"
    "odd?"
    "partition"
    "permutations"
    "pos?"
    "product"
    "quasiquote"
    "quote"
    "read"
    "read-str"
    "reduce"
    "remove"
    "repeat"
    "repeatedly"
    "rest"
    "second"
    "setv"
    "some"
    "string"
    "string?"
    "symbol?"
    "take"
    "take-nth"
    "take-while"
    "tee"
    "unquote"
    "unquote-splice"
    "xor"
    "zero?"
    "zip"
    "zip-longest"
    "--macros--" "__macros__")
  "Hy-only builtin names.")

;;; Python Builtins
(defconst hy-font-lock--python-builtins
  '("abs"
    "all"
    "any"
    "ascii"
    "bytes"
    "bin"
    "bool"
    "bytearray"
    "callable"
    "chr"
    "compile"
    "complex"
    "delattr"
    "dict"
    "dir"
    "divmod"
    "enumerate"
    "eval"
    "exec"
    "float"
    "format"
    "frozenset"
    "getattr"
    "globals"
    "hasattr"
    "hash"
    "help"
    "hex"
    "id"
    "input"
    "int"
    "isinstance"
    "issubclass"
    "iter"
    "len"
    "list"
    "locals"
    "map"
    "max"
    "memoryview"
    "min"
    "next"
    "object"
    "oct"
    "open"
    "ord"
    "pow"
    "range"
    "repr"
    "reversed"
    "round"
    "set"
    "setattr"
    "slice"
    "sorted"
    "str"
    "sum"
    "super"
    "tuple"
    "type"
    "vars"
    "--package--" "__package__"
    "--import--" "__import__"
    "--all--" "__all__"
    "--doc--" "__doc__"
    "--name--" "__name__")
  "Builtin names available in Python normally.")

;;; Constants
(defconst hy-font-lock--constants
  '("True"
    "False"
    "None"
    "Ellipsis"
    "NotImplemented"
    "nil"  ; Provided for those that alias None as nil, not a part of Hy
    )
  "Constant names in Hy.")

;;; Exceptions
(defconst hy-font-lock--exceptions
  '("ArithmeticError" "AssertionError" "AttributeError" "BaseException"
    "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
    "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError" "ImportError"
    "ImportWarning" "IndexError" "KeyError" "KeyboardInterrupt" "LookupError"
    "MemoryError" "NameError" "NotImplementedError" "OSError" "OverflowError"
    "PendingDeprecationWarning" "ReferenceError" "RuntimeError" "RuntimeWarning"
    "StopIteration" "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit"
    "TypeError" "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
    "UnicodeError" "UnicodeTranslateError" "UnicodeWarning" "UserWarning"
    "VMSError" "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
    "BufferError" "BytesWarning" "IndentationError" "ResourceWarning"
    "TabError")
  "Exception and error names.")

;;; Definitions
(defconst hy-font-lock--definitions
  '(;; Functions
    "defn" "defn/a"
    ;; Macros
    "defmacro" "defmacro/g!" "defmacro!"
    ;; Tag Macros
    "deftag"
    ;; Defining __main__
    "defmain"
    ;; Multi-methods
    "defmulti" "defmethod")
  "Names in Hy that define functions, macros, etc.")

;;; Operators
(defconst hy-font-lock--operators
  '("!=" "%" "%=" "&" "&=" "*" "**" "**=" "*=" "+" "+=" "," "-"
    "-=" "/" "//" "//=" "/=" "<" "<<" "<<=" "<=" "=" ">" ">=" ">>" ">>="
    "^" "^=" "|" "|=" "~")
  "Operators in Hy.")

;;; Special Names
(defconst hy-font-lock--special-names
  '(;; Looping
    "for" "for/a"
    "dfor" "lfor" "sfor"  ; comprehensions
    "loop" "recur"  ; hy.contrib.loop
    ;; Threading
    "->" "->>" "as->"
    ;; Flow control
    "return"
    "if" "if*" "if-not" "lif" "lif-not"
    "else" "unless" "when"
    "break" "continue" "while"
    "cond"
    "do"
    ;; Functional
    "fn" "fn/a"
    "await"
    "yield" "yield-from"
    "with" "with*" "with/a" "with/a*"
    "with-gensyms"
    ;; Error Handling
    "except" "try" "throw" "raise" "catch" "finally" "assert"
    ;; Python builtins (and shadowed calls to builtins)
    "print"
    "not" "and" "or"
    "in" "not-in"
    ;; Namespaces
    "global" "nonlocal"
    ;; Evaluation
    "eval" "eval-and-compile" "eval-when-compile")
  "Special names like compiler stuff to highlight as keywords.")

(defconst hy-font-lock--kwds-builtins
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--hy-builtins
            ,@hy-font-lock--python-builtins
            ,@hy-font-lock--operators)
        symbol-end))

   '(0 font-lock-builtin-face))
  "Hy builtin keywords.")

(defconst hy-font-lock--kwds-constants
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--constants)
        symbol-end))

   '(0 font-lock-constant-face))
  "Hy constant keywords.")

(defconst hy-font-lock--kwds-definitions
  (list
   (rx-to-string
    `(: "("
        symbol-start
        (group-n 1 (or ,@hy-font-lock--definitions))
        (1+ space)
        (group-n 2 (1+ word))))

   '(1 font-lock-keyword-face)
   '(2 font-lock-function-name-face nil t))
  "Hy definition keywords.")

(defconst hy-font-lock--kwds-exceptions
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--exceptions)
        symbol-end))

   '(0 font-lock-type-face))
  "Hy exception keywords.")

(defconst hy-font-lock--kwds-special-names
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@hy-font-lock--special-names)
        symbol-end))

   '(0 font-lock-keyword-face))
  "Hy special names keywords.")

(defconst hy-font-lock-kwds
  (list hy-font-lock--kwds-builtins
        hy-font-lock--kwds-constants
        hy-font-lock--kwds-definitions
        hy-font-lock--kwds-exceptions
        hy-font-lock--kwds-special-names)
  "All Hy font lock keywords.")

(defun hy-mode--setup-font-lock ()
  "Setup `font-lock-defaults' and others for `hy-mode.'"
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(hy-font-lock-kwds)))

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."
  (hy-mode--setup-font-lock))
