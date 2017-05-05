;; -*- mode: lisp; package: link-grammar; fill-column: 80 -*-

(in-package :link-grammar)

;; * special variables

(defvar *dictionary* nil)
(defvar *options* nil)
(defvar *sentence* nil)
(defvar *linkage* nil)

;; * sentence definition
;; ** slot documentation

(defgeneric len (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of words in the tokenized sentence, including the
    boundary words and punctuation.}
    @short{There needs to be an example here}
    @see{words}"))

(defgeneric null-count (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of null links that were used in parsing the sentence.}
    @short{Null-links are usually an indicator that the parsing misidentified
      something about the sentence.  Thus higher null-count would call indicate
      either a parsing problem, or that a sentence was originally
      ungrammatical.}"))

(defgeneric num-linkages-found (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The total number of linkages found when parsing the sentence.}
    @short{This also includes linkages which required post-processor attention
      and those which didn't.}
    @see{num-valid-linkages}
    @see{num-linkages-postprocessed}"))

(defgeneric num-valid-linkages (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of linkages found while parsing the sentence, which were
      unaltered during post-processing.}
    @short{The last step at parsing the sentence is the post-processing.  During
      this step linkages may be altered.  This counts only those linkages, which
      weren't altered by post-processor.}"))

(defgeneric num-linkages-post-processed (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of linkages altered by the post-processing parsing step.}
    @short{This counts the linkages which had to be altered by the post-processor.
      This is not necessary an indication of a problem with a sentence, however an
      ungrammatic sentence is more likely to produce a higher count of
      post-processor corrections.}"))

;; ** defclass sentence

(defclass sentence (standard-object)
  ((handle :initarg :handle :accessor handle)
   (input :initarg :input :initform nil :reader input)
   (len :reader len)
   (null-count :reader null-count)
   (num-linkages-found :reader num-linkages-found)
   (num-valid-linkages :reader num-valid-linkages)
   (num-linkages-post-processed :reader num-linkages-post-processed)))

;; ** constructor

(defmethod initialize-instance :around ((this sentence) &rest initargs)
  (destructuring-bind (&key input) initargs
    (let ((hd (handle *dictionary*))
          (ho (handle *options*)))
      (assert (and (not (null hd)) (not (null hd))) (hd ho)
              "Dictionary: ~s and options: ~s must be initialized")
      (let ((handle (sentence_create input hd)))
        (if (cffi:null-pointer-p handle)
            (error "Couldn't create sentence: ~s" input))
        (setf (slot-value this 'handle) handle
              (slot-value this 'input) input)
        (ecase (sentence_split handle ho)
          (-1 (error "Couldn't tokenize sentence: ~s" input))
          (-2 (error "Sentence has unknown words: ~s" input))
          (-3 (error "Couldn't flatten word graph: ~s" input))
          (0))
        (let ((nlinks (sentence_parse handle ho)))
          (case nlinks
            ;; this shouldn't happen because we certainly
            ;; called `sentence_split'
            (-1 (error "Couldn't split sentence: ~s" input))
            (-2 (error "Sentence is too long: ~s" input))
            (otherwise
             (setf (slot-value this 'num-valid-linkages) nlinks))))))
    (call-next-method)))

(defun make-sentence (input)
  (let ((result (make-instance 'sentence :input input)))
    (when (cffi:null-pointer-p (handle result))
      (error "Couldn't create sentence"))
    (tg:finalize
     result (lambda () (sentence_delete (handle result))))))

;; * linkage definition
;; ** slot documentation

(defgeneric sentence (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{The @class{sentence} to which this linkage belongs.}
    @short{Once sentence is parsed successfully, there will be multiple
      @class{linkage} objects associated with the words of the sentence.  This
      is a reference back to the sentence which created this linkage.}"))

(defgeneric index (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{The index at which this linkage was generated.}"))

(defun num-words (link)
   "@arg[link]{a @class{linkage}}
    @returns{The number of words in the @code{sentence}.}
    @short{The number of words in the sentence for which this is a linkage. Note
      that this function does @em{not} return the number of words used in the
      current sublinkage.}"
   (linkage_get_num_words (handle link)))

(defun num-links (link)
  "@arg[link]{a @class{linkage}}
   @returns{The number of links used in the current sublinkage.}
   @short{The number of links used in the current sublinkage.}"
  (linkage_get_num_links (handle link)))

(defun words (link)
  "@arg[link]{a @class{linkage}}
   @returns{A list of inflected words of the sublinkage.}
   @short{Returns a list of word spellings for the current sublinkage. These
      are the @code{\"inflected\"} spellings, such as @code{\"dog.n\"}. The
      original spellings can be obtained by calls to @code{(word sent
      wordnum)}.}"
  (iter
    (with h := (handle link))
    (with words := (linkage_get_words h))
    (for i :below (linkage_get_num_words h))
    (collect (mem-aref words :string i))))

(defun unused-word-cost (link)
  "@arg[link]{a @class{linkage}}
   @returns{A parameter used in post-processing.}
   @short{For more information see the dictionary documentation and source
      code.}"
  (linkage_unused_word_cost (handle link)))

(defun link-cost (link)
  "@arg[instance]{a @class{linkage}}
   @returns{A parameter used in post-processing.}
   @short{For more information see the dictionary documentation and source
      code.}"
  (linkage_link_cost (handle link)))

(defun corpus-cost (link)
  "@arg[instance]{a @class{linkage}}
   @returns{The total cost of this particular linkage, based on the cost of
      disjuncts stored in the corpus-statistics database.}
   @short{For more information see the dictionary documentation and source
      code.}"
   (linkage_corpus_cost (handle link)))

(defun violation-name (link)
  "@arg[instance]{a @class{linkage}}
   @returns{A name of the violated rule as specified in the post-process
      knowledge file.}
   @short{If the linkage violated any post-processing rules, this slot will
      contain the name of the violated rule in the post-process knowledge
      file.}"
   (linkage_get_violation_name (handle link)))

;; TODO: There are more linkage methods

;; ** defclass linkage

(defclass linkage (standard-object)
  ((handle :reader handle)
   (sentence :reader sentence)
   (index :initarg :index :reader index))
  (:documentation
   "@short{This class maps to @code{Linkage} C++ class.}

    @see-slot{sentence}
    @see-slot{num-words}
    @see-slot{num-links}
    @see-slot{words}
    @see-slot{unused-word-cost}
    @see-slot{disjunct-cost}
    @see-slot{link-cost}
    @see-slot{corpus-cost}
    @see-slot{violation-name}
"))

;; ** constructor

(defun make-linkage (&optional (index 0))
  (let ((result (make-instance 'linkage :index index)))
    (tg:finalize
     result (lambda () (linkage_delete (handle result))))))

(defmethod initialize-instance :around ((this linkage) &rest initargs)
  (destructuring-bind (&key index) initargs
    (let ((hs (handle *sentence*))
          (ho (handle *options*)))
      (assert (and (not (null hs)) (not (null ho))) (hs ho)
              "Sentence: ~s and options ~s must be initialized")
      (let ((handle (linkage_create index hs ho)))
        (when (cffi:null-pointer-p handle)
          (error "Couldn't create linkage for sentence ~s and options ~s"
                 *sentence* *options*))
        (setf (slot-value this 'handle) handle
              (slot-value this 'index) index)))))

;; * dictionary definition

;; ** defclass dictionary
(defclass dictionary (standard-object)
  ((handle :initarg :handle :accessor handle)
   (language :initarg :language :reader language)))

;; * constructor

(defun make-dictionary (&optional (language "en"))
  (let ((result (make-instance 'dictionary :language language))
        (handle (dictionary_create_lang language)))
    (when (cffi:null-pointer-p handle)
      (error "Couldn't create dictionary"))
    (setf (slot-value result 'handle) handle)
    (tg:finalize
     result (lambda () (dictionary_delete (handle result))))))

;; * parse-options definition
;; ** slot documentation

(defun verbosity (opts)
  "@arg[instance]{a @class{parse-options}}
   @return{the verbosity level of the parser, higher level means
      that the parser will be more verbose}
   @short{Default value is 0, seems like 2 is the highest value.}"
  (parse_options_get_verbosity (handle opts)))

(defun verbosity-set (opts v)
  (parse_options_set_verbosity (handle opts) v))

(defsetf verbosity verbosity-set)

(defun linkage-limit (opts)
   "@arg[instance]{a @class{parse-options}}
    @return{The maximum number of links a parser will try to find in
      a sentence it parses.}
    @short{Default value is 10000}

    This parameter determines the @em{maximum} number of linkages that
    are considered in post-processing. If more than
    @code{linkage-limit} linkages found, then a random sample of
    @code{linkage-limit} is chosen for post-processing. When this
    happen a warning is displayed at verbosity levels bigger than 1."
   (parse_options_get_linkage_limit (handle opts)))

(defun linkage-limit-set (opts lim)
  (parse_options_set_linkage_limit (handle opts) lim))

(defsetf linkage-limit linkage-limit-set)

(defun disjunct-cost (opts)
   "@arg[instance]{a @class{parse-options} or a @class{linkage}}
    @return{If instance is a @class{parse-options} -- the maximum
      number of links a parser will try to find in a sentence it
      parses. If instance is a @class{linkage} -- a parameter used in
      post-processing.}
    @short{Determines the maximum disjunct cost used during
    parsing, where the cost of a disjunct is equal to the maximum cost
    of all of its connectors. The default is that all disjuncts, no
    matter what their cost, are considered.

    For more information see the dictionary documentation and source code.}"
   (parse_options_get_disjunct_cost (handle opts)))

(defun disjunct-cost-set (opts cost)
  (parse_options_set_disjunct_cost (handle opts) cost))

(defsetf disjunct-cost disjunct-cost-set)

(defun max-null-count (opts)
  "@arg[instance]{a @class{parse-options}}
    @return{The maximum number of links a parser will try to find in
      a sentence it parses.}
    @short{Default value is 0}

    This determines the maximum number of null links that
    a parse might have. A call to @fun{parse} will find all
    linkages having the minimum number of null links within the range
    specified by this parameter in the @class{parse-options}.

    @see-slot{min-null-count}"
  (parse_options_get_max_null_count (handle opts)))

(defun max-null-count-set (opts count)
  (parse_options_set_max_null_count (handle opts) count))

(defsetf max-null-count max-null-count-set)

(defun min-null-count (opts)
   "@arg[instance]{a @class{parse-options}}
    @return{The lower bound on the range set by @code{max-null-count}.}
    @short{Default value is 0}

    @see-slot{max-null-count}"
   (parse_options_get_min_null_count (handle opts)))

(defun min-null-count-set (opts count)
  (parse_options_set_min_null_count (handle opts) count))

(defsetf min-null-count min-null-count-set)

(defun islandsp (opts)
   "@arg[instance]{a @class{parse-options}}
    @return{@code{T} if parser is allowed to parse disconnected links.}
    @short{Default value is nil}

    This option determines whether or not \"islands\" of links are
    allowed. For example, the following linkage has an island:

@begin{pre}
    +------Wd-----+
    |     +--Dsu--+---Ss--+-Paf-+     +--Dsu--+---Ss--+--Pa-+
    |     |       |       |     |     |       |       |     |
  ///// this sentence.n is.v false.a this sentence.n is.v true.a
@end{pre}

    I.e. islands are the connected components of the link graph."
   (/= (parse_options_get_islands_ok (handle opts)) 0))

(defun islandsp-set (opts ok)
  (parse_options_set_islands_ok (handle opts) (if ok 1 0)))

(defsetf islandsp islandsp-set)

(defun short-length (opts)
   "@arg[instance]{a @class{parse-options}}
    @return{The lenght of the longest allowed links.}
    @short{This parameter determines how long the links are allowed to
    be. The intended use of this is to speed up parsing by not
    considering very long links for most connectors, since they are
    very rarely used in a correct parse. An entry for
    @code{UNLIMITED-CONNECTORS} in the dictionary will specify which
    connectors are exempt from the length limit.}"
   (parse_options_get_short_length (handle opts)))

(defun short-length-set (opts len)
  (parse_options_set_short_length (handle opts) len))

(defsetf short-length short-length-set)

(defun max-memory (opts)
   "@arg[instance]{a @class{parse-options}}
    @return{The memory limit for the parser (in bytes?).}
    @short{Determines the maximum memory allowed during parsing. This is used
    just as @code{max-parse-time} is, so that the parsing process is
    terminated as quickly as possible after the total memory
    (including that allocated to all dictionaries, etc.) exceeds the
    maximum allowed.}

    @see-slot{max-parse-time}"
   (parse_options_get_max_memory (handle opts)))

(defun max-memory-set (opts mem)
  (parse_options_set_max_memory (handle opts) mem))

(defsetf max-memory max-memory-set)

(defun max-parse-time (opts)
   "@arg[instance]{a @class{parse-options}}
    @return{The time to strart to wrap up the parsing (in milliseconds?).}
    @short{Determines the @em{approximate} maximum time that parsing is
    allowed to take. The way it works is that after this time has
    expired, the parsing process is artificially forced to complete
    quickly by pretending that no further solutions (entries in the
    hash table) can be constructed. The actual parsing time might be
    slightly longer.}"
   (parse_options_get_max_parse_time (handle opts)))

(defun max-parse-time-set (opts time)
  (parse_options_set_max_parse_time (handle opts) time))

(defsetf max-parse-time max-parse-time-set)

;; (defun cost-model-type (opts)
;;    "@arg[instance]{a @class{parse-options}}
;;     @return{The type of the model used by the parser (I think it's a string).}
;;     @short{The cost model type for ranking linkages. Currently, there are two
;;     models: VDAL (1) and CORPUS (2). The VDAL model ranks parses from
;;     lowest to highest cost in and-cost, disjunct-cost,
;;     unused-word-cost and structure-violations-cost. The CORPUS model
;;     ranks parses according to the frequency of use of disjuncts, based
;;     on a statistical analysis of a collection of texts.}"
;;    (parse_options_get_cost_model_type (handle opts)))

;; (defun cost-model-type-set (opts type)
;;   (parse_options_set_cost_model_type (handle opts) type))

;; (defsetf cost-model-type cost-model-type-set)

(defun all-short-connectors-p (opts)
   "@arg[instance]{a @class{parse-options}}
    @return{@code{T} if there is a limit on how far connectors may be.}
    @short{If true, then all connectors have length restrictions imposed on
    them -- they can be no farther than @code{short-length} apart. This is
    used when parsing in \"panic\" mode, for example.}"
   (/= (parse_options_get_all_short_connectors (handle opts)) 0))

(defun all-short-connectors-p-set (opts ok)
  (parse_options_set_all_short_connectors (handle opts) (if ok 1 0)))

(defsetf all-short-connectors-p all-short-connectors-p-set)

(defun spell-guess (opts)
  (parse_options_get_spell_guess (handle opts)))

(defun spell-guess-set (opts val)
  (parse_options_set_spell_guess (handle opts) val))

(defsetf spell-guess spell-guess-set)

(defun display-morphology-p (opts)
  (/= (parse_options_get_display_morphology (handle opts)) 0))

(defun display-morphology-p-set (opts ok)
  (parse_options_set_display_morphology (handle opts) (if ok 1 0)))

(defsetf display-morphology-p display-morphology-p-set)

;; TODO: There are more fields in options

;; ** defclass parse-options

(defclass parse-options (standard-object)
  ((handle :reader handle))
  (:documentation
   "@short{This class maps to @code{Parse_Options} C++ class.}

    @see-slot{verbosity}
    @see-slot{linkage-limit}
    @see-slot{disjunct-cost}
    @see-slot{nim-null-count}
    @see-slot{max-null-count}
    @see-slot{islandsp}
    @see-slot{short-length}
    @see-slot{max-memory}
    @see-slot{max-parse-time}
    @see-slot{cost-model-type}
    @see-slot{display-morphology-p}
    @see-slot{spell-guess}
    @see-slot{all-short-connectors-p}
"))

;; ** constructor

(defun make-parse-options (&key
                             (verbosity 1)
                             (linkage-limit 100)
                             (disjunct-cost 2.7d0)
                             (min-null-count 0)
                             (max-null-count 0)
                             (short-length 16)
                             (max-memory -1)
                             (max-parse-time -1)
                             (spell-guess 7)
                             display-morphology-p
                             islandsp
                             all-short-connectors-p)
  ;; I don't think we can set / use cost-model-type because
  ;; it is defined during C compilation.
  ;; use sat solver = false
  ;; use vitebri = false
  ;; use spell guess = 7 (if has hunspell / aspell)
  ;; perform pp prune = true
  ;; twopwas length = 30
  ;; repeatable rand = true
  ;; use cluster disjuncts = false
  (let ((handle (parse_options_create)))
    (when (cffi:null-pointer-p handle)
      (error "Couldn't create parse-options"))
    (let ((result (make-instance 'parse-options)))
      (setf (slot-value result 'handle) handle
            (verbosity result) verbosity
            (linkage-limit result) linkage-limit
            (disjunct-cost result) disjunct-cost
            (min-null-count result) min-null-count
            (max-null-count result) max-null-count
            (islandsp result) islandsp
            (short-length result) short-length
            (max-memory result) max-memory
            (max-parse-time result) max-parse-time
            (display-morphology-p result) display-morphology-p
            (spell-guess result) spell-guess
            (all-short-connectors-p result) all-short-connectors-p)
      (tg:finalize
       result (lambda () (parse_options_delete (handle result)))))))

;; * utility macros

(defun data-dir () (dictionary_get_data_dir))

(defmacro with-dictionary ((dictionary &optional language data-dir) &body body)
  "@arg[dictionary]{A symbol to bind to the @class{dictionary}, in addition to
      this symbol *dictionary* special variable will be also bound to the
      currently active dictionary}
   @arg[language]{Language code (optional)}
   @arg[data-dir]{Directory to look up for dictionary files (optional)}
   @arg[body]{Forms to bind the @code{dictionary} in}
   @return{The result of the last form of the @code{body}}

   @short{Use this macro to create and manage @class{dictionary} objects.}

   @see{dictionary}"
  (alexandria:with-gensyms (dir lang)
    `(let ((,lang ,language) ,dictionary)
       (unwind-protect
            (let ((,dir ,data-dir))
              (when ,dir
                (dictionary_set_data_dir
                 (translate-logical-pathname ,dir)))
              (setf ,dictionary
                    (make-instance
                     'dictionary
                     :handle
                     (if ,lang
                         (dictionary_create_lang ,lang)
                         (dictionary_create_default_lang)))
                    *dictionary* ,dictionary)
              ,@body)
         (when ,dictionary
           (dictionary_delete (handle ,dictionary))
           (setf *dictionary* nil))))))

(defmacro with-sentence ((input) &body body)
  `(let ((*sentence* (make-sentence ,input)))
     ,@body))

(defmacro with-linkage ((&optional (index 0)) &body body)
  `(let ((*linkage* (make-linkage ,index)))
     ,@body))

(defmacro with-options ((&optional opts) &body body)
  `(let ((*options* (or ,opts (make-parse-options))))
     ,@body))

;; * utility methods

(defmethod timer-expired-p ((this parse-options))
  (parse_options_timer_expired (handle this)))

(defmethod memory-exhausted-p ((this parse-options))
  (parse_options_memory_exhausted (handle this)))

(defmethod resource-exhausted-p ((this parse-options))
  (parse_options_resources_exhausted (handle this)))

(defmethod reset-resources ((this parse-options))
  (parse_options_reset_resources (handle this)))

(defmethod parse ((this sentence) (options parse-options))
  "Parses THIS sentence into a structured representation and returns
the number of linkages found in it."
  (sentence_parse (handle this) (handle options)))

(defmethod split ((this sentence) (options parse-options))
  "Parses THIS sentence into a structured representation and returns
the number of linkages found in it."
  (sentence_split (handle this) (handle options)))

(defmethod linkages ((this sentence) &key (linkage-type :none))
  (let ((handle (handle this)))
    (ecase linkage-type
      (:none (sentence_num_linkages_found handle))
      (:valid (sentence_num_valid_linkages handle))
      (:post-processed (sentence_num_linkages_post_processed handle)))))

(defmethod num-violations ((this sentence) index)
  (sentence_num_violations (handle this) index))

(defmethod nth-disjunct-cost ((this sentence) index)
  (sentence_disjunct_cost (handle this) index))

(defmethod nth-link-cost ((this sentence) index)
  (sentence_link_cost (handle this) index))

(defmethod left-word ((this linkage) index)
  (linkage_get_link_lword (handle this) index))

(defmethod right-word ((this linkage) index)
  (linkage_get_link_rword (handle this) index))

(defmethod link-length ((this linkage) index)
  (linkage_get_link_length (handle this) index))

(defmethod link-label ((this linkage) index &key (direction :none))
  (ecase direction
    (:none (linkage_get_link_label (handle this) index))
    (:left (linkage_get_link_llabel (handle this) index))
    (:right (linkage_get_link_rlabel (handle this) index))))

(defmethod num-domains ((this linkage) index)
  (linkage_get_link_num_domains (handle this) index))

(defmethod domain-names ((this linkage) index)
  (linkage_get_link_domain_names (handle this) index))

;; * printing

(defmethod print-object ((this dictionary) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (format stream "~A" (dictionary_get_lang (handle this))))
  this)

(defmethod print-object ((this parse-options) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (format stream "{~{~A: ~A~^, ~}}"
            (list :verbosity (verbosity this)
                  :linkage-limit (linkage-limit this)
                  :disjunct-cost (disjunct-cost this)
                  :min-null-count (min-null-count this)
                  :max-null-count (max-null-count this)
                  :islandsp (islandsp this)
                  :short-length (short-length this)
                  :max-memory (max-memory this)
                  :max-parse-time (max-parse-time this)
                  :display-morphology-p (display-morphology-p this)
                  :spell-guess (spell-guess this)
                  :all-short-connectors-p (all-short-connectors-p this))))
  this)

(defmethod print-object ((this sentence) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (if (slot-boundp this 'input)
        (format stream "~A" (input this))
        (format stream "sentence was not created"))))

(defmethod print-object ((this linkage) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (format stream "~{~A~^, ~}" (words this)))
  this)

(defmethod print-diagram ((this linkage)
                          &optional display-wallsp
                            (screen-wdith (linkage-print-length this)))
  "Creates a diagram which shows the parse of THIS linkage.
DISPLAY-WALLSP if true, will instruct to print the linkage margins (walls).
SCREEN-WDITH by default, will try to guess the space needed to print the linkage."
  (linkage_print_diagram
   (handle this)
   (if display-wallsp 1 0)
   screen-wdith))

(defmethod linkage-print-length ((this linkage))
  (iter
    (for word :in (words this))
    (summing (1+ (length word)) :into result)
    (finally (return (1- result)))))

(defmethod print-postscript ((this linkage)
                             &optional display-wallsp
                               (screen-wdith (linkage-print-length this)))
  (linkage_print_postscript (handle this) display-wallsp screen-wdith))

(defmethod print-links-and-domains ((this linkage))
  (linkage_print_links_and_domains (handle this)))

(defmethod print-senses ((this linkage))
  (linkage_print_senses (handle this)))

;; TODO: Investigate what does this tree look like and how exactly am I supposed
;; to free it.

(defmethod print-constituent-tree ((this linkage) &key (mode :no-display))
  "The function linkage_constituent_tree returns a pointer to a tree; after
using it the space should be freed-up with a call to
linkage_free_constituent_tree."
  (linkage_print_constituent_tree
   (handle this)
   (cffi:foreign-enum-value 'ConstituentDisplayStyle mode)))

