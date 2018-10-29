;;; Port of https://gist.github.com/kseo/9383472

(ql:quickload :alexandria)
(declaim (optimize (speed 0) safety debug))
;; type term =
;;   | Ident of string
;;   | Lambda of string * term
;;   | Apply of term * term
;;   | Let of string * term * term
;;   | LetRec of string * term * term

;; exception TypeError of string
(defun error-type (string &rest args)
  (error "Type error: ~?" string args))
;; exception ParseError of string
(defun error-parse (string &rest args)
  (error "Parse error: ~?" string args))

;; type tyvar =
;;   { tyvar_id : int;
;;     mutable tyvar_instance : ty option;
;;     mutable tyvar_name: string option;
;;   }
(defstruct (tyvar (:constructor tyvar (id &optional instance name)))
  (id 0 :type integer :read-only t)
  instance                              ; ty
  name                                  ; string
  )

;; and tyop =
;;   { tyop_name : string;
;;     tyop_types: ty list;
;;   }
(defstruct tyop
  (name nil :type string :read-only t)
  (types nil :type list :read-only t))

;; and ty =
;;   | TypeVariable of tyvar
;;   | TypeOperator of tyop
;;
;; (* Robert: Just use the above directly. *)

;; let next_variable_id = ref 0
(defvar *next-variable-id* 0)

;; let make_variable () =
;;   let new_var = { tyvar_id = !next_variable_id; tyvar_instance = None; tyvar_name = None } in
;;   next_variable_id := !next_variable_id + 1;
;;   TypeVariable(new_var)
(defun make-variable ()
  (prog1 (tyvar *next-variable-id*)
    (incf *next-variable-id*)))

;; let next_unique_name = ref "a"
(defvar *next-unique-name* (char-code #\A))

;; let variable_name v = match v with
;;   | { tyvar_name = Some(name); _ } -> name
;;   | { tyvar_name = None; _ } ->
;;       let new_var_name = !next_unique_name in
;;       v.tyvar_name <- Some(new_var_name);
;;       next_unique_name := String.of_char @@ Option.value_exn (Char.of_int @@ (Char.to_int !next_unique_name.[0]) + 1);
;;       new_var_name
(defun variable-name (v)
  (or (tyvar-name v)
      (prog1 (setf (tyvar-name v) (string (code-char *next-unique-name*)))
        (incf *next-unique-name*))))

;; let rec type_to_string = function
;;   | TypeVariable({ tyvar_instance = Some(instance); _ }) -> type_to_string instance
;;   | TypeVariable({ tyvar_instance = None; _ } as v) -> variable_name v
;;   | TypeOperator({ tyop_name; tyop_types }) ->
;;       let length = List.length tyop_types in
;;       if length = 0
;;       then tyop_name
;;       else if length = 2
;;       then Printf.sprintf "(%s %s %s)"
;;             (type_to_string (List.nth_exn tyop_types 0))
;;             tyop_name
;;             (type_to_string (List.nth_exn tyop_types 1))
;;       else Printf.sprintf "%s %s" tyop_name
;;             (String.concat ~sep:" " (List.map ~f:type_to_string tyop_types))
(defun type-to-string (ty)
  (etypecase ty
    (tyvar
     (if (tyvar-instance ty)
         (type-to-string (tyvar-instance ty))
         (variable-name ty)))
    (tyop
     (if (null (tyop-types ty))
         (tyop-name ty)
         (format nil "(~A ~{~A~^ ~})" (tyop-name ty) (mapcar #'type-to-string (tyop-types ty)))))))

;; type env = (string * ty) list

;; let make_fun_type from_type to_type = TypeOperator({ tyop_name = "->"; tyop_types = [from_type; to_type] })
(defun make-fun-type (from to)
  (make-tyop :name "->" :types (list from to)))

;; let int_type = TypeOperator({ tyop_name = "int"; tyop_types = [] })
(sb-ext:defglobal int-type (make-tyop :name "int"))

;; let bool_type = TypeOperator({ tyop_name = "bool"; tyop_types = [] })
(sb-ext:defglobal bool-type (make-tyop :name "bool"))

;; let rec prune t = match t with
;;   | TypeVariable({ tyvar_instance = Some(instance); _ } as v) ->
;;       let new_instance = prune instance in
;;         v.tyvar_instance <- Some(new_instance);
;;         new_instance
;;   | _ -> t
(defun prune (ty)
  (etypecase ty
    (tyvar
     (let ((instance (tyvar-instance ty)))
       (if (null instance)
           ty
           (setf (tyvar-instance ty) (prune instance)))))
    (tyop
     ty)))

;; let rec occurs_in_type v t2 = match prune t2 with
;;   | pruned_t2 when pruned_t2 = v -> true
;;   | TypeOperator({ tyop_types; _ }) -> occurs_in v tyop_types
;;   | _ -> false
;; and occurs_in t types =
;;   List.exists ~f:(fun t2 -> occurs_in_type t t2) types
(defun occurs-in-type (v t2)
  (let ((pruned-t2 (prune t2)))
    ;; XXX is this RIGHT?
    (if (equalp v pruned-t2)
        t
        (typecase pruned-t2
          (tyop (occurs-in v (tyop-types pruned-t2)))
          (otherwise nil)))))

(defun occurs-in (ty types)
  (some (lambda (ty2) (occurs-in-type ty ty2)) types))

;; let is_integer_literal name =
;;   match
;;     try Some(int_of_string name)
;;     with Failure(_) -> None
;;   with
;;   | None -> false
;;   | Some(_) -> true
(defun is-integer-literal (name)
  (and (ignore-errors (parse-integer name :junk-allowed nil))
       t))

;; let is_generic v non_generic = not @@ occurs_in v (Set.to_list non_generic)
(defun is-generic (v non-generic)
  (not (occurs-in v non-generic)))

;; let fresh t non_generic =
;;   let table = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
;;   let rec freshrec tp =
;;     match prune tp with
;;     | TypeVariable(_) as p ->
;;       if is_generic p non_generic then
;;         match Hashtbl.find table p with
;;         | None ->
;;           let new_var = make_variable () in
;;             ignore (Hashtbl.add table ~key:p ~data:new_var);
;;             new_var
;;         | Some(var) -> var
;;       else p
;;     | TypeOperator({ tyop_types; _ } as op) ->
;;         TypeOperator({ op with tyop_types = List.map ~f:freshrec tyop_types })
;;   in freshrec t
(defun fresh (ty non-generic)
  ;; XXX make correct
  (let ((table (make-hash-table :test 'equalp)))
    (labels ((freshrec (tp)
               (let ((ptp (prune tp)))
                 (etypecase ptp
                   (tyvar
                    (if (not (is-generic ptp non-generic))
                        ptp
                        ;; XXX make correct
                        (multiple-value-bind (var exists?) (gethash ptp table)
                          (if exists?
                              var
                              (setf (gethash ptp table) (make-variable))))))
                   (tyop
                    (make-tyop :name (tyop-name ptp)
                               :types (mapcar #'freshrec (tyop-types ptp))))))))
      (freshrec ty))))

(defun assoc-find (env name)
  (let ((entry (assoc name env :test #'string=)))
    (and entry
         (cdr entry))))

;; let get_type name env non_generic =
;;   match List.Assoc.find env name with
;;   | Some(var) -> fresh var non_generic
;;   | None ->
;;       if is_integer_literal name
;;       then int_type
;;       else raise (ParseError ("Undefined symbol " ^ name))
(defun get-type (name env non-generic)
  (let ((var (assoc-find env name)))
    (cond
      (var (fresh var non-generic))
      ((is-integer-literal name) int-type)
      (t (error-parse "Undefined symbol ~S" name)))))

;; let rec unify t1 t2 =
;;   match prune t1, prune t2 with
;;   | (TypeVariable(v) as a), b ->
;;       if a <> b then
;;         if occurs_in_type a b
;;         then raise (TypeError "Recursive unification");
;;         v.tyvar_instance <- Some(b)
;;   | (TypeOperator(_) as a), (TypeVariable(_) as b) -> unify b a
;;   | (TypeOperator({ tyop_name = name1; tyop_types = types1 }) as a), (TypeOperator({ tyop_name = name2; tyop_types = types2 }) as b) ->
;;     if (name1 <> name2 || List.length types1 <> List.length types2)
;;     then raise (TypeError (Printf.sprintf
;;       "Type mismatch %s != %s" (type_to_string a) (type_to_string b)));
;;     ignore (List.map2_exn ~f:unify types1 types2)
(defun unify (ty1 ty2)
  (let ((pty1 (prune ty1))
        (pty2 (prune ty2)))
    (cond
      ((tyvar-p pty1)
       (unless (equalp pty1 pty2)
         (when (occurs-in-type pty1 pty2)
           (error-type "Recursive unification"))
         (setf (tyvar-instance pty1) pty2)))
      ((and (tyop-p pty1)
            (tyvar-p pty2))
       (unify pty2 pty1))
      ((and (tyop-p pty1)
            (tyop-p pty2))
       (let ((name1  (tyop-name pty1))
             (types1 (tyop-types pty1))
             (name2  (tyop-name pty2))
             (types2 (tyop-types pty2)))
         (when (or (not (string= name1 name2))
                   (not (= (length types1) (length types2))))
             (error-type "Type mismatch: ~S and ~S" (type-to-string pty1) (type-to-string pty2)))
         (mapc #'unify types1 types2)))
      (t
       (error "Unreachable."))))
  nil)

(defun assoc-add (env name val)
  (labels ((rec (current-assoc new-assoc seen)
             (if (endp current-assoc)
                 (if (null seen)
                     (acons name val (nreverse new-assoc))
                     (nreverse new-assoc))
                 (let ((entry (first current-assoc)))
                   (if (string= name (car entry))
                       (rec (rest current-assoc) (acons name val new-assoc) t)
                       (rec (rest current-assoc) (cons entry new-assoc) nil))))))
    (rec env nil nil)))

(defun set-add (set item)
  (adjoin item set :test 'equalp))

;; let analyse term' env' =
;;   let rec analyserec term env non_generic = match term with
(defun analyze (term env)
  (labels ((rec (term env non-generic)
             (alexandria:destructuring-ecase term
               ;;   | Ident(name) -> get_type name env non_generic
               ((:ident name)
                (get-type name env non-generic))

               ;;   | Apply(fn, arg) ->
               ;;     let fun_ty = analyserec fn env non_generic in
               ;;     let arg_ty = analyserec arg env non_generic in
               ;;     let ret_ty = make_variable () in
               ;;     unify (make_fun_type arg_ty ret_ty) fun_ty;
               ;;     ret_ty
               ((:apply fn arg)
                (let* ((fun-ty (rec fn env non-generic))
                       (arg-ty (rec arg env non-generic))
                       (ret-ty (make-variable)))
                  (unify (make-fun-type arg-ty ret-ty) fun-ty)
                  ret-ty))

               ;;   | Lambda(arg, body) ->
               ;;     let arg_ty = make_variable () in
               ;;     let ret_ty = analyserec body (List.Assoc.add env arg arg_ty) (Set.add non_generic arg_ty) in
               ;;     make_fun_type arg_ty ret_ty
               ((:lambda arg body)
                (let* ((arg-ty (make-variable))
                       (ret-ty (rec body (assoc-add env arg arg-ty) (set-add non-generic arg-ty))))
                  (make-fun-type arg-ty ret-ty)))

               ;;   | Let(v, defn, body) ->
               ;;     let defn_ty = analyserec defn env non_generic in
               ;;     analyserec body (List.Assoc.add env v defn_ty) non_generic
               ((:let v defn body)
                (let ((defn-ty (rec defn env non-generic)))
                  (rec body (assoc-add env v defn-ty) non-generic)))

               ;;   | LetRec(v, defn, body) ->
               ;;     let new_ty = make_variable () in
               ;;     let new_env = (List.Assoc.add env v new_ty) in
               ;;     let defn_ty = analyserec defn new_env (Set.add non_generic new_ty) in
               ;;     unify new_ty defn_ty;
               ;;     analyserec body new_env non_generic
               ((:letrec v defn body)
                (let* ((new-ty (make-variable))
                       (new-env (assoc-add env v new-ty))
                       (defn-ty (rec defn new-env (set-add non-generic new-ty))))
                  (unify new-ty defn-ty)
                  (rec body new-env non-generic))))))
    ;;   in
    ;;   analyserec term' env' (Set.empty ~comparator:Comparator.Poly.comparator)
    (rec term env nil)))
;;   | Ident of string
;;   | Lambda of string * term
;;   | Apply of term * term
;;   | Let of string * term * term
;;   | LetRec of string * term * term
