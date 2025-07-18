
;; SKILL variables

(inSkill
  only_skill_var = 42
  global_skill_var = 27
  imported_skill_var = 12

  (defun global_skill_function ()
    t)

  (defclass skill_class ())

  (defmethod skill_method ( ( obj skill_class) )
    obj)

  (let ()

    (defun local_skill_function ()
      nil)

    );let

  skill_container.attribute = t

  );inSkill

;; Scheme variables

(importSkillVar imported_skill_var)

global_skill_var = "new_value"
imported_skill_var = '(1 2 3)

only_scheme_var = (makeTable t nil)

(defun global_scheme_function ()
  (list t nil)
  )

(let ()

  (defun local_scheme_function ()
    (list nil t)
    )

  (defglobalfun nonlocal_scheme_function ()
    (list t t)
    )

  (defclass scheme_class ())

  (defmethod scheme_method ( ( obj scheme_class) )
    obj)

  scheme_container.attribute = t
  scheme_container.prop = nil

  )

