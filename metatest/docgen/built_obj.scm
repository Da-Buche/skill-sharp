
(let ( _obj )

  (@fun _\@set_built_obj
    ( ( obj ?type stdobj )
      )
    ?doc    "Cache OBJ so it can be retrieved at the proper time using `@built_obj'"
    ?out    stdobj
    ?global t
    (setq _obj obj)
    )

  (@fun @built_obj ()
    ?doc "Return the object currently being built.
This is meant to be used inside `defclass' slot @initform arguments to make dependent arguments.

For this to work, an `initializeInstance @before' method should be defined and use `_\\@set_built_obj'
to define the object being built.

This is managed by default when using `@class'."
    ?out    stdobj
    ?global t
    _obj
    )

  );closure

