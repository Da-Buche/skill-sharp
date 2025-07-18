(defmethod with_docstring ( ( obj t ) )
  "Return OBJ."
  obj)

(defmethod with_docstring_before @before ( ( obj t ) )
  "Return OBJ."
  obj)

(defmethod with_docstring_after @after ( ( obj t ) )
  "Return OBJ."
  obj)

(defmethod with_docstring_around @around ( ( obj t ) )
  "Return OBJ."
  obj)

(defmethod no_docstring ( ( obj t ) )
  obj)

(defmethod no_docstring_before @before ( ( obj t ) )
  obj)

(defmethod no_docstring_after @after ( ( obj t ) )
  obj)

(defmethod no_docstring_around @around ( ( obj t ) )
  obj)

