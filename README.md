**columnist** is an experimental C++23 library for

[ECS](https://en.wikipedia.org/wiki/Entity_component_system)
(Entity Component System), a data structure based on "struct
of vectors" for good locality of reference. Each "vector" is
column. Elements are kept packed towards the beginning of each
column, but can be referenced using stable row_id type. The
elements at the same row_id for each column is a row.

Status: **Highly Experimental**
