; Explicit dispatch: You must be careful to avoid naming collisions,
; and whenever you add a new type of representation, you must add a
; clause to each of the generic interface procedures to check for the
; new type and apply the approprite selector for that representation

; Data-directed style: You only need to add a new entry to the table
; every time you add a new type of representation. You need to add a new
; clause to every representation every time you add a new operation.

; Message-passing syle: To add a new type you simply implement it and to add
; a new operation you add its dispatch procedure to every type.

; A data-directed or message-passing style would be appropriate for
; organizations in which new types are frequently added.
