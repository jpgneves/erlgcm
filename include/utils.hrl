-ifndef(_GCM_UTILS_HRL).
-define(_GCM_UTILS_HRL, true).

-define(ternary(Condition, OnTrue, OnFalse), if
					       Condition -> OnTrue;
					       true      -> OnFalse
					     end).

-endif.
