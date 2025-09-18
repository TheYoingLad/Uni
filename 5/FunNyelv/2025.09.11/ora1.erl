-module(ora1).
-export([foo/0, foo/1]).
% -compile(export_all).

foo() -> "Hello".

foo(X) -> X + 1.