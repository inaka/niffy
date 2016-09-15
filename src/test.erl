%40848705

-module(test).
-compile({parse_transform, pt}).
%-niffy([sum_c/2, square/1, {nat_log/1, cpu_bound}]).
-niffy(#{functions => [sum_c/2, square/1, {nat_log/1, cpu_bound}],
         includes  => ["<math.h>"],
         flags     => ["-flag1", "-flag2"],
         load      => undefined,
         reload    => undefined,
         upgrade   => undefined,
         unload    => undefined}).

-export([sum/2, square/1, nat_log/1]).

%%==============================================================================
%% API
%%==============================================================================
sum(A, B) ->
  sum_c(A, B).

square(_A) ->
  "int square(int a)
   {
     return a * a;
   }".

nat_log(_A) ->
  "#include <math.h>
   double nat_log(double a)
   {
     double logA = log(a);
     return logA;
   }".

%%==============================================================================
%% Utils
%%==============================================================================
sum_c(_A, _B) ->
  "#include <math.h>
   int sum_c(int a, int b)
   {
     return a + b;
   }".