# niffy
Erlang parse transform to generate NIF files from inlined C code.

### Why?
Writing NIFs is time consuming and their code can't help but being riddled with macros. And not only that; our C code has to be in a separate file, adding a layer of complexity when debugging and trying to figure out exactly what's going on.

### What can niffy do to help?
Simple, instead of writing your nifs, just inline some C code in your erlang files:

```erlang
-module(example).
-niffy([square/1]).
-export([square/1]).

square(_A) ->
  "int square(int a)
   {
     return a * a;
   }".
```

That code compiles and does exactly what you would expect:

```
> example:square(2).
4
```

<img src="http://i.imgur.com/YsbKHg1.gif" align="center" style="float:center" height="400" />

Niffy takes care of many things. First, it generates the NIF from the inlined C code, making all the changes so that erlang can include it. For example, given the previous erlang file, niffy will generate (and compile) the following C code:

```c
#include "erl_nif.h"

static ERL_NIF_TERM square_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int a;
  if (!enif_get_int(env, argv[0], &a))
    return enif_make_badarg(env);
  return enif_make_int(env, a * a);
}

static ErlNifFunc niffuncs[] = {{"square", 1, square_nif, 0}};

ERL_NIF_INIT(example, niffuncs, NULL, NULL, NULL, NULL)
```

(Yes, it also handles the indentation)

Once that's done, it will modify the original erlang file so that the function ``square/1`` throws ``nif_not_loaded`` if you attempt to call it.

And finally, it will add a function to load the nif on the module's ``on_load`` function (don't worry, if no ``on_load`` function is present on the module, niffy just adds one).

### What else?
Well, that was just a basic example, niffy also handles includes, whatever compiler flags you need and you can even flag some functions as dirty.

A more complex example would be this one, where instead of a list of functions we define a map with some properties (they are actually all optional):

```erlang
-module(another_example).

-niffy(#{functions => [sum_c/2, square/1, {nat_log/1, cpu_bound}],
         includes  => ["<math.h>"],
         flags     => ["-pedantic"]}).

-export([sum/2, square/1, nat_log/1]).

square(_A) ->
  "int square(int a)
   {
     return a * a;
   }".

nat_log(_A) ->
  "#include <math.h>
   double nat_log(double a)
   {
     // this will return the natural log
     double logA = log(a);
     // return logB;
     return logA;
   }".
```

As you can see, it's pretty clear when one of our functions is either ``cpu_bound`` or ``io_bound``. Also, including headers and setting compiler flags is pretty simple (also, notice that you can add the headers as many times as you wish, including on the function bodies, just use whatever you feel makes for a more readable file).

Again, that works just fine and generates pretty easy to follow code:

```c
#include "erl_nif.h"
#include <math.h>

static ERL_NIF_TERM nat_log_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  double a;
  if (!enif_get_double(env, argv[0], &a))
    return enif_make_badarg(env);
  // this will return the natural log
  double logA = log(a);
  // return enif_make_double(env, logB);
  return logA;
}

static ERL_NIF_TERM square_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int a;
  if (!enif_get_int(env, argv[0], &a))
    return enif_make_badarg(env);
  return enif_make_int(env, a * a);
}

static ErlNifFunc niffuncs[] = {{"nat_log", 1, nat_log_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
                                {"square", 1, square_nif, 0}};

ERL_NIF_INIT(another_example, niffuncs, NULL, NULL, NULL, NULL)
```

Just remember that niffy is not parsing your code. And it's just making some reasonable assumptions regarding how it is written. You can see that in the way it handled the C comments, any ``return .*;`` that's not followed by ``enif_make_*`` will be modified for example.

### How to use niffy?
Couldn't be simpler: Set it as a dependency of your project and add ``{parse_transform, niffy_transform}`` to ``erl_opts`` in your ``rebar.config`` file. Other options for your ``erl_opts`` file include: The compiler flags that apply to all the files, and the directory where you want to output the generated C code:

```
{niffy_cdir, "_generated/c_src"}, % This is the default
{niffy_flags, ["-Werror",
               "-I/usr/lib/erlang/erts-5.8.1/include"]}
```

Just keep in mind that if you want to upgrade niffy, you **have to delete your _build folder** and that it would be a good idea to add ``*.so`` and ``_generated`` to your ``.gitignore``

### Contact Us
For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](https://www.hipchat.com/gpBpW3SsT).

If you find any **bugs**, **problems** or got any **suggestions**, please [open an issue](https://github.com/inaka/niffy/issues/new) in this repo (or even a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)
