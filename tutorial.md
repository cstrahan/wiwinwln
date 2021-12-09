<p class="center logo">
![](img/title.png)
</p>

Charles Strahan (<a class="author" href="https://twitter.com/charlesstrahan">@charlesstrahan</a>)

The source for this document is [available
here](https://github.com/cstrahan/wiwinwln). If there are any errors or
you think of a more illustrative example feel free to submit a pull
request on Github.

This is the first draft of this document.

**License**

This code and text are dedicated to the public domain. You can copy,
modify, distribute and perform the work, even for commercial purposes,
all without asking permission.

Basics
======

Installation
------------

If you don't have the [Nix package manager](http://nixos.org/nix/) yet,
I suggest that you do one of the following:

- Try out Nix from the comfort of one of the official [NixOS VirtualBox
  Appliances](http://nixos.org/nixos/download.html), or
- Install Nix directly

Should you choose the latter, you can rest assured that Nix won't
interfere with your existing package manager or presently installed
packages[^1].

Installing Nix is trivial:

``` console
$ curl https://nixos.org/nix/install | sh
```

REPL
----

Once you have Nix installed, install the `nix-repl` utility:

``` console
$ nix-env -iA nix-repl
```

The `nix-repl` command line tool provides a convenient
[REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
environment for learning the Nix expression language. Let's try it out:

``` console
$ nix-repl
Welcome to Nix version 1.8. Type :? for help.

nix-repl> 1 + 2
3
```

Now that you are equipped with the `nix-repl` tool, I encourage you to
read the [Nix manual](https://nixos.org/nix/manual)'s chapter on
["Writing Nix Expressions"][writing-nix-expressions], testing out the
syntax as you read.

Conventions
-----------

This document will use `pkgs` to refer to the result of `import
<nixpkgs> {}`, which is the set of all standard packages and supporting
attributes. From `nix-repl`, you can do:

``` console
nix-repl> pkgs = import <nixpkgs> {}

nix-repl> pkgs.system
"x86_64-linux"
```

Alternatively, you can load all of the attributes into the global scope:

``` console
nix-repl> :l <nixpkgs>
Added 4780 variables.

nix-repl> system
"x86_64-linux"
```

Writing Nix Expressions
=======================

Common Functions
----------------

Nixpkgs includes a number of incredibly useful functions. I recommend
first looking at [`pkgs.lib`][pkgs.lib]:

``` console
nix-repl> :l <nixpkgs>
Added 4780 variables.

nix-repl> lib.concatStringsSep ", " ["a" "b" "c"]
"a, b, c"
```

Aside from the [builtin functions][builtins] and those exposed by `lib`,
other functions worth keeping in mind:

- [buildEnv][buildEnv]: Merges multiple derivations into a new, unified
  derivation. This is done by creating a symlink tree, with the nodes
  pointing at their originating store path. (This is how profiles work.)
- [fetchurl, fetchzip, fetchgit, ...][fetchers]: Fetch a source package.
- [runCommand][runCommand]: Create a derivation from a shell command.
- [writeTextFile][writeTextFile]: Create a single file. Take a look around `trivial-builders.nix` for similar functions.


Dynamically Setting/Getting Attributes
--------------------------------------

You can dynamically set and get attributes:

``` console
nix-repl> rec { attrName = "foo"; ${attrName} = "bar"; }.foo
"bar"

nix-repl> let attrName = "foo"; in { foo = "bar"; }.${attrName}
"bar"
```

You can also use interpolation as usual:

``` console
nix-repl> rec { attrName = "foo"; "${attrName}bar" = "baz"; }.foobar
"baz"
```

Import From Derivation
----------------------

The `import` keyword can be used to evaluate an expression residing in a
derivation. That's sort of a mouthful, so let's look at an example. Save
the following code as `dynamic.nix`:

``` Nix
with import <nixpkgs> {};
let
  file = writeText "demo.nix" ''
    {
      a = 1;
      b = 2;
      c = 3;
      d = 4;
      e = 5;
    }
  '';
  evaluated = import file;

in
  evaluated.c
```

Now we'll ask Nix to evaluate the expression:

``` console
$ nix-instantiate --eval --read-write-mode dynamic.nix
building path(s) ‘/nix/store/yjr7mg8a8wpcrvm9mjpskhcip3qa6xvb-demo.nix’
3
```

We could have just as well generated the Nix expression dynamically:

``` Nix
with import <nixpkgs> {};
let
  file = runCommand "demo.nix" {} ''
    exec 1>$out
    i=1

    echo "{"
    for x in a b c d e; do
      echo "  $x = $i;"
      i=$((i+1))
    done
    echo "}"
  '';
  evaluated = import file;

in
  evaluated.c
```

``` console
$ nix-instantiate --eval --read-write-mode dynamic.nix
building path(s) ‘/nix/store/0hnis8bgscqqv2wxnypwmpf8dmbn30v4-demo.nix’
3
```

This isn't a particularly common practice, but it can be useful in some
cases. For example, you could write a function that takes a project
file, manipulates it with python to produce an equivalent Nix
expression, and then uses the evaluated metadata to package your project.

Tying The Knot
--------------

In a purely functional language like Nix, it might seem that creating
cyclic or doubly linked lists would be impossible. However, this is not
the case: laziness allows for such definitions, and the procedure of
doing so is called *tying the knot*. Let's look at an example:

``` console
nix-repl> let attrs = { a = attrs; b = 42; }; in attrs
{ a = { ... }; b = 42; }

nix-repl> let attrs = { a = attrs; b = 42; }; in attrs.a
{ a = { ... }; b = 42; }

nix-repl> let attrs = { a = attrs; b = 42; }; in attrs.a.a.a.a.a.a.b
42
```

The declaration that `b = 42` is fairly uninteresting, but what *is*
interesting is that we declare that `a` is equal to `attrs` - in other
words, `attrs` points at itself! Note the first couple lines where
`nix-repl` reports `a = { ... }`: `nix-repl` prints the ellipses here
because it realizes that `attrs` is infinitely recursive.

Lets abstract this pattern into a new function called
[`fix`](http://en.wikipedia.org/wiki/Fixed-point_combinator):

``` console
nix-repl> fix = f: let x = f x; in x
```

`fix` is a function that takes another function `f` as a parameter. It
then gives back `x`, where `x` is equal to whatever `f x` is. Note the
recursion there! You might be wondering how this could be possible - in
most imperative languages, you can't pass a value as an argument without
having first evaluated that argument; not so with Nix! Let's rewrite the
last example using `fix`:

``` console
nix-repl> fix (attrs: { a = attrs; b = 42; })
{ a = { ... }; b = 42; }

nix-repl> (fix (attrs: { a = attrs; b = 42; })).a.a.a.a.b
42
```

We give `fix` a function `f`, and `fix` gives `f` its own output. It's
as though `f` were [eating its own tail][ouroboros]!

Let's now use `fix` to simulate the `rec` keyword:

``` console
nix-repl> fix (attrs: with attrs; { a = b*2; b = c*-1; c = 42; })
{ a = -84; b = -42; c = 42; }
```

*Tying the knot* is an incredibly useful technique: it is used
extensively in the implementation of the [NixOS configuration
module][modularity] system, and is crucial in the implementation of many
supporting functions in [Nixpkgs][nixpkgs].

Packaging
=========

TODO

Developing Software
===================

There are a couple ways to manage your development environments:

- Per-project environments using the [`nix-shell`][nix-shell] tool.
- User profiles.
- If you're using NixOS, you can install software system-wide by
  listing your packages in `/etc/nixos/configuration.nix`.\
  See [`environment.systemPackages`][environment.systemPackages].

For utilities that I always want at my fingertips, I'll install such
packages into my profile via `nix-env`; however, for real projects I
tend to use `nix-shell` to provide fully deterministic development
environments.

nix-shell
---------

The [`nix-shell`][nix-shell] tool is used to set up per-project
development environments. Let's look at some examples, shall we?

Running a single command:

``` console
$ nix-shell --pure -p python --command "which python"
/nix/store/1sj48dia82dn7r77sg2rdy3mq47da400-python-2.7.9/bin/python
```

The `--pure` argument tries to purify the environment by
resetting environment variables like `$PATH`, that way you *only* have
the packages you specify.

The `-p` argument allows you to refer to a package by attribute name.
Behind the scenes, `nix-shell` is actually evaluating the following
expression (assuming you used `-p python`):

``` Nix
with import <nixpkgs> {}; python
```

You can therefore use any arbitrary Nix expression, so long as it
evaluates to a derivation (note that you *must* parenthesize the
expression if it contains a space):

``` console
$ nix-shell -p "(ruby.override { cursesSupport = false; })" --command "which ruby"
/nix/store/vlcvlmxripr5c8vg6nhsmml68jiivzh3-ruby-1.9.3-p547/bin/ruby

$ nix-shell -p "(ruby.override { cursesSupport = true; })" --command "which ruby"
/nix/store/z6jmr2kphiqnrjypbrskjdhys7ajdk12-ruby-1.9.3-p547/bin/ruby
```

In addition to running one-off commands, you can also work within a new shell:

``` console
$ nix-shell -p python

[nix-shell:~]$ echo "hello from a new shell! python is $(which python)"
hello from a new shell! python is /nix/store/1sj48dia82dn7r77sg2rdy3mq47da400-python-2.7.9/bin/python
```

We can also pass `nix-shell` an expression on the command line, in
which case you'll be given the build environment for that derivation:

``` console
$ nix-shell -E "with import <nixpkgs> {}; hello"

[nix-shell:~]$ which hello

[nix-shell:~]$ which make
/nix/store/wzalh81ncfr3j8jrsksavq05xk8jcps8-gnumake-3.82/bin/make

[nix-shell:~]$ echo $src
/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz
```

As you can see above, `hello`'s build environment (e.g. `make`, `gcc`,
etc.) is in our new shell environment, but the `hello` binary itself
*isn't*. We could, however, now use this to build `hello` outside of `nix-build`.

Ok, binaries make sense - `nix-shell` just puts them on `$PATH` for you.
But how does library discovery work? Let's say you're doing some C/C++
development and you want to link to [Cap'n
Proto](https://capnproto.org/) - how will your build tools know where to
find `libcapnp.so`? Let's take a look:

``` console
$ nix-shell --pure -p capnproto -p pkgconfig

[nix-shell:~]$ echo $PKG_CONFIG_PATH
/nix/store/6pjvbx2590bilwnc893gylc1pp4d4i1f-capnproto-0.5.0/lib/pkgconfig

[nix-shell:~]$ pkg-config --list-all
capnp-rpc Cap'n Proto RPC - Fast object-oriented RPC system
capnp     Cap'n Proto - Insanely fast serialization system

[nix-shell:~]$ pkg-config --libs capnp
-L/nix/store/6pjvbx2590bilwnc893gylc1pp4d4i1f-capnproto-0.5.0/lib -lcapnp -lkj -pthread -lpthread
```

Aha! The same way one would develop a Nix package with such
dependencies. In this case, we add [`pkgconfig`][pkgconfig], which
includes an [envHook][pkgconfig-envHook] responsible for setting-up
`$PKG_CONFIG_PATH` correctly in the presence of other `pkg-config` aware
libraries. When we add the `capnproto` package, the `envHook` is
triggered, and now our build tools can find the necessary flags.

`nix-shell` also supports reading expressions from files via `nix-shell
some-file.nix`. The behavior is identical to:

``` console
$ nix-shell -E "$(cat some-file.nix)"
```

In other words, just as `-E expr` gives you the build environment for
`expr`, pointing `nix-shell` at a file gives you the build environment
for the expression therein.

If `nix-shell` is run without any arguments, it will attempt to evaluate
`shell.nix`, falling back to `default.nix`. We can create a dummy
derivation in `shell.nix` to create per-project development
environments:

``` Nix
# Save this as shell.nix
with import <nixpkgs> {};
runCommand "dummy" {
  # The packages we want in our environment
  buildInputs = [
    python
    pkgconfig
    capnproto
    vim
  ];
  # nix-shell looks for this (optional) 'shellHook' attribute on the target
  # derivation, which is then evaluated in the new shell.
  shellHook = ''
    echo "Hey look, we're doing some setup stuff!"
    FOO=BAR
  '';
}
"" # <-- There's no need to specify a build command,
   #     since nix-shell won't try to build this package.
```

``` console
$ nix-shell
Hey look, we're doing some setup stuff!

[nix-shell:~]$ echo $FOO
BAR

[nix-shell:~]$ which python
/nix/store/1sj48dia82dn7r77sg2rdy3mq47da400-python-2.7.9/bin/python
```

Using `nix-shell` and `shell.nix`, we can create very reusable,
deterministic development environments - no more "works on my machine"
scenarios. Pretty slick, huh?

[buildEnv]: https://github.com/NixOS/nixpkgs/blob/1aed33f68b055180d0d52ff04699b0ca0ab05257/pkgs/top-level/all-packages.nix#L260
[fetchers]: https://github.com/NixOS/nixpkgs/blob/1aed33f68b055180d0d52ff04699b0ca0ab05257/pkgs/top-level/all-packages.nix#L309-324
[runCommand]: https://github.com/NixOS/nixpkgs/blob/1aed33f68b055180d0d52ff04699b0ca0ab05257/pkgs/build-support/trivial-builders.nix#L8
[writeTextFile]: https://github.com/NixOS/nixpkgs/blob/1aed33f68b055180d0d52ff04699b0ca0ab05257/pkgs/build-support/trivial-builders.nix#L15


[pkgconfig]: https://github.com/NixOS/nixpkgs/blob/1aed33f68b055180d0d52ff04699b0ca0ab05257/pkgs/development/tools/misc/pkgconfig/default.nix
[pkgconfig-envHook]: https://github.com/NixOS/nixpkgs/blob/1aed33f68b055180d0d52ff04699b0ca0ab05257/pkgs/development/tools/misc/pkgconfig/setup-hook.sh
[nixpkgs]: https://github.com/NixOS/nixpkgs
[modularity]: http://nixos.org/nixos/manual/sec-configuration-syntax.html#sec-modularity
[builtins]: http://nixos.org/nix/manual/#ssec-builtins
[pkgs.lib]: https://github.com/NixOS/nixpkgs/blob/master/lib/default.nix
[environment.systemPackages]: http://nixos.org/nixos/manual/ch-options.html#opt-environment.systemPackages
[nix-shell]: http://nixos.org/nix/manual/#sec-nix-shell
[ouroboros]: http://en.wikipedia.org/wiki/Ouroboros
[writing-nix-expressions]: http://nixos.org/nix/manual/#chap-writing-nix-expressions),


[^1]: Nix only touches files under its own prefix (`/nix/store`), so
  as far as the rest of your system is concerned, it doesn't even exist.
  Uninstalling is as easy as `rm -rf /nix`.
