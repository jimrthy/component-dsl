# component-dsl

A Clojure library designed to cut back on the boiler plate associated
with building up [Stuart Sierra
Component](https://github.com/stuartsierra/component/ "Components") trees.

There isn't a lot of it (so this is a very small library), but I
got annoyed with repeating what little there is.

This was heavily inspired by
[oolong](https://github.com/irresponsible/oolong "Oolong"). I started
out trying to use that, but the author told me that he was having
intractable problems getting it to restart.

## Usage

Until I get around to figuring out how to push this to clojars (and getting
a version that's useful enough to seem worth doing so), you'll
need to install it manually.

1. Clone this repo
2. Run `lein install`

Add [com.jimrthy/component-dsl "0.1.0"] to your project's :dependencies.

Set up a constructor function for each of your components. It should accept
one argument (which you'll supply).

In the place where you'd normally build your System tree and set up its
dependencies:
    (require '[component-dsl.system :as system])
    (require '[com.stuartsierra.component :as component])
    (let [description {:structure {:component1 'namespace1.creation-function
                                   :component2 'namespace2.creation-function}
	               :dependencies {:component2 [:component2]}
          options {:component1 "This is the parameter for its constructor"
	           :component2 {:arg1 "Supply whatever you like"
		                :arg2 :something-very-cool}}
          ready-to-go (system/build description options)
	  started (component/start ready-to-go)]
      (do-your-stuff started)
      (component/stop started))

Alternatively, put description into an EDN file and use system/ctor instead:

    (def ready-to-go (system/ctor "/path/to/description.edn" constructor-options))

I haven't found the second approach very useful, so it hasn't really been
used much.

## Context Managers

The component-dsl.context-manager namespace has a couple of functions for
managing where the system lives. Inspired by [5 Faces of Dependency Injection
in Clojure]
(http://software-ninja-ninja.blogspot.co.il/2014/04/5-faces-of-depndency-injection-in.html).

Once your System is started, call (context-manager/setup system).

After that, access to the System is "globally" available by calling
(context-manager/context f arg1 arg2 arg3 ... argN). The context manager
will call (f system arg1 arg2 arg3 ... argN).

Time will tell whether this is actually a good idea or not. Used judiciously,
with some sort of notation warning that the function being called is not
purely functional (which also pollutes all its callers), I think it probably
makes more sense than the alternative methods I've used to hold the System.


## Afterthoughts

Odds are, you should be able to pass the dependencies as a map, the
same way you would to component/system-using. This is just a light
wrapper over that, after all.

## Future

1. Get this published on clojars

## License

Copyright © 2015 James Gatannah

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version, the same as Clojure.
