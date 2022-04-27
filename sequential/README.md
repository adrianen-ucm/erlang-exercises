# Erlang sequential exercises 

The exercise answers are in the file `exercises.erl`, and some [QuickCheck](http://www.quviq.com/products/erlang-quickcheck/) properties for them are also defined in in the file `exercises_prop.erl`. They can be compiled with 

```sh
erlc {exercises,exercises_prop}.erl 
```

Once compiled, an Erlang escript with some simple tests which also runs the the tests for the properties can be executed as follows: 

```sh
chmod u+x exercises_test
./exercises_test
```
