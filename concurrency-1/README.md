# Erlang concurrency exercises 1

The exercise answers are in the files `exercises.erl` and `bank.erl`. There are also some [QuickCheck](http://www.quviq.com/products/erlang-quickcheck/) state machine commands in the file `bank_prop.erl`. 

All of these can be compiled for example with

```sh
erlc *.erl 
```

Once compiled, some Erlang escripts with simple tests can be executed as
follows: 

```sh
chmod u+x {exercises,bank}_test
./exercises_test
./bank_test
```

The last one also executes the property based state machine test for the bank exercise.
