# Write-You-A-Python-Lexer

Write-You-A-Lexer is a project that implements a lexer for Python 3.9 version. This project has five `Example` folders. Each `Example` folder builds on the previous version. But, each `Example` folder can be run and tested indepenently.

This project is used in the tutorial `Alex By Example (Write-You-A-Python-Lexer) and it can be understood by going through the tutorial.

# Layout of the Project

The project contains five `Example` folders.  Each `Example` folder contains the following modules.

 - Lexer.x - Contains the implementation of lexer features explained under each section in the tutorial.
 - LexerRunner.x - Wrapper for running the `Lexer` under the respective `Example` folder.
 - LexerUtil.x - All the helper function/definitions required by the `Lexer.x`. The `Lexer.x` imports this module.
 - Tokens.hs   - Defintions of `Tokens` the Lexer generates.

All modules are named the same across all Example folders. Therfore, the above description holds true for all `Example` folders.

 ## Tests

 For each `Example` folder we have a corresponding tests under `test_tokenizer<no>.hs`, where `<no>` is the `Example` number. For examle, all tests for `Example2` lexer can be run by running the tests in `test_tokenizer2.hs`. The tests uses input data from test_fixtures files. All the test fixtures, for each `Example` folder is available under `test/test_fixtures/<no>`


``` bash
$ stack test --test-arguments "--pattern=tokenizer_tests_example_1"  wya-lexer:wya-lexer-test-1
wya-lexer> test (suite: wya-lexer-test-1, args: --pattern=tokenizer_tests_example_1)

tokenizer_tests_example_1
  ./test/test_fixtures/1/3.txt: OK
  ./test/test_fixtures/1/2.txt: OK
  ./test/test_fixtures/1/1.txt: OK
  ./test/test_fixtures/1/3.txt: OK
  ./test/test_fixtures/1/2.txt: OK
  ./test/test_fixtures/1/1.txt: OK

All 6 tests passed (0.00s)

wya-lexer> Test suite wya-lexer-test-1 passed

```

 The `test_fixtures` (around 80 of them) were generated by capturing in the input and the results by running `Lib/test/test_tokenizer.py`. This implementation passes for all those tests. Caveat: `Unicode` support for identifiers are not supported. There may still be cases this implementation may not support. The project is still in testing phase.

## Execute

There are multiple ways to use this project.

### Running from command line

Each example can be run from the command line either by providing `input` directly at the console and providing a valid `Python` file. Note, that depending on which `Example` is run, the program could fail if the grammar is not supported. For example `Example1` lexer implementation may not support all tokens in a provided valid `Python` file. `Example5` should work on any Python file (provided there are not bugs!)

Here are some examples, where the prior Example does not support a grammar but using a later version the tokens are generated.

``` bash
#  ~/fsf/write-you-a-python-lexer on git:main x
$ stack exec wya-lexer-exe -- --example_name Example1
identifier
0,0-0,0:             Name       "identifier"
42
0,0-0,0:             Number     "42"

--- Note Example2 does not support `+` yet, therefore the Lexer will fail
#  ~/fsf/wya-lexer on git:main x
$ stack exec wya-lexer-exe -- --example_name Example2
a + b
wya-lexer-exe: Lexical error(' ',[],"+ b\n")
CallStack (from HasCallStack):
  error, called at src/Example2/Lexer.x:103:28 in wya-lexer-0.1.0.0-CGMHpbYSL6uA5793UGrB8f:Example2.Lexer

-- Whereas Example3 will succeed
#  ~/fsf/write-you-a-python-lexer on git:main x C:130
$ stack exec wya-lexer-exe -- --example_name Example3
a + b
1,1-1,1:             Name       "a"
1,3-1,3:             Plus       "+"
1,5-1,5:             Name       "b"

```

Now let's print a full Python program.

``` bash

#  ~/fsf/write-you-a-python-lexer on git:main x
$ more hello_world.py
def hello_world():
    print('Testing the lexer') # with a comment here

    # and comment on empty line followed by empty line

    if 42: # here we create an indent
        another_indent()
# dedent here
hello_world()


#  ~/fsf/write-you-a-python-lexer on git:main x
$ time stack exec wya-lexer-exe -- --example_name Example5 --file_name hello_world.py
1,0-1,3:             Name       "def"
1,4-1,15:            Name       "hello_world"
1,15-1,16:           Lpar       "("
1,16-1,17:           Rpar       ")"
1,17-1,18:           Colon      ":"
1,18-1,19:           Newline    "\n"
2,0-2,4:             Indent     "    "
2,4-2,9:             Name       "print"
2,9-2,10:            Lpar       "("
2,10-2,29:           String     "'Testing the lexer'"
2,29-2,30:           Rpar       ")"
2,31-2,52:           Comment    "# with a comment here"
2,52-2,53:           Newline    "\n"
4,4-4,54:            Comment    "# and comment on empty line followed by empty line"
4,54-4,55:           Nl         "\n"
5,0-5,1:             Nl         "\n"
6,4-6,6:             Name       "if"
6,7-6,9:             Number     "42"
6,9-6,10:            Colon      ":"
6,11-6,37:           Comment    "# here we create an indent"
6,37-6,38:           Newline    "\n"
7,0-7,8:             Indent     "        "
7,8-7,22:            Name       "another_indent"
7,22-7,23:           Lpar       "("
7,23-7,24:           Rpar       ")"
7,24-7,25:           Newline    "\n"
8,0-8,13:            Comment    "# dedent here"
8,13-8,14:           Nl         "\n"
9,0-9,0:             Dedent     ""
9,0-9,0:             Dedent     ""
9,0-9,11:            Name       "hello_world"
9,11-9,12:           Lpar       "("
9,12-9,13:           Rpar       ")"
9,13-9,14:           Newline    "\n"
stack exec wya-lexer-exe -- --example_name Example5 --file_name hello_world.p  0.19s user 0.03s system 101% cpu 0.220 total

```

### Executing from ghci / Using Library Functions

``` bash

λ >>import Example5.LexerRunner as L5

λ >>L5.runLexer "s = a + b"
Right [TokenInfo {token_type = Name, token_string = "s", start_pos = (1,0), end_pos = (1,1)},TokenInfo {token_type = Equal, token_string = "=", start_pos = (1,2), end_pos = (1,3)},TokenInfo {token_type = Name, to
ken_string = "a", start_pos = (1,4), end_pos = (1,5)},TokenInfo {token_type = Plus, token_string = "+", start_pos = (1,6), end_pos = (1,7)},TokenInfo {token_type = Name, token_string = "b", start_pos = (1,8), end_pos = (1,9)}]
it :: Either String [Example5.Tokens.TokenInfo]

λ >>L5.runAndPrettyPrintLexer  "s = a + b"
1,0-1,1:             Name       "s"
1,2-1,3:             Equal      "="
1,4-1,5:             Name       "a"
1,6-1,7:             Plus       "+"
1,8-1,9:             Name       "b"
it :: ()
λ >>

```

The `Example5.runLexer` function can be used to call the lexer when used as a library.


# Known Issues (Differences with the tokenizer.py)

The library is still in testing phase. This library was implemented to support the accompanying tutorial.
   - Note that we don't generate the first ENCODE token and the last ENDMARKER token just to keep testing simple. This code can easily be changed.
   - \ line continuation character is currently not supported
   - Unicide characters in identifiers are not supported
