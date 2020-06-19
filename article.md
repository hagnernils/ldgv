# A backend for Label-Dependent Session Types LDST

The paper by [Peter Thiemann and Vasco T. Vasconcelos](#references) gives an excellent introduction to the LDST calculus.
A good introductory explanation of Session Types is given in [Igarashi, Thiemann, Vasconcelos and Wadler](#references).

This post documents a backend of LDST, written in Haskell and available at
[https://hagnernils.github.io/ldgv](https://hagnernils.github.io/ldgv).

## Goals of this article
This article provides documentation of the code (and ideas behind it) that makes up the backend of the
interpreter. It does not concern itself with the frontend (type checker) code,
which was provided by Peter Thiemann. Some parts of the code such as declarations only used for typechecking will be left out.

There are two main parts:
One about how the interpreter is built with a monad transformer, the second about creating a static site with our Haskell executable.

## Interpreters and monad transformers
### The Input : LDST
Before building an interpreter, we need to know what to interpret. The custom Parser built by the tokenizer
[Alex](https://www.haskell.org/alex/) and parser generator [Happy](https://www.haskell.org/happy/) takes
in a `String` of code and outputs LDST declarations (here simplified):

```haskell
data Decl = DType TIdent Type  -- define a type with name
          ...
          | DFun Ident [Ident] Exp -- define a function with name, parameter list and Expression
          ...
          deriving (Show, Eq)
```
where `TIdent` and `Ident` are type aliases for `String` and `Exp` are expressions.

There are also declarations only for typechecking, like type equivalency checking. They get typechecked but not
interpreted by the backend. The parsing of text into these declarations is also [tested](README.md/#testing-the-parser).

Let's take a look at available expressions. First, we have basic types:
```haskell
data Exp = Unit
         | Int Int  -- bounded Int
         | Nat Nat  -- positive Natural Number, used for Recursion
         | Var Ident  -- Variables 
         | Lab String  -- Labels, the special feature of LDST
```
Expressions allow for variable assignment 
```haskell
         | Let Ident Exp Exp
```
basic math
```haskell
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
```
function definition and application
```haskell
         | Lam Ident Exp  -- bind Ident in Expression
         | App Exp Exp  -- Apply the second expression the first
```
dependent pairs, pair decompostion / unpacking and accessing the first / second value in a pair
```haskell
         | Pair Ident Exp Exp  -- a dependent pair identifies it's first component with a tag that which can be used in the second part
         | LetPair Ident Ident Exp Exp  -- pair decomposition
         | Fst Exp 
         | Snd Exp
```
using Session types
```haskell
         | New Type  -- create an Access Point (a pair of dual channels of type Type)
         | Fork Exp  -- fork an expression
         | Send Exp  -- Send a value over a channel
         | Recv Exp  -- Receive a value over a channel
```
case deconstruction, which evaluates to an expression according to a label
```haskell
         | Case Exp [(String, Exp)]
```
and finally some recursion:
```haskell
         | Succ Exp
         | NatRec Exp Exp Ident Ident Type Exp
         | Rec Ident Ident Type Type Exp
  deriving (Show,Eq)
```
### Value representation of Expressions
The interpreter has to represent the value of expressions it is working on. This is done by the following type:
```haskell
data Value = VUnit
      | VLabel String  -- Labels
      | VInt Int  -- for simplicity, we use Int for Integers and natural numbers
      | VPair Value Value -- pair of values (or of ids that map to values)
      | VDecl S.Decl -- when an identifier maps to another function we have not yet interpreted
      | VChan (C.Chan Value) (C.Chan Value)
      | VFun (Value -> InterpretM) -- Function Type
```
This `Value` type is an algebraic datatype able to represent everything that occurs in our program.
It would also be the place to add new types like Strings, Lists, Trees and others.

To represent Session Type Channel ends we use `Chan`nels out of the package
 [ Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Concurrent-Chan.html),
which themselves are wrappers around `MVars`.
We use two channels to represent an endpoint, one for reading and one for writing to the other side,
 so we do not read our own written values.
So instead of reading what was just written, a `readChan` on a Channel gets blocked in case the other side did not write anything.

Functions are not defined in the form of `Value -> Value`, but instead use `Value -> InterpretM`, a `Value` returned inside a monad.
That is useful because operations on `Chan`s like `Chan.newChan`, reading and writing are of type `IO (Chan a)`, `IO ()` or `IO a`,
 so we have to somehow run them in the `IO` monad.
The second feature we need is an environment that keeps track of variable bindings during the evaluation.
We can solve this by stacking `Reader` (for access to variable bindings) with `IO` in a so-called monad transformer.

### Monad transformers
Monad transformers behave like stacks of monads where each monad layer provides its own functionality.
To keep track of variable bindings we use [mtl](https://github.com/haskell/mtl)s Reader.
Instead of using a `Reader r a` with environment `r` and return type `a`, we use the constructor `ReaderT r m a` together with
`runReaderT :: ReaderT r m a -> r -> m a`, which 
still allows for environment `r` and return value `a`, but this time inside a monad `m` of our choosing - here we use `IO`.

We can access functionality of monads in lower layers by lifting functions into them:
`ReaderT` implements `MonadIO m => liftIO :: IO a -> m a` to run a `IO a` as long as we fulfill a `MonadIO` typeclass constraint for our
current monad. Luckily `ReaderT` and others in the `mtl` do this: if the wrapped monad `m` is an instance of `MonadIO`, so is our combined `ReaderT r m a`.
<!--Later on we will replace the inner `IO` with JSaddles `JSM` (that also implements `MonadIO`).-->
```haskell
-- | the interpretation monad
type InterpretM = ReaderT Env IO Value

-- | maps identifiers to Values of expressions
type Env = [EnvEntry]
type EnvEntry = (String, Value)
```
<!--A monad that implements the typeclass `MonadReader r m` with inner monad `m` and resource `r` allows us to `ask` for the current variable bindings and to modify them with `local`-->
Now that we can represent the Values of Expressions and store them, we can move on to define basic interpretation.
First, we want interpretation to be a function that maps Expressions to Values inside our transformer:
```haskell
-- | interpret a single Expression
interpret' :: Exp ->  InterpretM
```
for basic Expressions the resulting Value is simple
```haskell
  Unit -> return VUnit
  Lab s -> return $ VLabel s
  Int i -> return $ VInt i
  Nat i -> return $ VInt i
```
to look up variables we use a helper
```haskell
  Var s -> do
          v <- envlookup s
          case v of
              VDecl d -> evalDFun d  -- resolve an identifier if it binds to a not yet interpreted value oder function
              _ -> return v
```
which gives an error message if a variable is used which has no `Value` bound to it.
```haskell
envlookup :: String -> InterpretM
envlookup id = do
    identifiers <- ask
    case lookup id identifiers of
        Nothing -> fail ("No Value for identifier " ++ id ++ " in Environment")
        Just val -> return val
```
Back to the `interpret'`ation: To avoid repetition we use another helper function for mathematical functions
```haskell
  Plus e1 e2 -> mathHelper (+) e1 e2
  Minus e1 e2 ->  mathHelper (-) e1 e2
  Times e1 e2 ->  mathHelper (*) e1 e2
  Div e1 e2 ->  mathHelper quot e1 e2
  Negate e1 ->  mathHelper (-) (Int 0) e1
  Succ e -> mathHelper (+) (Int 1) e
```
that applies an operator to two interpreted expressions.
```haskell
mathHelper op e1 e2 = do
    v1 <- interpret' e1
    v2 <- interpret' e2
    return $ case (v1, v2) of
      (VInt a, VInt b) -> VInt (op a b)
```
To assign a value of expression `e1` to a variable `s` we simply prepend it using `local`
to our environment in the `interpret'`ation of the inner expression `e2`.
```haskell
  Let s e1 e2 -> do
      v  <- interpret' e1
      local ((s, v):) $ interpret' e2
```
The same way a pair is unpacked, this time with two values added.
```haskell
  LetPair s1 s2 e1 e2 -> do
      v <- interpret' e1
      case v of
        (VPair v1 v2) -> local (\env -> (s1, v1):(s2, v2):env) $ interpret' e2
```
We can also have dependent pairs, where the result of e1 can be used in the interpretation of e2.
```haskell
  Pair mul s e1 e2 -> do
      v1 <- interpret' e1
      v2 <- local ((s, v1) :) $ interpret' e2
      return $ VPair v1 v2
```
As convenience we can access the first and second value in a pair with `fst` and `snd`.
```haskell
  Fst e -> do
      v <- interpret' e
      case v of
          (VPair s1 s2) -> return s1
  Snd e -> do
      v <- interpret' e
      case v of
          (VPair s1 s2) -> return s2
```
### Functions
Finally, we have the interpretation of functions. They are represented by a type that holds a closure `f` which maps the identifier `i` to an
argument `arg` and then interprets the body `e` of the function.

```haskell
  Lam i e -> do
                 env <- ask 
                 let f = \arg -> do
                                 liftIO $ runReaderT (interpret' e) ((i, arg):env)
                 return $ VFun f
```
Applying a function to a Value is now done one argument at a time.
Functions with multiple arguments get curried to intermediate `VFun`s.
```haskell
  App e1 e2 -> do
      -- the innermost App is the function with its first argument (App "funcname" val)
      -- while outer Applications are of (App (VFun somefun) val)
      arg <- interpret' e2
      v <- interpret' e1  -- interpret the inner function
      case v of
          VFun f -> do
            f arg  -- return the function applied to the argument
          _ -> do
            fail $ "Trying to Apply " ++ show e2 ++ " to " ++ show e1
```
### Session Type functionality
The backend does not implement any consistency checking for the soundness of what is sent on the session type channels. <!--s, as done in the [simple-sessions](https://hackage.haskell.org/package/simple-sessions) package. -->
Instead it relies on the type checker statically checking all code for violations.
The main functionality is given by an access point, a pair of dual endpoints. When writing to (reading from) one side it is expected that the dual part in the code reads (wrote to) it and vice versa.
To create a new access point, we use the `new` keyword, construct a channel for each direction and simply pack it into a pair.
```haskell
  New t -> do
    r <- liftIO Chan.newChan
    w <- liftIO Chan.newChan
    return $ VPair (VChan r w) (VChan w r)
```
A `send` on a channel in LDST is a partially applied function, that later gets applied to the argument that is sent.
So for `(send channelname) 42` we return a closure that writes its argument `VInt 42` to and then returns the channel.
```haskell
  Send e -> do
      v <- interpret' e
      case v of
        (VChan _ c) -> return $ VFun $ \arg -> do
                                        liftIO (Chan.writeChan c arg)
                                        return v
```
Receiving is simply reading from a channel with `readChan`, lifted into `IO`.
```haskell
  Recv e -> do
      v <- interpret' e
      case v of
        (VChan c _) -> do
          val <- liftIO $ Chan.readChan c
          return $ VPair val v
```
To enable parallelism, we can fork off an expression (but not collect it's result).
```haskell
  Fork e -> do
      penv <- ask
      liftIO $ forkIO (do
                      res <- runReaderT (interpret' e) penv
                      C.traceIO $ "Ran a forked operation with result " ++ show res)
      return VUnit
```
One special feature of LDST is a type-level recursor: it allows for implementing number-indexed protocols.
The declaration consists of a number `e1` as well as a zero and a nonzero case that get evaluated when `e1` is 0 or > 0.
The identifier `i1` binds the current value of `e1`, `i2` the result of the recursive interpretation.
```haskell
  exp@(NatRec e1 e2 i1 i2 e3) -> do
  -- returns a function indexed over e1 (should be a variable pointing to a Nat)
  -- e1 should evaluate to the recursive variable which gets decreased each time the
  -- non-zero case is evaluated
  -- e2 is the zero case
  -- i1 is the current value of e1
  -- i2 is the identifier of the predecessor
  -- e3 is the nonzero case
         i <- interpret' e1
         case i of
                 VInt 0 -> interpret' e2
                 VInt n -> do
                        -- interpret the n-1 case i2 and add it to the env
                        -- together with n before interpreting the body e3
                        let newexp = NatRec (Var i1) e2 i1 i2 e3
                        lower <- local ((i1, VInt (n-1)):) $ interpret' newexp
                        local (\env -> (i1, VInt n):(i2, lower):env) $ interpret' e3
```
The defining feature is the label dependency: Depending on a label we choose a branch to execute.
If there is no branch for a label, we fail the interpretation with an error message.
```haskell
  Case e cases -> do
      v <- interpret' e
      case v of
        (VLabel s) -> do
          case lookup s cases of
            Just e' -> interpret' e'
            Nothing -> fail $ "No case found for label " ++ show v ++ " in cases " ++ show cases
 ```

The function to evaluate references to other declarations can simply interpret when we have no free variables:
```haskell
-- | interpret a DFun (Function declaration)
evalDFun :: Decl -> InterpretM
evalDFun decl@(DFun name [] expression) = interpret' expression
```
but for functions with free variables we use the value constructor `VFun` to construct a closure for each parameter `id` in the parameter list `binds`
(the same as for lambda binding).
```haskell
evalDFun decl@(DFun name (id:binds) e) = do
                          env <- ask
                          let inner = DFun name binds e
                          let f = \arg -> do
                                 liftIO $ runReaderT (evalDFun inner) ((id, arg):env) 
                          return $ VFun f
```
## Moving to the web - compiling Haskell to Javascript
To compile the Haskell application to client-side Javascript we use reflex and
reflex-dom, which use the ghcjs compiler together with the nix paket manager.
The resulting static site can then be easily hosted by GitHub-Pages.

Reflex follows a programming pattern called [Functional Reactive Programming](https://stackoverflow.com/a/1030631/13598798), which the [Queensland Functional Programming Lab](https://qfpl.io/posts/reflex/basics/introduction) better explains than I ever could.

The backend for `reflex-dom` is the `JSM` provided by [JSaddle](https://github.com/ghcjs/jsaddle),
which luckily is an instance of `MonadIO` and thus provides `liftIO` which we can use to run the main `interpret`ation of ldst source code. 

We build a [basic page](https://hagnernils.github.io/ldgv) with two main text fields, one with the HTML id `tSrc` for the input

```haskell
elAttr "textarea" ("id" =: "tSrc" <> "class" =: "source_textarea" <>"spellcheck" =: "false") $ dynText $ fmap (T.pack.lookupExample) dVal
```
that uses a `dynText` as inner content, which is built by looking up the example file described by the dropdown.
For the second one (the output) the same method is used, but with the readonly attribute.

```haskell
elAttr "textarea" ("id" =: "tOutput" <> "class" =: "output_textarea" <> "readonly" =: "readonly" <> "spellcheck" =: "false") blank
```
Output by the typechecking and interpretation which used to go to stdout can then be redirected to our textbox.
This is done by replacing `putStrLn :: String -> IO ()` with our own `String -> JSM ()` version which
escapes newlines and quotes and uses the JSM to append text to the `value` of the `tOutput` textbox.
```haskell
printLn :: String -> JSM ()
printLn s = do
    ...
    val <- eval $ T.concat ["document.getElementById('tOutput').value += '", t, "\\n'"]
    ...
```
There is most likely another (better) way by using a `WriterT` around the `ReaderT` to store output, but appending text makes it more interactive for the user
(and easier to understand, coming from a non-functional programming background).

Upon a change of the dropdown index `_dropdown_change` of the dropdown for example files `d`, we set the source text with a simple JSaddle 
javascript evaluation to the content of the current example file.
Here, the applicative `<$>` functions the same as `fmap`ping the first argument over the second.

```haskell
        performEvent_ $ (\s -> liftJSM $ setSrc s) <$> (fmap lookupExample $ _dropdown_change d) 
```
When the interpretation button is pressed, we create an event `e`.
A constant function fetches the source text from the textbox through a javascript evaluation and maps over `e`,
so `srcText :: Event Text` contains the text inside the source textarea upon button click.
```haskell
        -- interpretation button
        (b, _) <- elAttr' "button" ("id" =: "bInterpret") $ text "Interpret" 

        -- interpret button click event
        let e = domEvent Click b

        -- get the text inside the area on button click
        srcText <- performEvent $ (const $ liftJSM $ textareaget) <$> e
```
This event is then `fmap`ped over to give the source code as input to the interpretation.
After interpretation it contains an output of type `Event String`which is `either` the result or an error mesage.
```haskell
doneEv <- performEvent ((\v -> liftIO $ do
                                                  let s = T.unpack v
                                                  -- clear the old output
                                                  resetOutput
                                                  -- interpret
                                                  res <- try $ typecheck s >> I.interpret s :: IO (Either SomeException Environment.Value)
                                                  -- print errors to the output if there are any
                                                  return $ either (\e -> "Error: " ++ show e) (\r -> "Result: " ++ show r) res
                                ) <$> srcText)

```
To display the result, a placeholder is created with `holdDyn :: a -> Event a -> m (Dynamic a)`.
The interpretation result in `doneEv` is then used to update the `output :: Dynamic Text`.
```haskell
output <- holdDyn "" $ fmap T.pack doneEv
elAttr "p" ("id" =: "tResult") $ dynText output
```

## Future improvements
### Process
- ~~Use bazel to build iteratively, not on package-level~~ we can build iteratively in the nix shell using cabal instead of building the whole nix package
- remove unnescessary javascipt, for example with the google closure compiler
- use (Travis) CI to run tests and CD to deploy to gh-pages
### Interpreter
- Write tests for the interpreter or give it a denotional semantics (would probably be a lot of work)
- Catch a division by 0
- Print out nicer error messages with line numbers
- Implement the Access Points and Channels over sockets etc to show real-world practicality
  using the interpreter over multiple machines
- Allow for sending of channels <!--(TODO look in LDST)-->
- Implement basic Datatypes such as Strings, Vectors of Types etc
As proposed in the paper:
- recursion and recursive datatypes through recursive types
- Coinductive subtyping for the recursor <!--S.18 Gay and Hole-->

## Personal Notes
This project exposed me to a lot of Haskell and Functional (Reactive) Programming concepts, which was quite new for me,
especially working with and debugging the monad transformer took me a long time.
Nix and reflex were also daunting at first, but going over the project to write this made me remove and simplify a lot of code and lead to a much better understanding.
The design of the interpreter and the frontend page can definitely be improved.

## References
- (1) Peter Thiemann and Vasco T. Vasconcelos. 2019. Label-dependent session types. Proc. ACM Program. Lang. 4, POPL, Article 67 (January 2020), 29 pages. DOI:https://doi.org/10.1145/3371135
- (2) Atsushi Igarashi, Peter Thiemann, Vasco T. Vasconcelos, and Philip Wadler. 2017. Gradual session types. Proc. ACM Program. Lang. 1, ICFP, Article 38 (September 2017), 28 pages. DOI:https://doi.org/10.1145/3110282
- (3) Guy L. Steele. 1994. Building interpreters by composing monads. In Proceedings of the 21st ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL ’94). Association for Computing Machinery, New York, NY, USA, 472–492. DOI:https://doi.org/10.1145/174675.178068
- (4) Sheng Liang, Paul Hudak, and Mark Jones. 1995. Monad transformers and modular interpreters. In Proceedings of the 22nd ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL ’95). Association for Computing Machinery, New York, NY, USA, 333–343. DOI:https://doi.org/10.1145/199448.199528
