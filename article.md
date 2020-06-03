# LDST

The paper by [Peter Thiemann and Vasco T. Vasconcelos](#references) gives an excellent introduction to the LDST calculus.
A good introductory explanation of Session Types is given in [2](#references).

This post documents a backend of LDST, written in Haskell.

## Goals of this article
This article provides documentation of the code (and ideas behind it) that makes up the backend of the
interpreter. It does not concern itself with the frontend (type checker) code,
which was provided by Peter Thiemann. Some parts of the code such as Declarations only for Typechecking, will be left out.

There will be two main parts:
The first one about Interpreter, the second one about creating a static site with our haskell executable.

## The Input : LDST
The custom Parser built by the tokenizer [Alex](https://www.haskell.org/alex/) and parser generator [Happy](https://www.haskell.org/happy/) takes
in a `.ldgv` file and outputs LDST declarations (here simplified):

```haskell
data Decl = DType TIdent Type  -- define a type with name
          | DFun Ident [Ident] Exp -- define a function with name, parameter list and Expression
          deriving (Show, Eq)
```
where `TIdent` and `Ident` are alias types for `String`.

There are also Declarations only for Typechecking, like Type equivalency checking. They get typechecked but not
interpreted by the backend.

The parsing of text into these Declarations is later [tested](#testing).

Let's take a look at available Expressions. First, we have basic types:
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
         | NatRec Exp Exp Ident TIdent Ident Type Exp
         | Rec Ident Ident Type Type Exp
  deriving (Show,Eq)
```

## Interpreters and monad transformers
The interpreter has to represent the value of expressions it is working on.
This `Value` type is an algebraic datatype of everything that ocurs in our programs, such as basic Types
```haskell
data Value = VUnit
      | VLabel String  -- Labels
      | VInt Int  -- for simplicity, we use Int for Integers and natural numbers
      | VPair Value Value -- pair of values (or of ids that map to values)
```
functions
```haskell
      | VFun (Value -> InterpretM) -- Function Type TODO move to later part when monad is defined
```
and our representation of Channels with haskells `Chan`nels out of [Control.Concurrent.Chan](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Concurrent-Chan.html) (which themselves are wrappers around `MVars`).
We use two channels, one for reading and one for writing to the other, so we do not read our own written values. Instead reading gets blocked when reading in case the other side did not write anything.
```haskell
      | VChan (C.Chan Value) (C.Chan Value)
      | VDecl S.Decl -- when an identifier maps to another function we have not yet interpreted
```
Next to storing variable bindings we want to send and receive `Values` to create the basic Session Type functionality.
So we have to allow for operations on `Chan`s to occur in the `IO` monad.
We can solve this by stacking a `ReaderT` and `IO` in a monad transformer.

### Enter monad transformers
Monad transformers behave like stacks of monads where each monad layer provides its own functionality.
Instead of using a `Reader r a` with environment `r` and return type `a`, we use the constructor `ReaderT r m a` together with `runReaderT :: ReaderT r m a -> r -> m a`, which 
still allows for environment `r` and return value `a`, but this time inside a monad of our choosing - here we use `IO`.

<!--Later on we will replace `IO` with JSaddles `JSM` (that implements `MonadIO` and thus provides `liftIO`) to run a
We use the `mtl` to wrap a `ReaderT` around the IO monad, used to keep track of current variable bindings-->
```haskell
-- | the interpretation monad
type InterpretM = R.ReaderT Env IO Value  -- TODO replace with JSM

-- | maps identifiers to Values of expressions and stores
type Env = [EnvEntry]
type EnvEntry = (String, Value)
```

Now that we can represent the Values of Expressions, we can move on to define basic interpretation.
First, we want a function that maps Expressions to Values inside out transformer:
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
  Var s -> envlookup s
```
which gives us an error message if we try to use a variable which has no `Value` bound to it.
```haskell
envlookup :: String -> InterpretM
envlookup id = do
    identifiers <- ask
    case lookup id identifiers of
        Nothing -> fail ("No Value for identifier " ++ id ++ " in Environment")
        Just val -> return val
```

To avoid repetition we use a helper for mathematical functions
```haskell
  Plus e1 e2 -> mathHelper (+) e1 e2
  Minus e1 e2 ->  mathHelper (-) e1 e2
  Times e1 e2 ->  mathHelper (*) e1 e2
  Div e1 e2 ->  mathHelper quot e1 e2
  Negate e1 ->  mathHelper (-) (Int 0) e1
  Succ e -> mathHelper (+) (Int 1) e
```

```haskell
mathHelper op e1 e2 = do
    v1 <- interpret' e1
    v2 <- interpret' e2
    return $ case (v1, v2) of
      (VInt a, VInt b) -> VInt (op a b)
```
To assign a value of expression `e1` to a variable `s` we simply prepend it to our Environment in the `interpret'`ation of the inner expression `e2`
```haskell
  Let s e1 e2 -> do
      v  <- interpret' e1
      R.local ((s, v):) $ interpret' e2
```
The same way a pair is unpacked, this time with two values added to our `env`
```haskell
  LetPair s1 s2 e1 e2 -> do
      -- simple pair unpacking
      v <- interpret' e1
      case v of
        (VPair v1 v2) -> R.local (\env -> (s1, v1):(s2, v2):env) $ interpret' e2
```

We can also have dependent pairs, where the result of e1 can be used in the interpretation of e2
```haskell
  Pair mul s e1 e2 -> do
      v1 <- interpret' e1
      v2 <- R.local ((s, v1) :) $ interpret' e2
      return $ VPair v1 v2
```
As convenience we can access the first and second value in a pair with `fst` and `snd`
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
Finally, we have some functions. They are represented by a Type that holds a Closure `f` which maps the identifier `i` to an
argument `arg` and then interprets the body of the function `e`.

TODO if binding of env like this makes sense, or should be resolved at invocation time
```haskell
  Lam m i t e -> do
                 env <- ask 
                 let f = \arg -> do
                                 liftIO $ R.runReaderT (interpret' e) ((i, arg):env)
                 return $ VFun f
```
Applying a function to a Value is now done one Argument at a time.
Functions with multiple arguments get curried to intermediate `VFun`s themselves.
```haskell
  App e1 e2 -> do
      arg <- interpret' e2
      v <- interpret' e1
      -- check if the variable refers to a function label
      case v of
          VDecl d -> do  -- if it is a declaration, we resolve the function name
            res <- evalDFun d
            case res of
              (VFun f) -> do
                f arg
          VFun f -> do  -- if not, we can apply the function directly
            f arg
          _ -> do
            fail $ "Trying to Apply " ++ show e2 ++ " to " ++ show e1
```
### Session Type functionality
The backend does not implement dynamic type checking for session type channel. <!--s, as done in the [simple-sessions](https://hackage.haskell.org/package/simple-sessions) package. -->
Instead it relies on the type checker statically checking all code for violations.
The main functionality is given by an access point, a pair of dual Channels. When writing to (reading from) one side it is expected that the dual part in program reads (wrote to) it and vice versa.
To create a new access point, we use the `new` keyword, construct a channel for each direction and simply pack it into a pair.
```haskell
  New t -> do
    r <- liftIO Chan.newChan
    w <- liftIO Chan.newChan
    return $ VPair (VChan r w) (VChan w r)
```
Because the expression for `send` does not give us the argument, we return a closure that writes to and then returns the channel.
The expression `(send channelname)` is then used as a function TODO
```haskell
  Send e -> do
      v <- interpret' e
      case v of
        (VChan _ c) -> return $ VFun $ \arg -> do
                                        liftIO (Chan.writeChan c arg)
                                        return v
```
Receiving is simply reading fr
```haskell
  Recv e -> do
      v <- interpret' e
      case v of
        (VChan c _) -> do
          val <- liftIO $ Chan.readChan c
          return $ VPair val v
  Fork e -> do
      penv <- ask
      liftIO $ forkIO (do
                      res <- runReaderT (interpret' e) penv
                      C.traceIO "Ran a forked operation")
      return VUnit
```
One special feature of LDST is a type-level recursor: it allows for implementing G
```haskell
  exp@(NatRec e1 e2 i1 t1 i2 t2 e3) -> do
  -- returns a function indexed over e1 (should be a variable pointing to a Nat)
  -- e1 should evaluate to the recursive variable which gets decreased each time the
  -- non-zero case is evaluated
  -- e2 is the zero case
  -- e3 is the nonzero case
         i <- interpret' e1
         case i of
                 VInt 0 -> interpret' e2
                 VInt 1 -> do
                             zero <- interpret' e2
                             local (\env -> (i1, VInt 1):(i2, zero):env) $ interpret' e3
                 VInt n -> do
                        -- interpret the n-1 case i2 and add it to the env
                        -- together with n before interpreting the body e3
                        let newexp = NatRec (Var i1) e2 i1 t1 i2 t2 e3
                        lower <- local ((i1, VInt (n-1)):) $ interpret' newexp
                        local (\env -> (i1, VInt n):(i2, lower):env) $ interpret' e3
 
```
The function to evaluate references to other Declarations `VDecl d` can simply interpret when we have no free variables:

```haskell
-- | interpret a DFun (Function declaration)
evalDFun :: Decl -> InterpretM
evalDFun decl@(DFun name [] expression _) = interpret' expression
```
but for functions with free variables, we construct a `VFun` for each parameter `id` in the parameter list `binds`, like
we did for Lambda binding
```haskell
evalDFun decl@(DFun name ((_, id, _):binds) e mty) = do
                          env <- ask
                          let inner = DFun name binds e mty
                          let f = \arg -> do
                                 liftIO $ R.runReaderT (evalDFun inner) ((id, arg):env) 
                          return $ VFun f
```
## Moving to the web - compiling Haskell to Javascript
To compile the haskell application to client-side Javascript we use reflex and
reflex-dom, which use the ghcjs compiler together with the nix paket manager.
The resulting static site can then be easily hosted by GitHub-Pages.

Reflex follows a programming pattern called [Functional Reactive Programming](TODO), which is best explained by the [Queensland Functional Programming Lab](https://qfpl.io/projects/reflex)

The Environment this runs in is the `JSM`, which luckily is an instance of `MonadIO` and thus provides `lifIO` which we can use to run the main `interpret`ation of source code. 

We then build a very basic page with two main text fields, one with the HTML id `tSrc` for the input

```haskell
elAttr "textarea" ("id" =: "tSrc" <> "class" =: "source_textarea" <>"spellcheck" =: "false") $ dynText $ fmap (T.pack.lookupExample) dVal
```
which uses a `dynText` as inner content, which is built by looking up the example file described by the dropdown.
For output the same method is used, but it is set readonly. 

```haskell
elAttr "textarea" ("id" =: "tOutput" <> "class" =: "output_textarea" <> "readonly" =: "readonly" <> "spellcheck" =: "false") blank
```
We can then redirect output which used to go to stdout to our textbox

There is most likely another way by using a `WriterT` around the `ReaderT` to store output, but appending text makes it more interactive
(and easier to understand, coming from a non-functional programming background).

```haskell
printLn :: String -> JSM ()
printLn s = do
    ...
    val <- eval $ T.concat ["document.getElementById('tOutput').value += '", t, "\\n'"]
    ...
```
Upon a `_dropdown_change` of the example-dropdown `d`, we set the source text with a simple JSaddle 
javascript evaluation

```haskell
setSrc :: String -> JSM ()
setSrc = setHtmlElement "tSrc"

setHtmlElement :: String -> String -> JSM ()
setHtmlElement ident s = do
    _ <- eval (T.pack $ "document.getElementById('" ++ ident ++ "').value = " ++ (show s))
    pure ()
```

TODO: explain event and src text
Wen our interpretation button ` ` is pressed, we run our `Interpreter`

```haskell
-- get the text inside the area on button click
srcText <- performEvent $ ffor e $ (const $ liftJSM $ textareaget)

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
the output of type `Event String` is then used to update a `Dynamic Text` and displayed.
```haskell
holdDyn :: a -> Event a -> m (Dynamic a)
```
```haskell
output <- holdDyn "" $ fmap T.pack doneEv
elAttr "p" ("id" =: "tResult") $ dynText output
```

# Testing
Testing the parser is done using [hspec]() with automatic discovery as described in here.
To build the tests, we can use ghc inside a nix shell

```bash
nix-shell -A shells.ghc  # use the nix shell for our dependencies
cabal configure --ghc  # make sure we use ghc as compiler, not ghcjs
cabal test  # build and run the tests
```

## Future improvements
### process
- ~~Use bazel to build iteratively, not on package-level~~ we can build iteratively in the nix shell using cabal instead of building the whole nix package
- remove unnescessary javascipt, for example with the google closure compiler
- use CI to deploy to gh-pages and run tests
### Interpreter
- Catch a division by 0
- Print out nicer error messages with line numbers
- Implement the Access Points and Channels over sockets etc to support
  using the interpreter over multiple machines
- Allow for sending of channels (TODO look in LDST)
- Implement basic Datatypes such as Strings, Vectors of Types etc
Proposed in the paper:
- recursion and recursive datatypes through recursive types
- coinductive subtyping for the recursor S.18 Gay and Hole

## Personal Notes
This project exposed me to a lot of haskell concepts, which was quite new for me.
Especially working with and debugging the monad transformer took me a long time.
Nix and reflex were also 

## References
(1) Peter Thiemann and Vasco T. Vasconcelos. 2019. Label-dependent session types. Proc. ACM Program. Lang. 4, POPL, Article 67 (January 2020), 29 pages. DOI:https://doi.org/10.1145/3371135
(2) Atsushi Igarashi, Peter Thiemann, Vasco T. Vasconcelos, and Philip Wadler. 2017. Gradual session types. Proc. ACM Program. Lang. 1, ICFP, Article 38 (September 2017), 28 pages. DOI:https://doi.org/10.1145/3110282
