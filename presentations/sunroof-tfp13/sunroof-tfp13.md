# Sunroof 
<h3>A Monadic DSL to Generate JavaScript</h3>
<p><small>15th May 2013</small></p>
<p>
  <small>Jan Bracker<sup>1,2</sup> and Andy Gill<sup>1</sup></small>
</p>
<p><small>
  <address>
    <sup>1</sup>
    ITTC / EECS<br />
    The University of Kansas<br />
    Lawrence, KS, 66045
  </address>
</small></p>
<p><small>
  <address>
    <sup>2</sup>
    Institut für Informatik<br />
    Christian-Albrechts-Universität<br />
    Kiel, Germany
  </address>
</small></p>

# Motivation

## Why JavaScript and Browsers?

<ul>
 <li class="fragment">Graphical canvases</li>
 <li class="fragment">Event handling</li>
 <li class="fragment">Available across platforms</li>
 <li class="fragment">Access through JavaScript</li>
</ul>

<p class="fragment">
We want to utilize the browser's capabilities!
</p>

## What is Sunroof?

<ul>
<li class="fragment">Deep embedding of JavaScript in Haskell</li>
<li class="fragment">Foreign Function Interface to JavaScript</li>
<li class="fragment">Platform for hybrid Haskell/JavaScript applications</li>
</ul>

## Features

<ul>
<li class="fragment">Types</li>
<li class="fragment">Haskell-style (cooperative) concurrency</li>
<li class="fragment">Ready to use server</li>
</ul>

## Example

```haskell
jsCode :: JS t ()
jsCode = do
    name <- prompt "Your name?"
    alert ("Your name: " <> name)
```

<div class="fragment">
Types:

```haskell
prompt :: JSString -> JS t JSString
alert  :: JSString -> JS t ()
```
</div>

<div class="fragment">
Produces:

```javascript
var v0 = prompt("Your name?");
alert("Your name: " + v0);
```
</div>

# How does Sunroof work?

# JS-Monad: `JS t a`

 * Captures side-effects and imperative nature of JavaScript
 * Offers two threading models, specified by `t`

## JS-Monad: Problem

How do we constrain it to JavaScript types?

 * Normalize monad through Operational
 * Allows use to constain involved types (Sculthorpe, 2013)


# Object Model

 * Untypes expression language
   
```haskell
data Expr = Var Id | Apply Expr [Expr] | ...
```
 
 * Types are wrappers of expressions
   that implement `Sunroof` class
   
```haskell
class Sunroof a where
  box   :: Expr -> a
  unbox :: a -> Expr
```
 
 * Allows new types to be added later (Svenningsson, 2012)

# Functions

 * Functions are values in Haskell and JavaScript
 * Sunroof embeds this connection:
   ![](sunroof-func-cont.png)
 * Allows direct translation to JavaScript
 * Continuations needed for second threading model
 * `JS`-monad is a continuation monad (Claessen, 1999)

# Threading Models

## Model A: Atomic
 * The JavaScript threading model
 * Callback centric
 * One thread with event loop

## Model B: Blocking
 * Adds cooperative concurrency to Sunroof
 * Offers abstractions known from Haskell: 
    * `forkJS` and `yield`
    * `MVar` and `Chan`
 * Implemented through translation of continuations to JavaScript

# Foreign Function Interface

<div class="fragment">
```javascript
document.getElementById("id")
```
</div>
<div class="fragment">
becomes:

```haskell
object "document" # invoke "getElementById" "id"
```

```haskell
object :: String -> JSObject
invoke :: (...) => String -> a -> o -> JS t r
(#)    :: o -> (o -> JS t r) -> JS t r
```
</div>

<div class="fragment">
Creating a binding:

```haskell
alert :: JSString -> JS t ()
alert s = fun "alert" `apply` s
```

```haskell
fun :: (...) => String -> JSFunction a r
```
</div>

# Compiler

![Structure of Sunroof](sunroof-structure.png)

Statment Datetype is Target / Translation of Branches

Leave this away?


# Server

 * Allows to execute Sunroof code in the browser

```haskell
syncJS  :: ... -> JS t a  -> IO (ResultOf a)
asyncJS :: ... -> JS t () -> IO ()
```
 
 * Ability to interleave Haskell and JavaScript


# Case Study: A small calculator

![The example application](example-application.png)

## Case Study: Structure

![](example-structure.png)

## Case Study: TODO: REMOVE THIS?

Statistics + Downsides

# Conclusion

 * Write JavaScript in Haskell
 * Type-safety
 * Foreign-function interface
 * Excute JavaScript from Haskell
 * Generate JavaScript when needed

# References

TODO







