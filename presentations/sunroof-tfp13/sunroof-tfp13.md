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

# What is Sunroof?

<ul>
<li class="fragment">Deep embedding of JavaScript in Haskell</li>
<li class="fragment">Foreign Function Interface to JavaScript</li>
<li class="fragment">
Platform for hybrid Haskell/JavaScript browser-based applications
</li>
</ul>

# Why JavaScript and Browsers?

<ul>
 <li class="fragment">Graphical canvases</li>
 <li class="fragment">Event handling</li>
 <li class="fragment">Available across platforms</li>
 <li class="fragment">Access through JavaScript</li>
</ul>

<p class="fragment">
We want to utilize the browser's capabilities!
</p>

# How does Sunroof look?

<div class="fragment">
JavaScript:

```javascript
var v0 = prompt("Your name?");
alert("Your name: " + v0);
```
</div>

<div class="fragment">
Sunroof:

```haskell
jsCode :: JS ()
jsCode = do
    name <- prompt "Your name?"
    alert ("Your name: " <> name)
```
</div>


# Types

<div class="fragment">
 * We use Haskell's type system to add static typing

```haskell
prompt :: JSString -> JS JSString
alert  :: JSString -> JS ()
```
</div>

<ul>
<li class="fragment">Types are wrappers of an untyped expression language</li>
<li class="fragment">and implement the `Sunroof` class</li>
<li class="fragment">Allows new types to be added later</li>
</ul>

# JS-Monad

```haskell
jsCode :: JS ()
jsCode = do
    name <- prompt "Your name?"
    alert ("Your name: " <> name)
```

<ul>
<li class="fragment">Captures side-effects and sequences of statements</li>
<li class="fragment">Binding is translated to assignment of fresh variable</li>
</ul>

<div class="fragment">
_How do we constrain it to JavaScript types?_

 * Normalize and reify the monad
</div>

# Threading Models

<div class="fragment">
Actually the `JS`-monad has another type parameter `t`:

```haskell
JS t a
```
</div>

<ul>
<li class="fragment">`t` determines the threading model used</li>
<li class="fragment">`t` can either be `A` or `B`</li>
</ul>

# Model A: Atomic

<ul>
<li class="fragment">The JavaScript threading model</li>
<li class="fragment">Callback centric</li>
<li class="fragment">One thread with event loop</li>
</ul>

# Model B: Blocking
<ul>
<li class="fragment">Adds cooperative concurrency to Sunroof</li>
<li class="fragment">Offers abstractions known from Haskell: 

  * `forkJS :: JS B () -> JS t ()`
  * `yield :: JS B ()`
  * `JSMVar` and `JSChan`

</li>
<li class="fragment">Implemented through translation of continuations to JavaScript</li>
</ul>

# Functions

```haskell
square :: JS t (JSFunction JSNumber JSNumber)
square = function $ \x -> return (x * x)
```

<div class="fragment">
```haskell
jsCode = do
  sqr <- square       -- Create / Bind
  n <- sqr `apply` 2  -- Use
  alert $ cast n      -- Output
```
</div>

<div class="fragment">
```haskell
function :: (...) => (a -> JS A r) 
                  -> JS t (JSFunction a r)
apply    :: (...) => JSFunction a r 
                  -> a -> JS t r
```
</div>

# Continuations

<ul>
<li class="fragment">Continuations needed for second threading model</li>
<li class="fragment">`JS`-monad is a continuation monad</li>
</ul>
<div class="fragment">
 * We have access to the underlying cont. in JavaScript

```haskell
callcc :: (...) => (JSContinuation a -> JS B a)
                -> JS B a
```
</div>

# Functions & Continuations

<ul>
<li class="fragment">Both are values in Haskell and JavaScript</li>
<li class="fragment">Allows direct translation to JavaScript</li>
<li class="fragment">Sunroof embeds this connection:

![](sunroof-func-cont.png)
</li>

# Calling JavaScript: Inline

<div class="fragment">
JavaScript:

```javascript
document.getElementById("n")
```
</div>
<div class="fragment">
becomes:

```haskell
object "document" # invoke "getElementById" "n"
```
</div>

<div class="fragment">
Types:

```haskell
object :: String -> JSObject
invoke :: (...) => String -> a -> o -> JS t r
(#)    :: o -> (o -> JS t r) -> JS t r
```
</div>

# Calling JavaScript: Bindings

<div class="fragment">
Creating a binding:

```haskell
alert :: JSString -> JS t ()
alert s = fun "alert" `apply` s
```
</div>

<div class="fragment">
Types:

```haskell
fun :: (...) => String -> JSFunction a r
```
</div>

# Server

<ul>
<li class="fragment">Allows to execute Sunroof code in the browser</li>
</ul>

<div class="fragment">
 * Ability to interleave Haskell and JavaScript

```haskell
syncJS  :: ... -> JS t a  -> IO (ResultOf a)
asyncJS :: ... -> JS t () -> IO ()
```
</div>

# Case Study: A small calculator

![The example application](example-application.png)

# Case Study: Structure

![](example-structure.png)

# Conclusion

<ul>
<li class="fragment">Generate JavaScript as needed in Haskell</li>
<li class="fragment">Type-safety through static types</li>
<li class="fragment">Foreign-function interface to JavaScript</li>
<li class="fragment">A threading model similar to Haskell's</li>
<li class="fragment">Excute JavaScript in the browser from Haskell</li>
</ul>







