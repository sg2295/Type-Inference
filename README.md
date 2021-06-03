## Type Inference
Type inference is the process of finding a type for a lambda-term. This project is an implementation of a simple and transparent version of Type Inference. The project can be broken down into five distinct sub-parts: _Types with variables, Substitutions, Unification, Derivations _and_ Type Inference_, each discussed further below.

### Types with variables
Types can be defined by the grammar: 
<p align="center"> <i>
  ρ, σ, τ, υ ::= α ∈ Α | σ -> τ
</i> </p>

Where _A = {α, β, γ, ...}_.

This definition is implemented by the _Type_ data type, which uses an infix type constructor `:->`. The patterns for the data type are: 

```Haskell
At x
x :-> y
```
In order to handle _types with variables_ the function `occurs` determines if an atom occurs in a given type and the function `findAtoms` returns the atoms occuring in a type in an alphabetically ordered list.

### Substitutions
Substitutions for types is simpler than substitutions for lambda-terms, since variable capture is not an issue, and as such there is no need for capture-avoidance. The definition of substitution for types is the following:

<p align="center"> <i>
  α[τ/α] = τ
</i></p><p align="center"> <i>
  β[τ/α] = β
</i></p><p align="center"> <i>
  (ρ->σ)[τ/α] = ρ[τ/α]->σ[τ/α]
</i></p>

A series of substitutions is abbreviated by _S_:

<p align="center"> <i>
  S = [τ<sub>1</sub>/α<sub>1</sub>]...[τ<sub>n</sub>/α<sub>n</sub>]
</i></p>

And _ε_ is the **empty** series of substitutions. _τ[S]_ is _τ_ with the substitution _S_ applied to it. So we have _τ[ε] = τ_. Using the above definition of _S_, we have:

<p align="center"> <i>
  τ[S] = τ[τ<sub>1</sub>/α<sub>1</sub>]...[τ<sub>n</sub>/α<sub>n</sub>]
</i></p>

Therefore a substitution _[τ/α]_ can be written as a pair `(a,t)` where *a* is an `Atom` and *t* is a `Type`. Then a series of substitutions is a list of pairs:

_τ[S] = τ[τ<sub>1</sub>/α<sub>1</sub>]...[τ<sub>n</sub>/α<sub>n</sub>]_ 

`[(an, tn), ..., (a1, t1)]`
 
 > Note that the order is reversed 

To deal with substitutions, the `sub` function applies a substitution to a given type and `subs` applies a list of substitutions to a type, applying the head of the list last and the tail first.
