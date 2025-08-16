+++
title = "Typeclasses in Scala3"
date = "2025-08-10"

[taxonomies]
tags=["scala","advanced","compile-time", "typelevel"]
+++

## Introduction

There's not a whole lot of content that explains typeclasses in Scala well, especially from the point of why we should bother using them. Most concepts in functional programming aren't immediately obvious. As an example, it
can be difficult to reason about why certain abstractions are useful when, in short snippets, they may look either terse or even boilerplate code. In this article, I aim to provide some evidences of why typeclasses can be valuable and where they can be applied.

Let's assume we need to design a user-facing json library that exposes a clean and ergonomic API for serializing data. To this end will implement and compare three different ways to serialize JSON to a string in Scala.

For pragmatic reasons, we will design and serialize a JSON dialect that doesn’t necessarily adhere to the official specification, but it will be sufficient for our use case. We will start by modeling our type-level definitions (ADT's), which will subsequently compose into an AST that we will provide manually, and our interpreter will then convert it into a string. This blog isn’t a JSON parsing tutorial, so we won’t be covering that.

A spec of the ***values*** our JSON implementation will encode is as follows.

- Integer
- String
- Boolean
- Null
- Array
- Object

## Part 1 - The Naive OOP Way

{{ note(header="Assumptions", body="This part assumes that you are familiar with the basic scala syntax and have some functional programming experience.") }}

We'll start by modelling a trait `JsonExpression`, containing an abstract method that returns the string representation of the Json.

```scala
trait JsonExpression:
  def stringify: String
```

We then progressively add `JsonInt`, `JsonString`, `JsonBool` as case classes, and `JsonNull` as a singleton case object all of which extend the `JsonExpression` trait

```scala
case class JsonInt(value: Int) extends JsonExpression:
  override def stringify = value.toString

case class JsonString(value: String) extends JsonExpression:
  override def stringify = "\"" + value + "\"" // Json Strings are surrounded by quotes

case class JsonBool(value: Boolean) extends JsonExpression:
  override def stringify = value.toString

case object JsonNull extends JsonExpression:
  override def stringify = "null"
```  

Now, that we have implemented 4 out of the 6 types, we will compose such simple types into more complex types such as `JsonArray` and `JsonObject` as follows -


```scala
case class JsonArray(values: List[JsonExpression]) extends JsonExpression:
  override def stringify = values.map(_.stringify).mkString("[", ",", "]") 

case class JsonObject(values: Map[String, JsonExpression]) extends JsonExpression:
  override def stringify = 
    values
      .map((key, value) => "\"" + key + "\":" + value.stringify)
      .mkString("{", ",", "}")
```

We are finally done with modelling our JSON via an ADT ☺️, 

Now before proceeding further let's build a very simple AST representing our ADT and evaluate it to verify it's functionality.

```scala
val expression: JsonExpression = JsonObject(
  Map(
    "numbers" -> JsonArray(List(JsonInt(1), JsonInt(2), JsonInt(3))),
    "strings" -> JsonArray(
      List(JsonString("a"), JsonString("b"), JsonString("c"))
    )
  )
)

// {"numbers":[1,2,3],"strings":["a","b","c"]}
println(expression.stringify)
```

Congrats on reaching upto here, we hope you've been following along. Either on your local machine or on the interactive [scastie](https://scastie.scala-lang.org/).
There's a lot more to come so buckle-up.

Now, let's assume we have some class and we wish to serialize it, say for example a class that models a person, i.e `class Person(name, age)`. How can we do it? One way of doing it is to simply extend the `JsonExpression`
trait and implement it's `stringify` method, which would look like - 

```scala
case class Person(name: String, age: Int) extends JsonExpression:
  override def stringify =
    JsonObject(Map("name" -> JsonString(name), "age" -> JsonInt(age))).stringify

// We can now serialize the object to a Json string
val person = Person("Bob", 42)

// {"name":"Bob","age":42}
println(person.stringify)
```

Arguably, there's atleast two things that ***feel*** very wrong about this, 

1. There's very tight coupling between the `Person` class and the `JsonExpression` as evident by the implementation.

2. We're conceding the fact that `Person <: JsonExpression` which doesn't feel natural at all. Ideally a data carrier shouldn't know how to serialize itself.

Let's try to solve the above mentioned problems -

The first problem can be mitigated by not allowing users of the `JsonExpression` trait to extend it arbitrarily. In addition we can make the case classes `final` in scala to avoid it being extended as well. Scala offers the `sealed` keyword for traits, which prohibits extending the trait outside the compilation unit (i.e. outside the source file where it is defined).

Another benefit of this is that it allows the compiler to perform exhaustive checks at compile time for pattern matching. Because all subclasses of a sealed trait must be defined within the same compilation unit, the compiler knows the complete set of possible cases and can warn the developer if any case is not handled.

So let's add the `sealed` and `final` modifers on our traits and case classes respectively, So in the end, our code would look like this -

```scala
sealed trait JsonExpression:
  def stringify: String

case object JsonNull extends JsonExpression:
  override def stringify = "null"

final case class JsonInt(value: Int) extends JsonExpression:
  override def stringify: String = value.toString

final case class JsonBool(value: Boolean) extends JsonExpression:
  override def stringify: String = value.toString

final case class JsonString(value: String) extends JsonExpression:
  override def stringify: String = s""""$value""""

final case class JsonArray(value: List[JsonExpression]) extends JsonExpression:
  override def stringify: String =
    value.map(_.stringify).mkString("[", ",", "]")

final case class JsonObject(value: Map[String, JsonExpression]) extends JsonExpression:
  override def stringify: String = value
    .map((key, value) => "\"" + key + "\":" + value.stringify)
    .mkString("{", ",", "}")
```

A way to solve the second problem i.e to *decouple* the classes from the serialization logic is to, basically do heuristics as such.

```scala
def toJsonExpression(value: Any): JsonExpression
```

As we can tell from the signature itself, the method takes a `value: Any` and returns a `JsonExpression`, the implementation would look like this for some example classes.

```scala
// Let's assume we need to serialize these data types
case class Person(name: String, age: Int)
case class JobRole(id: Int, applicants: List[Person])

def toJsonExpression(value: Any): JsonExpression =
  value match
    case Person(name, age) =>
      JsonObject(Map(
        "name" -> JsonString(name),
        "age"  -> JsonInt(age)
      ))

    case JobRole(id, applicants) =>
      JsonObject(Map(
        "id"         -> JsonInt(id),
        "applicants" -> JsonArray(applicants.map(toJsonExpression))
      ))
    
    case _ => JsonNull // Or throw some exception, since we defined a JsonNull might as well go with that

val expression = toJsonExpression(
  JobRole(
    42,
    List(Person("Bob", 52), Person("Steve", 96), Person("John", 120))
  )
)

// {"id":42,"applicants":[{"name":"Bob","age":52},{"name":"Steve","age":96},{"name":"John","age":120}]}
println(expression.stringify)
```

On first glance this looks like a much cleaner alternative without either `Person` or `JobRole` subtyping into the `JsonExpression` trait, and for the most part it is.

It is certainly much better than our previous implementation where we employed subtyping.

<!-- A major code smell here is `Any` in the signature of `toJsonExpression`. This causes us to lose type-safety and the compiler will not complain when I try to pass in arbtrary types. -->
<!---->
<!-- There is another thing to look out as well, which might give a false sense of safety to the uninitiated, is that it might look like the code will always default to `JsonNull` however that's not necessarily true, due to something called [type erasure](https://en.wikipedia.org/wiki/Type_erasure). -->
<!---->
<!-- So, if we pass something other than `List[Person]` the runtime has no way to know, and it won't fall back into `JsonNull` either, because there is no difference between `List[Person]` and `List[User]`to the underlying runtime, and both of them would be caught by our second match, not necessarily ever falling into the last match, which can have us end up getting `ClassCastException` at runtime. -->
<!---->
<!-- It's always best to be careful and concerned whenever you see `Any` and/or pattern matching over generic containers. -->

Now, let's see the pain our library user has to go through -

1. Huge lack of type-safety because of the signature containing `Any`. We can ***very*** easily pass innocent looking code which will result in a `ClassCastException` at runtime.

2. For every class that the user wishes to serialize, he has to painstakingly add additional serialization logic to the pattern match, which is already unsafe in the first place.

With that, we're at the end of Part-1, and I hope you were able to make sense of it, we only used basic scala syntax and hopefully were able to make clear the pitfalls of the approach. Typeclasses in part-2 will
allow us to write type-safe code.

## Part 2 - Enter Typeclass

{{ note(header="Assumptions", body="This part assumes that you are familiar with Scala3's generics and [contextual abstractions](https://docs.scala-lang.org/scala3/reference/contextual/). Typeclasses in scala are *emulated* via [Given](https://docs.scala-lang.org/scala3/reference/contextual/givens.html) and [Using clauses](https://docs.scala-lang.org/scala3/reference/contextual/using-clauses.html). It is best to learn/refresh those concepts as in this part we will employ the use of them quite heavily.") }}


Alright, we've made it to the second part of the article, and before we introduce typeclasses we'd like to improve on our `JsonExpression` type, which currently looks like this - 

```scala
sealed trait JsonExpression:
  def stringify: String

case object JsonNull extends JsonExpression:
  override def stringify = "null"

final case class JsonInt(value: Int) extends JsonExpression:
  override def stringify: String = value.toString

final case class JsonBool(value: Boolean) extends JsonExpression:
  override def stringify: String = value.toString

final case class JsonString(value: String) extends JsonExpression:
  override def stringify: String = s""""$value""""

final case class JsonArray(value: List[JsonExpression]) extends JsonExpression:
  override def stringify: String =
    value.map(_.stringify).mkString("[", ",", "]")

final case class JsonObject(value: Map[String, JsonExpression]) extends JsonExpression:
  override def stringify: String = value
    .map((key, value) => "\"" + key + "\":" + value.stringify)
    .mkString("{", ",", "}")
```

Even though the above is a valid ADT, it isn't a *pure value container*, as in the above snippet doesn't just carry data, it also carries the behavior which acts on the data. Let's refactor this and move the behavior outside the ADT and use pattern match to implement `stringify`.

```scala
sealed trait JsonExpression
case object JsonNull extends JsonExpression
final case class JsonString(value: String) extends JsonExpression
final case class JsonInt(value: Int) extends JsonExpression
final case class JsonBool(value: Boolean) extends JsonExpression
final case class JsonArray(value: List[JsonExpression]) extends JsonExpression
final case class JsonObject(value: Map[String, JsonExpression]) extends JsonExpression

def stringify(json: JsonExpression): String =
  json match
    case JsonNull          => "null"
    case JsonString(value) => s""""$value""""
    case JsonInt(value)    => value.toString
    case JsonBool(value)   => value.toString
    case JsonArray(value)  => value.map(stringify(_)).mkString("[", ",", "]")
    case JsonObject(value) =>
      value
        .map((key, value) => "\"" + key + "\":" + stringify(value))
        .mkString("{", ",", "}")
```

Here, we separated the behavior from our ADT, however this comes with a few DX (Developer Experience) drawbacks, notice that now you have to do `stringify(expression)` instead of `expression.stringify`, which easily becomes clunky as soon as you add more of those and start nesting them. To resolve this, we will use the Scala3 `extension` methods, so we rewrite the above as follows -

```scala
sealed trait JsonExpression
case object JsonNull extends JsonExpression
final case class JsonString(value: String) extends JsonExpression
final case class JsonInt(value: Int) extends JsonExpression
final case class JsonBool(value: Boolean) extends JsonExpression
final case class JsonArray(value: List[JsonExpression]) extends JsonExpression
final case class JsonObject(value: Map[String, JsonExpression]) extends JsonExpression

extension (json: JsonExpression)
  def stringify: String =
    json match
      case JsonNull          => "null"
      case JsonString(value) => s""""$value""""
      case JsonInt(value)    => value.toString
      case JsonBool(value)   => value.toString
      case JsonArray(value)  => value.map(_.stringify).mkString("[", ",", "]")
      case JsonObject(value) =>
        value
          .map((key, value) => "\"" + key + "\":" + value.stringify)
          .mkString("{", ",", "}")
```

This now looks pretty decent, however there's more that the scala compiler allows us to do. `sealed` traits and `final case` classes are in fact such a common pattern, that scala3 introduces new syntax sugar in the form of `enums` to define such [ADT's](https://docs.scala-lang.org/scala3/book/types-adts-gadts.html) without the additional boilerplate. You would rewrite the above snippet as follows - 

```scala
enum JsonExpression:
  case JsonNull
  case JsonString(value: String)
  case JsonInt(value: Int)
  case JsonBool(value: Boolean)
  case JsonArray(value: List[JsonExpression])
  case JsonObject(value: Map[String, JsonExpression])

import JsonExpression.*

extension (json: JsonExpression)
  def stringify: String =
    json match
      case JsonNull          => "null"
      case JsonString(value) => s""""$value"""" and start nesting them
      case JsonInt(value)    => value.toString
      case JsonBool(value)   => value.toString
      case JsonArray(value)  => value.map(_.stringify).mkString("[", ",", "]")
      case JsonObject(value) =>
        value
          .map((key, value) => "\"" + key + "\":" + value.stringify)
          .mkString("{", ",", "}")
```

Now we have a well defined ADT and an extension method along with it which supplies the behavior to our types. With all that out of the way, we are now ready to get started with typeclasses, notice we didn't get into serializing custom classes like `Person` or `User` till now, I wanted to make sure that we setup a strong base and a clear intentional Sum-type ADT model instead of the boilerplate we had before.

Typeclasses are a way to achieve ad-hoc polymorphism that allows you write functions and APIs that work for many types, but without ***forcing*** those types into a common inheritance hierarchy. Instead, you define capabilities (in the form of traits) and provide implementations separately via given instances, often outside the type’s original definition.

They are a compile-time resolved, opt-in, non-invasive way of achieving polymorphism, where behavior is dispatched not by the object inheritance tree but by the presence of a given implementation in scope.

All in all, what a typeclass really requires is a generic trait and corresponding given instances that implement the capabilities for that trait.

Now, let's define a generic `JsonEncoder` trait - 

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression
```

Now that we’ve defined the JsonEncoder trait, we can provide given instances for specific types. For example, for the `Person` type:

```scala
// case class Person(name: String, age: Int)

given JsonEncoder[Person] with
  def encode(value: Person) = JsonObject(
    Map(
      "name" -> JsonString(value.name),
      "age" -> JsonInt(value.age)
    )
  )
```

Note that when a trait accompanies a SAM (Single Abstract Method), Scala allows you to rewrite them as values as such -

```scala
// case class Person(name: String, age: Int)

given JsonEncoder[Person] = 
  (value: Person) => JsonObject(
    Map(
      "name" -> JsonString(value.name),
      "age" -> JsonInt(value.age)
    )
  )
```

We'll be using this so our code looks much neater and concise. With this capability in place, we can also define a generic method that works for any type `A` with `JsonEncoder` in scope.

Its signature would be -

```scala
def toJson[A](value: A)(using JsonEncoder[A]): JsonExpression
```

Note that this `using` clause requires a given instance of `JsonEncoder[A]` to be available in the scope.

{{ note(header="Note", body="
Scala allows the use of haskell-like syntax instead of `using` via something called [***context bounds***](https://docs.scala-lang.org/scala3/book/ca-context-bounds.html), we won't be using this syntax in our article, but it's helpful to know if the syntax is found in the wild.
```scala
def toJson[A: JsonEncoder](value: A): JsonExpression
```
") }}

Before implementing the method, we need a generic way to access the contextual `given` instance of `JsonEncoder[A]`. We do this by calling `summon` (or `implicitly` in Scala2), which directs the compiler to provide the appropriate instance from the current scope. 

The implementation would look like this -

```scala
def toJson[A](value: A)(using JsonEncoder[A]): JsonExpression = 
  summon[JsonEncoder[A]].encode(value)
```
Note that `summon[JsonEncoder[A]]` is simply a way to ***retrieve*** or `summon` the given instance so you can could `.encode` on it.


Now that we have implemented the `toJson` method, let's define one more given instance for the `JobRole` type and write out the typeclass we've created so far.

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression

// case class Person(name: String, age: Int)
given JsonEncoder[Person] = 
  (value: Person) => JsonObject(
    Map(
      "name" -> JsonString(value.name),
      "age" -> JsonInt(value.age)
    )
  )

// case class JobRole(id: Int, applicants: List[Person])
given JsonEncoder[JobRole] =
  (value: JobRole): JsonExpression =>
    JsonObject(
      Map(
        "id" -> JsonInt(value.id),
        "applicants" -> JsonArray(
          value.applicants.map(summon[JsonEncoder[Person]].encode)
        )
      )
    )

def toJson[A](value: A)(using JsonEncoder[A]) =
  summon[JsonEncoder[A]].encode(value)

val jobRole = JobRole(
  42,
  List(Person("Bob", 52), Person("Steve", 96), Person("John", 120))
)

// {"id":42,"applicants":[{"name":"Bob","age":52},{"name":"Steve","age":96},{"name":"John","age":120}]}
println(toJson(jobRole).stringify)
```

In Scala, when we need a specific given instance, we often invoke `summon` at the call site. This can be avoided by naming the context parameter in the `using` clause, which allows us to treat it like a normal method parameter.

For example, instead of summoning the instance explicitly, we can define the method as - 

```scala
def toJson[A](value: A)(using jsonEncoder: JsonEncoder[A]): JsonExpression = 
  jsonEncoder.encode(value)
```
Here, jsonEncoder is a named context parameter, so we can invoke its methods directly without explicitly summoning it.

Similarly, We can also name the `given` instances themselves. This makes them directly accessible in scope and easier to refer to when needed:

```scala
given personEncoder: JsonEncoder[Person] =
  (value: Person) => JsonObject(
    Map(
      "name" -> JsonString(value.name),
      "age" -> JsonInt(value.age)
    )
  )
```
With a named given like personEncoder, we can either let Scala resolve it implicitly or refer to it explicitly by name if we want to pass it around as a value.

Now after making these changes and making `toJson` an extension method while improving the user facing api, we end up with this -

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression

// case class Person(name: String, age: Int)
given personEncoder: JsonEncoder[Person] =
  (value: Person) => JsonObject(
    Map(
      "name" -> JsonString(value.name),
      "age" -> JsonInt(value.age)
    )
  )

// case class JobRole(id: Int, applicants: List[Person])
given JsonEncoder[JobRole] =
  (value: JobRole): JsonExpression =>
    JsonObject(
      Map(
        "id" -> JsonInt(value.id),
        "applicants" -> JsonArray(
          value.applicants.map(personEncoder.encode)
        )
      )
    )

extension [A](value: A)(using jsonEncoder: JsonEncoder[A])
  def toJson : JsonExpression =
    jsonEncoder.encode(value)
  def stringify: String = // We define an additional method to improve the User API
    toJson.stringify
```

Now let's take our typeclass for a spin, and see how we can use them - 

```scala
val jobRole = JobRole(
  42,
  List(Person("Bob", 52), Person("Steve", 96), Person("John", 120))
)

// {"id":42,"applicants":[{"name":"Bob","age":52},{"name":"Steve","age":96},{"name":"John","age":120}]}
println(jobRole.toJson.stringify)

// {"id":42,"applicants":[{"name":"Bob","age":52},{"name":"Steve","age":96},{"name":"John","age":120}]}
println(jobRole.stringify)

// {"name":"Steve","age":36}
println(Person("Steve", 36).stringify)

// JsonObject(Map(name -> JsonString(Links), age -> JsonInt(17)))
println(Person("Links", 17).toJson)
```

The important part here is that we're able to use `.toJson` and `.stringify` without entering into any subtyping relationship, note that this isn't as seamless as we would like to be, we still have to define given instances
for the types that need to be serialized, if the compiler can't find the given instances corresponding to the type requested, it will throw a compile-time error.

Now let's talk about what changed between this iteration and the version we wrote in Part-1

1. We haven't entered into any subtyping relationship.
2. We now have very strong type guarantees.
3. We still have to manually provide `given` instances for our classes.

The third point is addressed by typeclass derivation which we'll learn about in Part-3

## Part 3 - Typeclass derivation

{{ note(header="Assumptions", body="This part assumes that you are *very* comfortable with scala as well as familiar with Scala3's `inline` keyword and all the various places it can be used for example in pattern matching.")}}

Congrats on making it to the final part! As a fellow learner, I’ll try to explain typeclass derivation the way I wish someone had explained to me a couple of months ago.

Before we get into typeclass derivation, I would like to talk a bit about Algebraic Data Types (ADT's).

They are primarily of two types

- Sum Types
- Product Types

We will first explore these and see what they mean. 

In type theory and programming languages, Product types are types that combine multiple values into a single composite value, where each component is present and accessible whereas Sum types represent a choice between multiple alternatives, where exactly one of the possible variants is present at any given time.

More formally,

A product type $A × B$ is the cartesian product of types $A$ and $B$, meaning that a value of type $A × B$ is a pair $(a, b)$ where $a: A$ and $b: B$.

The simplest product types that you and I are familiar in scala are tuples!

```scala
("Shack", 79, false)

// case classes also count as product types
case class Restaurant(name: String, address: String, zipcode: Int)
```
Conversely, a sum type $A + B$  is a type whose value is either an $A$ or a $B$, but not both at the same time.

Sum types in scala are primarily represented by `sealed traits` and `case classes` or in Scala3 `enums` as well.

{{ note(header="Note", body="
You can convert `case classes` into tuples and vice versa via the `Mirror` typeclass. We'll talk about this later in this part since it is very useful to us.
") }}


```scala
enum Shape:
  case Square
  case Circle
  case Triangle

// These also count as sum types
sealed trait Shape
case object Square extends Shape
case object Circle extends Shape
case object Triangle extends Shape

// These as well but not suitable for our purposes so we'll not use these
type Shape = Square | Circle | Triangle
```

Now that we discussed this, let's come back to code land.

Let's take a look at the `JsonEncoder[Person]` given instance

```scala
// case class Person(name: String, age: Int)
given personEncoder: JsonEncoder[Person] =
  (value: Person) => JsonObject(
    Map(
      "name" -> JsonString(value.name),
      "age" -> JsonInt(value.age)
    )
  )
```

The above example requires us to hardcode the "labels" for each case class, as evident by the `"name"` and `"age"`, If you've used a java json library like Gson before, you'd know that you don't really need to care about the field names or labels of your object, you can most of the time just hand in the object, and the library can find the field names via reflection at runtime and proceed to serialize the data.

However, this method breaks type-safety as the compiler has no way to know what's being accessed, invoked or being casted, therefore it is unable to guarantee any correctness.

What we need is a way to preserve type-safety at compile-time and not having to write the boilerplate of multiple `given` instances for each and every possible case class we wish to serialize. This is accomplished via typeclass derivation.

***Typeclass derivation is a way to automatically synthesize `given` instances for our typeclasses instead of providing it ourselves.***

To solve this and other similar problems, Scala 3’s metaprogramming features allow compile-time inspection of a type’s structure, including field names and types. This enables generating serializers that adapt to a case class definition without hardcoding any labels (as we did in the above `given` instance), preserving type safety and avoiding runtime penalty via reflection.

Metaprogramming techniques in scala primarily fall in two categories -

1. Typelevel Computations
2. Macros

We will primarily focus on typelevel computations in this blog, the goal is also accomplished via Scala [macros](https://docs.scala-lang.org/scala3/guides/macros/macros.html) but among other things I do not have sufficient confidence to write about it at this time.

Scala3's standard library includes a typeclass `Mirror` in the `scala.deriving` package that provides *compile-time reflection* specifically for Product and Sum types.

Quoting the [documentation](https://www.scala-lang.org/api/current/scala/deriving/Mirror.html), ***Mirrors allows typelevel access to enums, case classes and objects, and their sealed parents.*** Reading the documentation is highly recommended as well as poking through the [source code](https://github.com/scala/scala3/blob/3.7.2/library/src/scala/deriving/Mirror.scala#L5).

Let's try to play around with the library before jumping into the derivation part.

```scala
import deriving.Mirror

case class Book(name: String, pages: Int, fiction: Boolean)

enum Bool:
  case True
  case False
```

Here, we imported `Mirror` and defined a product type case class called `Book` and a sum type enum called `Bool`. This time we'll be working and playing around with the `Mirror` typeclass to get a feel for what it can do.

```scala
import deriving.Mirror

case class Book(name: String, pages: Int, fiction: Boolean)

enum Bool:
  case True
  case False

// This is how we use Mirror
val bookMirror = summon[Mirror.Of[Book]]
val boolMirror = summon[Mirror.Of[Bool]]
```

The types of `bookMirror` and `boolMirror` encode various typelevel information about the ADT's. Here's what there string representation looks like -

```scala
// Note that the compiler is automatically able to infer whether these are sum types or product types

// case class Book(name: String, pages: Int, fiction: Boolean)
Mirror.Product {
  type MirroredMonoType = Book;
  type MirroredType = Book;
  type MirroredLabel = "Book";
  type MirroredElemTypes = (String, Int, Boolean);
  type MirroredElemLabels = ("name", "pages", "fiction")
}

// enum Bool: case True, False
Mirror.Sum {
  type MirroredMonoType = Bool;
  type MirroredType = Bool;
  type MirroredLabel = "Bool";
  type MirroredElemTypes = (Bool.True.type, Bool.False.type);
  type MirroredElemLabels = ("True", "False")
}
```

Now, to convert these type-level entities to actual values we can use and manipulate, we need an additional import of `compiletime.*` which allows us a bunch of useful imports like `erasedValue`, `constValue`, `constValueTuple`, `summonInline` which we'll discuss later.

To achieve our goal of Json serializing, we will need to care about ***ALL*** of these.

```scala
import compiletime.{constValue, constValueTuple}
import deriving.Mirror

case class Book(name: String, pages: Int, fiction: Boolean)

enum Bool:
  case True
  case False

// This is how we use Mirror
val bookMirror = summon[Mirror.Of[Book]]
val boolMirror = summon[Mirror.Of[Bool]]

// Get the name of the labels of the class
val bookMirrorLabels: (String, String, String) = constValueTuple[bookMirror.MirroredElemLabels]
val boolMirrorLabels: (String, String) = constValueTuple[boolMirror.MirroredElemLabels]

// ("name", "pages", "fiction")
println(bookMirrorLabels)

// ("True", "False")
println(boorMirrorLabels)

// To get a single value label instead of a Tuple, we can use

//"Book"
val bookMirrorLabel: String = constValue[bookMirror.MirroredLabel]

//"Bool"
val boolMirrorLabel: String = constValue[boolMirror.MirroredLabel]
```

Now that we have concrete values of our ADT's, this is a good moment to discuss how typeclass derivation actually works.

Typeclass derivation works by destructuring complex types which do not yet have `given` instances provided for them, to simpler types which do have `given` instances provided for them. As an example consider how the `Book` type
consists of (String, Int, Boolean).

To derive a given instance for the said `Book` type, the compiler recursively decomposes it into its constituent types, String, Int, and Boolean, and verifies that given instances exist for each component, or that the types are themselves derivable. This recursive decomposition allows the derivation mechanism to systematically reduce complex types to simpler types with the available given instances.

Now, let us recall our `JsonEncoder` typeclass we wrote in Part-2 -

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression

// case class Person(name: String, age: Int)
given personEncoder: JsonEncoder[Person] =
  (value: Person) => JsonObject(
    Map(
      "name" -> JsonString(value.name),
      "age" -> JsonInt(value.age)
    )
  )

// case class JobRole(id: Int, applicants: List[Person])
given JsonEncoder[JobRole] =
  (value: JobRole): JsonExpression =>
    JsonObject(
      Map(
        "id" -> JsonInt(value.id),
        "applicants" -> JsonArray(
          value.applicants.map(personEncoder.encode)
        )
      )
    )
```

In here, we do not need the `given` instances for `JobRole` or for `Person` case classes, we would like to derive them ourselves, so instead of that, let us provide some *primitive* `given` instances, which can be used by the compiler to *construct* the `given` instances for complex types sunce as `JobRole` or `Person`. 

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression

given JsonEncoder[Int] = (value: Int) => JsonInt(value)

given JsonEncoder[String] = (value: String) => JsonString(value)

given JsonEncoder[Boolean] = (value: Boolean) => JsonBool(value)
```

Now that we have defined the `given` instances for `JsonEncoder[A]`, we'll first refactor the `given` instances in the companion object for `JsonEncoder`. The reasons for this will be explained later apart from being idiomatic in general.

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression

object JsonEncoder:
  given JsonEncoder[Int] = (value: Int) => JsonInt(value)
  given JsonEncoder[String] = (value: String) => JsonString(value)
  given JsonEncoder[Boolean] = (value: Boolean) => JsonBool(value)
```

Additionally we'll define an `inline` method in the companion object called `derived` with the following signature. The naming is important and will also be explained later -

```scala
inline def derived[A <: Product](using mirror: Mirror.ProductOf[A]): JsonEncoder[A]
```

This looks a bit cryptic, so let's go over it,

- The reason for the constraint `A <: Product` is that every case class in a `sealed` trait heirarchy, as well as tuples automatically extend the `Product` trait. More information can be found [here](https://www.scala-lang.org/api/current/scala/Product.html) 
- The reason for using `mirror: Mirror.ProductOf[A]`, is that so we can extract the labels, types and eventually process them into a JsonEncoder[A].

We begin the implementation by converting our case class into a tuple. This is done using `Tuple.fromProductTyped`, which takes a case class instance and produces a corresponding tuple.

The advantage of using a tuple is that we can easily destructure it into head and tail elements, similar to how we handle lists, using the `*:` operator. This enables us to pattern match on it recursively.

```scala
inline def derived[A <: Product](using mirror: Mirror.ProductOf[A]): JsonEncoder[A] =
  (value: A) =>
    val valueTuple = Tuple.fromProductTyped(value)
    val mapRepresentation = ???
```
Now that we have some abstract implementation for the `derived` method, the next step is to extract the field labels of the tuple and determine their types. These types can be directly defined, or be
eventually deconstructed to.

For example, consider the `case class Person(name: String, age: Int)` and the instance `Person("Bob", 42)`.

We start by converting the `Person` instance into a tuple of values: `("Bob", 42)`. Using the `Mirror.ProductOf[A]` typeclass, we already have access to the field labels and types via `mirror.MirroredElemLabels` and `mirror.MirroredElemTypes`. With this, we have three vital pieces of information:

1. Labels: `("name", "age")`
2. Types: `(String, Int)`
3. Values: `("Bob", 42)`

We can now recurse over these in parallel to construct a `Map[String, JsonExpression]`. Here, the `String` are the field labels, and the `JsonExpression` are the values which are obtained by dispatching to the primitive `given` instances we defined earlier, such as `JsonEncoder[Int]`, `JsonEncoder[String]`, and `JsonEncoder[Boolean]`.

Hope all this made sense.

At this stage, we can implement the signature of a helper inline method, which will be delegated to for handling the processing

```scala
inline def processTupless[E <: Tuple, L <: Tuple](elements: E): Map[String, JsonExpression] = ???

inline def derived[A <: Product](using mirror: Mirror.ProductOf[A]): JsonEncoder[A] =
  (value: A) =>
    val valueTuple = Tuple.fromProductTyped(value)
    val mapRepresentation = 
      processTuples[mirror.MirroredElemTypes, mirror.MirroredElemLabels](
        valueTuple
      )
    JsonObject(mapRepresentation)
```

Let's go over `processTuples`'s type signature -

1. The `<: Tuple` bound ensures that `E` and `L` are a tuple type, so we can safely destructure them recursively using `*:` in pattern matching:

2. `E <: Tuple` represents the type of the tuple of ***values*** from the case class instance which we supply manually in `derived` via `mirror.MirroredElemTypes`. 

3. `L <: Tuple` represents the tuple of field labels from the `Mirror.ProductOf[A]` which we supply manually in `derived` via `mirror.MirroredElemLabels`. 

4. `elements: E` represent the the runtime tuple of values, e.g. `("Bob", 42)`


Now that this is hopefully clear, we can partially complete it's implementation - 

```scala
inline def processTuples[E <: Tuple, L <: Tuple](elements: E): Map[String, JsonExpression] =
  inline (elements, erasedValue[L]) match
    case (EmptyTuple, EmptyTuple) => Map.empty
    case ...

inline def derived[A <: Product](using mirror: Mirror.ProductOf[A]): JsonEncoder[A] =
  (value: A) =>
    val valueTuple = Tuple.fromProductTyped(value)
    val mapRepresentation = 
      processTuples[mirror.MirroredElemTypes, mirror.MirroredElemLabels](
        valueTuple
      )
    JsonObject(mapRepresentation)
```

We introduced a new keyword `erasedValue`. 

- [erasedValue](https://docs.scala-lang.org/scala3/reference/metaprogramming/compiletime-ops.html#erasedvalue) allows types to be treated as values at compile time. Normally, types exist only in the type system and cannot be pattern matched on directly.
`erasedValue[T]` provides a placeholder value for a type T that can be used in an inline match.

Note that you can only use `erasedValue` at compile time and with an `inline` match.

Now we can complete the implementation as follows - 

```scala
inline def processTuples[E <: Tuple, L <: Tuple](
      elements: E
  ): Map[String, JsonExpression] =
    inline (elements, erasedValue[L]) match
      case (EmptyTuple, EmptyTuple) => Map.empty
      case (ele: (eleHead *: eleTail), lab: (labelHead *: labelTail)) =>
        val (head *: tail) = ele
        val label = constValue[labelHead]
        val value = summonInline[JsonEncoder[eleHead]].encode(head)
        processTuples[eleTail, labelTail](tail) + ("" + label -> value)

inline def derived[A <: Product](using mirror: Mirror.ProductOf[A]): JsonEncoder[A] =
  (value: A) =>
    val valueTuple = Tuple.fromProductTyped(value)
    val mapRepresentation = 
      processTuples[mirror.MirroredElemTypes, mirror.MirroredElemLabels](
        valueTuple
      )
    JsonObject(mapRepresentation)
```

This is a bit terse, but soon it'll become intuitive as it's not too conceptually different from when we process regular Lists instead of Tuples.

- [summonInline](https://docs.scala-lang.org/scala3/reference/metaprogramming/compiletime-ops.html#summoninline) is basically the same as regular `summon` except that it's execution is delayed until after the method get inlined
at the callsite thereby changing it's scope.

What we're doing here, is processing the two tuples in parallel and recursively putting them into a `Map[String, JsonExpression]`, where each `head` of a tuple contains exactly one element which we then `summonInline` on, and we keep recursively dispatching the heads to their predefined `given` instances as seen in `summonInline[JsonEncoder[eleHead]].encode(head)`.
After we're finished processing them we finally get to the base case and then return our newly constructed map.

Hope this was well explained.

Now, let's look our implemented `JsonEncoder` typeclass -

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression

object JsonEncoder:
  given JsonEncoder[Int] = (value: Int) => JsonInt(value)
  given JsonEncoder[String] = (value: String) => JsonString(value)
  given JsonEncoder[Boolean] = (value: Boolean) => JsonBool(value)

  inline def processTuples[E <: Tuple, L <: Tuple](
      elements: E
  ): Map[String, JsonExpression] =
    inline (elements, erasedValue[L]) match
      case (EmptyTuple, EmptyTuple) => Map.empty
      case (ele: (eleHead *: eleTail), lab: (labelHead *: labelTail)) =>
        val (head *: tail) = ele
        val label = constValue[labelHead]
        val value = summonInline[JsonEncoder[eleHead]].encode(head)
        processTuples[eleTail, labelTail](tail) + ("" + label -> value)

  inline def derived[A <: Product](using
      mirror: Mirror.ProductOf[A]
  ): JsonEncoder[A] =
    (value: A) =>
      val valueTuple = Tuple.fromProductTyped(value)
      val mapRepresentation =
        processTuples[mirror.MirroredElemTypes, mirror.MirroredElemLabels](
          valueTuple
        )
      JsonObject(mapRepresentation)
```

We can now use it as follows  -

```scala
case class Book(name: String, pages: Int, read: Boolean)

val book = Book("Harry Potter", 300, false)

// JsonObject(Map(read -> JsonBool(true), age -> JsonInt(42), name -> JsonString(JOB)))
val jsonStructure: JsonExpression = JsonEncoder.derived[Book].encode(book)

// {"read":true,"age":42,"name":"JOB"}
println(jsonStructure.stringify)
```

As we can see, we have automatically derived the `given` instance at compile time, and this would work for ***ANY*** case class that can eventually decompose into some combination of either `String`, `Boolean`, `Int` which we provided before.

We can also extend it to include `List[A]` but let's first talk about a convenient feature that Scala3 offers us, i.e the `derives` clause.

Recall how we moved the `given` instances and the `inline` methods under the companion object of the `JsonEncoder` trait. This was done for reasons I will explain now -

If within a companion object of a typeclass contains a `derived` method, it has a special meaning not unlike `toString`, `apply`, `unapply` etc. It allows us to annotate our case classes with derives as such.

```scala
case class Book(name: String, pages: Int, read: Boolean) derives JsonEncoder
```

This allows us to directly summon the `given` instances as if it was already defined as such -

```scala
case class Book(name: String, pages: Int, read: Boolean) derives JsonEncoder

val book = Book("Harry Potter", 300, false)

// JsonObject(Map(read -> JsonBool(true), age -> JsonInt(42), name -> JsonString(JOB)))
val jsonStructure: JsonExpression = summon[JsonEncoder[Book]].encode(book)

// {"read":true,"age":42,"name":"JOB"}
println(jsonStructure.stringify)
```

Let's bring the extension method we wrote in Part - 2  to take care of the boilerplate of summoning instances, as well as provide the `JsonEncoder[List[A]]`

```scala
trait JsonEncoder[A]:
  def encode(value: A): JsonExpression

object JsonEncoder:
  extension [A](value: A)(using jsonEncoder: JsonEncoder[A])
    def toJson: JsonExpression =
      jsonEncoder.encode(value)
    def stringify: String = toJson.stringify

  given JsonEncoder[Int] = (value: Int) => JsonInt(value)
  given JsonEncoder[String] = (value: String) => JsonString(value)
  given JsonEncoder[Boolean] = (value: Boolean) => JsonBool(value)

  given [A](using encoder: JsonEncoder[A]): JsonEncoder[List[A]] =
    (value: List[A]) =>
      JsonArray(
        value.map(encoder.encode)
      )

  inline def jsonTuple[E <: Tuple, L <: Tuple](
      elements: E
  ): Map[String, JsonExpression] =
    inline (elements, erasedValue[L]) match
      case (EmptyTuple, EmptyTuple) => Map.empty
      case (ele: (eleHead *: eleTail), lab: (labelHead *: labelTail)) =>
        val (head *: tail) = ele
        val label = constValue[labelHead]
        val value = summonInline[JsonEncoder[eleHead]].encode(head)
        jsonTuple[eleTail, labelTail](tail) + ("" + label -> value)

  inline def derived[A <: Product](using
      mirror: Mirror.ProductOf[A]
  ): JsonEncoder[A] =
    (value: A) =>
      val valueTuple = Tuple.fromProductTyped(value)
      val mapRepresentation =
        jsonTuple[mirror.MirroredElemTypes, mirror.MirroredElemLabels](
          valueTuple
        )
      JsonObject(mapRepresentation)
```

And.... there you go! We have successfully created our own very ergonomic, typesafe, json serializing library :P.

We can take a look for ourselves how easy and intuitive it is -

```scala
case class Book(name: String, pages: Int, read: Boolean) derives JsonEncoder

case class Library(name: String, zipcode: Int, books: List[Book]) derives JsonEncoder

val library = Library(
  name = "Central Library",
  zipcode = 12345,
  books = List(
    Book("1984", 328, read = true),
    Book("Brave New World", 268, read = false),
    Book("Fahrenheit 451", 194, read = true)
  )
)
// {"books":[{"read":true,"pages":328,"name":"1984"},{"read":false,"pages":268,"name":"Brave New World"}, {"read":true,"pages":194,"name":"Fahrenheit 451"}],"zipcode":12345,"name":"Central Library"}
println(library.stringify)

// JsonObject(Map(books -> JsonArray(List(JsonObject(Map(read -> JsonBool(true), pages -> JsonInt(328), name -> JsonString(1984))), JsonObject(Map(read -> JsonBool(false), pages -> JsonInt(268), name -> JsonString(Brave New World))), JsonObject(Map(read -> JsonBool(true), pages -> JsonInt(194), name -> JsonString(Fahrenheit 451))))), zipcode -> JsonInt(12345), name -> JsonString(Central Library)))
println(library.toJson)

// {"books":[{"read":true,"pages":328,"name":"1984"},{"read":false,"pages":268,"name":"Brave New World"}, {"read":true,"pages":194,"name":"Fahrenheit 451"}],"zipcode":12345,"name":"Central Library"}
println(library.toJson.toString)
```

## Conclusion

Congratulations on making it to the end! This has been a lot to digest, and I'm not sure if it's advisable to finish in a single read. Let’s take a breather and reflect on what we’ve accomplished.

We explored several approaches to building a JSON library and discussed the benefits and drawbacks of each. Ultimately, we implemented a JSON library in Scala using typeclass derivation without relying on any subtyping hierarchy or runtime reflections, the latter which are very common in Java JSON libraries.

Our solution provides compile-time type safety and an excellent developer experience. For example, users can simply call `obj.toJson` or `obj.stringify` on any case class that uses the derives clause, and everything just works.

What remains are additional given instances, such as for `Map[K, V]` or even higher-kinded types `F[_]`. We won’t be going into that, as our main point has been demonstrated, but exploring them could make a great exercise for the reader.

The full final source code is available [here](https://github.com/ajafri2001/typeclass-tutorial) for you guys to try out. Feel free to experiment in the `Main.scala` file and see what works and what doesn't.

Thanks for reading my very first technical blog post! I hope you found this helpful!
