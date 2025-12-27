# ReNGBis

this is a "technology preview" (meaning that it is still just a rough experiment) of a possible schema language for validating payloads.

It is a **content** schema definition, that allows to specify the expected content of a payload.
It's not focused on specific formats, and thus it is not able to validate the use of any specific serialization option (eg. using attributes instead of elements in an `xml` document).

The aim of this project is mostly to collect feedback, in order to decide how to push it forward (or just drop it altogether if a better alternative is found).

Thanks to [Tim Bray](https://www.tbray.org/ongoing/misc/Tim) for [suggesting](https://cosocial.ca/@timbray/114574799312311970) the name for the project!

## Why creating another data validator
Most schema definition languages are pretty tedious to use; the only "enjoyable" schema language I have met is the compact syntax of [RELAX NG](https://en.wikipedia.org/wiki/RELAX_NG); its project and tools are pretty stale (they were last updated in the early 2000), but they still work wonders for processing XML files.
RELAX NG affordance in defining a schema beates XSD hands down.

This project is an experiment to try to replicate the convenience and affordance of RELAX NG schema definition getting rid of the tight binding with XML, making it suitable to validate payloads of multiple serialization protocols.

## How does it compare to alternative options
The closest alternative I have spotted is [CUE](https://cuelang.org/), even though I discoverd it only after having already worked on this project. I will have to investigate the CUE project further to understand if this is just an half baked duplicated effort, or there are some differences that may justify the effort to push this project forward.

I personally don't like working neither with XSD nor JSON-Schema; their choice of defining a schema using the same structure and syntax used for serializing the same data that needs to be specified does not seem like an effective option to me as you end up being constrained by choices suitable for a problem (effectively marshalling values) while solving a completely different problem (define valid values).

# Examples
Here are some examples of a `rengbis` definitions taken from the [tests](./src/test/scala/rengbis/ValidatorSpec.scala).

## Simple structure
#### Rengbis schema
```rengbis
= {
    name: text
    age: number
    hobbies: text*
}
```

The above schema would validate the following documents:
#### JSON
```json
{
    "name": "John",
    "age":  30,
    "hobbies": ["reading", "hiking"]
}
```

#### Yaml
```yaml
name: John
age:  30
hobbies:
  - reading
  - hiking
```


#### XML
```xml
<root>
    <name>John</name>
    <age>30</age>
    <hobbies>reading</hobbies>
    <hobbies>hiking</hobbies>
</root>
```

# Implementation
The current implementation is written in `scala 3`, using [`ZIO`](https://zio.dev) and its [`parser` library](https://zio.dev/zio-parser/)

## Supported formats
The current implementation provides helpers to validate `xml`, `json`, and `yaml` documents. Adding support for more formats should be pretty easy, especially if other libraries are already available to desirialize the payload.

The current implementation encodes all the values using the `rengbis.Value` class before validating their content.

# Features
Here is a brief run down of the currently supported features

## Basic value types and Alternative options
```rengbis
= text | number | boolean
```
This schema defines a value that may be a `text`, a `number`, or a `boolean` value.
Besides the basic value types, it also shows the way to define alternative options (`|`) for a given value.

## Basic structure (aka "dictionary")
```rengbis
= {
    name: text
    age?: number
    hobbies: text*
}
```
This schema defines a dictionaly value that **should** contain a `name` (of type `text`), **may** (`?`) contain an `age` (of type `number`), and **should** contain some `hobbies` (list, possibly empty – `*`, of `text`).
The comma to separate the different keys is required only when listing multiple values on the same line; when separating values in new lines, separatinig commas are optional.

## Named structures
```rengbis
foo = {
    foo_1: number,
    foo_2: text
}

bar = {
    bar_A: text*,
    bar_b: number*
}

= {
    key_1: foo,
    key_2: bar
}
```
This schema defines two **named structures** (`foo` and `bar`) that are used to define other structures.

## Constant values
```rengbis
= "yes" | "no"
```
This schema defines a value that could only have either the text `yes` or the text `no` as values.

## Tuple values
```rengbis
= (text, text, number)
```
This schema defines a value that should contain three values, the first two of type `text`, and the last of type `number`.
Tuple values are not useful for *regular* documents (`yaml`, `json`, `xml`, …), but may be quite handy in describing tabular data (eg `csv` files) when columns do not have an explicit name.


## Text constraints

### Size constraints
It is possible to constraint the size of the text.
```rengbis
exactSize = text { length == 10 }
short = text { length <= 4 }
long  = text { length > 20 }
inBetween  = text { 8 < length <= 12 }
exactSize = text { length == 10 }
= exactSize | short | long | inBetween
```


### Regular expressions
```rengbis
= text { regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})", length == 10 }
```
Text values may be constraint with either a `regex` or a `length`.
This would match an ISO8060 date value, like `2025-12-27`.

### Patterns
It's also possible to use COBOL like 'picture clause' in order to express easier to read formats.

```rengbis
= text { pattern = "(###) ###-####" }
```
This would match an US formatted phone number, like `(123) 456-7890`.

The current implementation handles these format specifiers:
- `#`: any digit
- `X`: any letter
- `@`: any alphanumeric character
- `*`: any character
- any other character is parsed unchanged

## List constraints
```rengbis
= {
  possibly_empty_list_of_text: text*
  list_with_at_least_one_element: text+
  list_with_exactly_three_element: text{3}
  list_with_a_number_of_elements_between_two_and_ten: text{2,10}
}
```
List values may be constrainted in the number of items they contain.

## Number costraints - MISSING
The options for numeric constraints are way more articulated than what has already been implemented for text and list values, and they will require some more thoughts on how to define them.
Nothing is conceptually difficult, but will probably require a lot of tries before finding a satisfying arrangement.
Values would have to be constraint at least on **range of values**, number of decimal digits, precision, scale; more option may emerge while working on the definition.

## Comments - MISSING
Comments are currently not supported; I briefely tried to add their definition to the syntax, but failed miserably, so I have postpone the effort.


# Status of the project
This is a very early prototype, shared only to get some feedback; besides the few test included in the code base, it has not be used anywhere else.

## Future enhancements
Before venturing in this experiment, I did many tries with [`zio-schema`](https://github.com/zio/zio-schema); unfortunately I was not able to find a way to use that library to achieve my goals.
[Lately](https://x.com/jdegoes/status/1919380595597090856) a [new version](https://github.com/zio/zio-blocks) of `zio-schema` has been announced; I haven't had the time to play with it yet, but the idea of using `rengbis` as a language to define `zio-schema` values that could be later leveraged to validate/parse actual payloads, getting rid of the custom  machinary I had to build myself seems an interesting option to validate.
