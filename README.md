# ReNGBis

This is a "technology preview" (meaning that it is still just a rough experiment) of a possible schema language for defining and validating payloads.

It is a **content** schema definition, that allows to specify the expected content of a payload.
It's not focused on specific formats, and thus it is not able to validate the use of any specific serialization option (eg. using attributes instead of elements in an `XML` document).

The aim of this project is mostly to collect feedback, in order to decide how to push it forward (or just drop it altogether if a better alternative is found).

Thanks to [Tim Bray](https://www.tbray.org/ongoing/misc/Tim) for [suggesting](https://cosocial.ca/@timbray/114574799312311970) the name!
If it wasn't for Tim, this project may have been called **YADD** (Yet Another Data Definition), as a tribute to [YACC](https://en.wikipedia.org/wiki/Yacc); or the mostly *boring* (and definitely not distinguishable enough) **Relaxed Schema**.

## Why (yet) another data definition language
Most schema definition languages available today are pretty tedious to use; the only "enjoyable" option we are aware of is [RELAX NG](https://en.wikipedia.org/wiki/RELAX_NG), when using its [compact syntax](https://books.xmlschemata.org/relaxng/relax-CHP-4-SECT-2.html); this project and its tools are pretty stale (they were last updated in the early 2000), but they still work wonders for processing XML files, and their affordance in defining an XML schema beats XSD hands down.

ReNGBis is an experiment to try to replicate the convenience and affordance of RELAX NG, while making it suitable to validate payloads of multiple serialization formats, not just XML.

## How does it compare to alternative options
The closest alternative we are aware of is [CUE](https://cuelang.org/), even though we discovered it only after having already started working on this project. The aims of both projects are similar, but with very different approaches; we like ReNGBis affordance better, though.

ReNGBis does not even try to leverage any of the data serialization format it handles to define its schemas; ReNGBis syntax has been defined from scratch just to be as effective as possible in the definitions of the structures of the data it has to validate.

# Examples
Here are some examples of ReNGBis definitions; more examples are available in the test [resources](./modules/core/src/test/resources/schemas/).

## Simple structure
Here's a simple schema that should be pretty straightforward to understand; it's a structure with three fields: `name`, `age`, and `hobbies`, respectively of type `text`, `number`, and *list* of `text` (`text*`)

#### Rengbis schema
```rengbis
= {
    name: text
    age: number
    hobbies: text*
}
```

The above schema can validate the following payloads
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
The current implementation provides helpers to validate `xml`, `json`, `yaml`, and `csv` documents. Adding support for more formats should be pretty easy, especially if other libraries are already available to deserialize the payload.

The current validation procedure is implemented on a generic `rengbis.Value` data; the different formats are first read and encoded in values of this class, before actually being validated.

# Schema features
Here is a brief run down of the currently supported features:
- [Types](#types)
- [Constraints](#constraints)
- [Comments](#comments)
- [Import](#import)

## Types

### Basic type values
ReNGBis supports four basic types, `text`, `number`, `boolean`, and `any`.
The code below defines a type that may be either a `text`, a `number`, a `boolean`, or just `any` value.
This is just a dull example to list all the basic types, as it wouldn't make much sense to define a type like this, as it would match anything anyway.
Besides the basic value types, it also shows the way to define alternative options (`|`) for a given value.

```rengbis
= text | number | boolean | any
```

Basic values on their own would be pretty boring; but there are enough options to combine them together and keep it interesting (hopefully!).

### Structured objects
Structured objects (also known as *dictionary* or *maps* allow to combine multiple values, giving a unique name each.
In the example below, it's defined a value that contains a `title` (of type `text`), **may** (`?`) contains the number of  `pages` (of type `number`, obviously), and also the `available` information (encoded as a `boolean` value).
Commas are used to separate the different options, but are only required when listing multiple items on the same line; when separating definitions in new lines, the comma is optional.
```rengbis
= {
    title: text
    pages?: number
    available: boolean
}
```

#### … also with unconstrained keys
It's also possible to define structures with *free* keys; in order to express this constraint, just use the `…` (or `...`) value for the key:
```rengbis
service = { name: text, port?: number }
= {
    name: text
    services: { …: service }
}
```

### Constant values
It is also possible to define values that can only assume specific constant values. This schema defines a value that could only have either the text `"Of course"` or the text `"No way"` as values.
```rengbis
= "Of course" | "No way"
```

## Tuple values
```rengbis
= (text, text, number)
```
This schema defines a value that should contain three values, the first two of type `text`, and the last of type `number`.
Tuple values are not useful for *regular* documents (`yaml`, `json`, `xml`, …), but may be quite handy in describing tabular data (eg `csv` files) when columns do not have an explicit name.

## Any values
As much a we like to make everything well defined, from time to time you need to let the schema a little loose; in these cases you can use the `any` type.
For example, if we want to specify that a structure is just a map where values may be anything, just use the following definition
```rengbis
= { …: any }
```

### Named structures
It's possible to give **names** to any value; this allows to reference it else where to both avoid repetitions and making relationships more clear.
```rengbis
foo = { foo_1: number, foo_2: text }
bar = { bar_A: text*, bar_B: number* }

= { key_1: foo, key_2: bar }
```

### Recursive definitions
It's also possible to use recursive definitions:
```rengbis
Expression = Value | Operation
Value = number
Operation = {
    operator: "+" | "-" | "*" | "/"
    firstOperand: Value
    secondOperand: Expression
}

= Expression
```

## Constraints

### Text constraints

#### Size
It is possible to constraint the size of the text.
```rengbis
exactSize = text [ length == 10 ]
short = text [ length <= 4 ]
long  = text [ length > 20 ]
inBetween  = text [ 8 < length <= 12 ]
exactSize = text [ length == 10 ]
= exactSize | short | long | inBetween
```

#### Regular expressions
```rengbis
= text [ regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})", length == 10 ]
```
Text values may be constraint with either a `regex` or a `length`.
This would match an ISO8060 date value, like `2025-12-27`.

#### Pattern
It's also possible to use COBOL like 'picture clause' in order to express easier to read formats.

```rengbis
= text [ pattern = "(###) ###-####" ]
```
This would match an US formatted phone number, like `(123) 456-7890`.

The current implementation handles these format specifiers:
- `#`: any digit
- `X`: any letter
- `@`: any alphanumeric character
- `*`: any character
- any other character is parsed unchanged

### List constraints

#### Number of items

```rengbis
= {
  possibly_empty_list_of_text: text*
  list_with_at_least_one_element: text+
  list_with_exactly_three_element: text* [ size == 3 ]
  list_with_a_number_of_elements_between_two_and_ten: text* [ 2 <= size <= 10 ]
}
```
List values may be constrainted in the number of items they contain.

#### Uniqueness
It is also possible to define some uniqueness criteria for values in a given list.

```rengbis
structure = {
    name: text
    description: text
}

= {
    list_of_unique_strings?: text* [ unique ]
    list_of_unique_numbers?: number* [ unique ]
    list_of_unique_integers?: number [ integer ]* [ unique ]

    unique_names?: structure* [ unique = name ]
    unique_name_plus_description?: structure* [ unique = (name, description) ]
    unique_names_and_descriptions?: structure* [ unique = name, unique = description ]

    list_of_exactly_three_integer?: number[ integer ]* [ size == 3 ]
    list_of_at_least_two_integer_greater_that_10?: number[ integer, value >= 10 ]* [ size >= 2 ]
}
```

At the moment there are a few limits on how this constraints may be defined:
- uniqueness can only be defined on basic values (text, number, boolean) or combination of such basic values
- only direct elements of the object where uniqueness constraints are defined may be referenced

### Number constraints
Number definitions support two kind of constraints: *type* and *range*.
For the *type*, there is currently just one option that can be specified, and that is 'integer'.
```rengbis
= number [ integer ]
```

This defines that the value must be integer, with no decimals digits.

To specify the *range* of legit values, the syntax mimics the one used to specify the `length` of `text` values:
```rengbis
positiveValue = number [ value > 0 ]
positiveInteger = number [ integer, value > 0 ]

reservedPortNumber = number [ integer, 0 <= value < 1024 ]
registeredPortNumber = number [ integer, 1024 <= value < 49152 ]
ephemeralPortNumber = number [ integer, 49152 <= value <= 65535 ]
```

## Comments
Anything after a `#` character, up to the end of the line, is ignored by the parser and thus treated as a comment.

## Imports
It is possible to reference external files (currently only with a relative path) in order to *include* external definitions in the current scope.

The syntax is pretty simple:
```rengbis
types => import ./types.rengbis
```

This defines a new *scope* `types` whose content is loaded from the `./types.rengbis` file.
After the import, newly imported definitions may be used adding the `types.` prefix:
```rengbis
types => import ./types.rengbis

= {
    username: types.username
}
```

The `root` element of the imported schema definition may also be used, just referring to the *scope* name.

# Status of the project
This is a very early prototype, shared only to get some feedback; besides the few tests included in the code base, it has not been used anywhere else.

## Future enhancements
Before venturing into this experiment, we tried working with [`zio-schema`](https://github.com/zio/zio-schema); unfortunately we were not able to find a way to use that library to achieve our goals.
[Lately](https://x.com/jdegoes/status/1919380595597090856) a [new version](https://github.com/zio/zio-blocks) of `zio-schema` has been announced; we haven't had the time to explore it yet, but the idea of using `rengbis` as a language to define `zio-schema` values that could be later leveraged to validate/parse actual payloads, getting rid of the custom machinery we had to build, seems an interesting option to explore.


## Project Structure

The project is organized into three distinct modules:

- **`core`** - Pure library module with schema parsing and validation logic. Can be used as a dependency in other projects.
- **`cli`** - Executable wrapper that provides a unified command-line interface for all functionality.

## Build Options

### Library JAR (for developers)
The `core` module can be built as a library JAR for use in other projects:

```bash
sbt assembly
```

This creates `modules/core/target/scala-3.7.4/rengbis.jar` - a fat JAR containing all dependencies that can be used in other projects.

### Executable JAR (for end users)
Create a self-contained executable using the [ExecJar](https://github.com/parttimenerd/execjar) project:

```bash
sbt execjar
```

This creates `modules/cli/target/execjar/rengbis-cli` - an executable that includes the JVM launcher. Requires [jbang](https://www.jbang.dev) to build:
```bash
sdk install jbang
```

The executable is currently set to require Java 17+ to run, but no actual tests have been run to verify if this constraint is relevant. At the moment it has only been tested with Java 25.0.1, so [YMMV](https://en.wiktionary.org/wiki/YMMV#English).

### Native Binary
Build a native executable using GraalVM:

```bash
sbt nativeImage
```

This creates `modules/cli/target/native-image/rengbis-cli` - a standalone native binary with no JVM required.

To build native images, install GraalVM:
```bash
sdk install java 25.0.1-graalce
sdk use java 25.0.1-graalce
```

Older GraalVM versions may require also to install the `native-image` explicitly, using GraalVM Updater (`gu`):
```bash
gu install native-image
```

## Usage
The unified `rengbis` executable provides three main commands:

### Validate Schema Files
```bash
rengbis validate-schema schema1.rengbis schema2.rengbis
```

### Validate Data Files
```bash
rengbis validate-data --format json --schema schema.rengbis data1.json data2.json
```

Supported formats: `json`, `yaml`, `xml`, `csv`
