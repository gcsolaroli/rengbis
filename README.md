# ReNGBis

this is a "technology preview" (meaning that it is still just a rough experiment) of a possible schema language for validating payloads.

It is a **content** schema definition, that allows to specify the expected content of a payload.
It's not focused on specific formats, and thus it is not able to validate the use of any specific serialization option (eg. using attributes instead of elements in an `xml` document).

The aim of this project is mostly to collect feedback, in order to decide how to push it forward (or just drop it altogether if a better alternative is found).

Thanks to [Tim Bray](https://www.tbray.org/ongoing/misc/Tim) for [suggesting](https://cosocial.ca/@timbray/114574799312311970) the name for the project!

## Why creating another data validator
Most schema definition languages are pretty tedious to use; the only "enjoyable" schema language I have met is the compact syntax of [RELAX NG](https://en.wikipedia.org/wiki/RELAX_NG); its project and tools are pretty stale (they were last updated in the early 2000), but they still work wonders for processing XML files.
RELAX NG affordance in defining a schema beates XSD hands down.

This project is an experiment to try to replicate the convenience and affordance of RELAX NG schema definition, but getting rid of the tight binding with XML and making it suitable to validate payloads of multiple serialization formats.

## How does it compare to alternative options
The closest alternative I have spotted so far is [CUE](https://cuelang.org/), even though I discoverd it only after having already started this project. I will have to investigate the CUE project further to understand if this is just an half baked duplicated effort, or there are some differences that may justify the effort to push this project forward.

I personally don't like working neither with XSD nor JSON-Schema; their choice of defining a schema using the same structure and syntax used for serializing the data itself is a self imposed constraint that provides very little value.

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

# Schema features
Here is a brief run down of the currently supported features

## Basic value types and Alternative options
```rengbis
= text | number | boolean | any
```
This schema defines a value that may be a `text`, a `number`, a `boolean`, or just `any` value.
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

### Objects with uncostraint keys
It's also possible to define structures with *free* keys; in order to express this constraint, just use the `…` (or `...`) value for the key:
```rengbis
service = { name: text, port?: number }
= {
    name: text,
    services: { …: service }
}
```

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

### Size
It is possible to constraint the size of the text.
```rengbis
exactSize = text [ length == 10 ]
short = text [ length <= 4 ]
long  = text [ length > 20 ]
inBetween  = text [ 8 < length <= 12 ]
exactSize = text [ length == 10 ]
= exactSize | short | long | inBetween
```

### Regular expressions
```rengbis
= text [ regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})", length == 10 ]
```
Text values may be constraint with either a `regex` or a `length`.
This would match an ISO8060 date value, like `2025-12-27`.

### Pattern
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

## List constraints

### Number of items

```rengbis
= {
  possibly_empty_list_of_text: text*
  list_with_at_least_one_element: text+
  list_with_exactly_three_element: text* [ size == 3 ]
  list_with_a_number_of_elements_between_two_and_ten: text* [ 2 <= size <= 10 ]
}
```
List values may be constrainted in the number of items they contain.

### Uniqueness
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

## Number costraints
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
This is a very early prototype, shared only to get some feedback; besides the few test included in the code base, it has not be used anywhere else.

## Future enhancements
Before venturing in this experiment, I did many tries with [`zio-schema`](https://github.com/zio/zio-schema); unfortunately I was not able to find a way to use that library to achieve my goals.
[Lately](https://x.com/jdegoes/status/1919380595597090856) a [new version](https://github.com/zio/zio-blocks) of `zio-schema` has been announced; I haven't had the time to play with it yet, but the idea of using `rengbis` as a language to define `zio-schema` values that could be later leveraged to validate/parse actual payloads, getting rid of the custom  machinary I had to build myself seems an interesting option to validate.


## Project Structure

The project is organized into three distinct modules:

- **`core`** - Pure library module with schema parsing and validation logic. Can be used as a dependency in other projects.
- **`lsp`** - Language Server Protocol implementation for editor integration.
- **`cli`** - Executable wrapper that provides a unified command-line interface for all functionality.

## Build Options

### Library JAR (for developers)
The `core` module can be built as a library JAR for use in other projects:

```bash
sbt assembly
```

This creates `modules/core/target/scala-3.7.4/rengbis-core.jar` - a fat JAR containing all dependencies that can be used in other projects.

### Executable JAR (for end users)
Create a self-contained executable using the [ExecJar](https://github.com/parttimenerd/execjar) project:

```bash
sbt execjar
```

This creates `modules/cli/target/rengbis` - an executable that includes the JVM launcher. Requires [jbang](https://www.jbang.dev) to build:
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

Supported formats: `json`, `yaml`, `xml`

### Start LSP Server
```bash
rengbis lsp
```

This starts the Language Server Protocol server for editor integration.

## Editor Support

ReNGBis includes a **Language Server Protocol (LSP)** implementation that provides rich IDE features in any LSP-compatible editor!

> [!WARNING]
> This is just the skeleton to start working on a proof-of-concept.
> It's not meant or expected to be working nor useful at this point in time

### Features

- **Syntax Validation**: Real-time error checking as you type
- **Code Completion**: Auto-complete for keywords, types, and schema syntax
- **Hover Documentation**: Helpful information about schema elements and keywords
- **Go to Definition**: Navigate to named value definitions and imports

### Quick Start

1. Build the executable:
```bash
sbt execjar
```

2. Configure your editor to use the LSP server:
```bash
modules/cli/target/rengbis lsp
```

For complete setup instructions and editor-specific configurations, see the [LSP documentation](modules/lsp/README.md).
