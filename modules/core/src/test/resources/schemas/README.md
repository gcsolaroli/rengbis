# Tests automatically run on the content of this folder

Files in this folder (`src/test/resources/schemas`) are automatically processed by the `SchemaSamplesSpec` object defined in `src/test/scala/rengbis/SchemaSpec.scala`.

This is the logic applied:
- validate all `*.rengbis` files found in this directory (subfolders are not inspected)
    - if the name of the file (not considering the `.rengbis` exstension) ends with `-NOT_VALID`, the parsing of the file is expected to fail; otherwise it should succed
    - any result not matching these exceptations is reported as an error
- once a `*.rengbis` file is validated, the tests looks for a folder with the same name of the file, removing the `.rengbis` exstension
    - this folder contains a set of sample files to be validated with the selected schema
    - files are arranged in subfolders, one for each relevant type; possible options are `json`, `yaml`, `xml`
    - each file is validated; if the name of the files ends with `-NOT_VALID`, the validation is expected to fail; otherwise it should succeed
    - any result not matching these exceptations is reported as an error
