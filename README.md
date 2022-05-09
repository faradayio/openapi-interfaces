# `openapi-interfaces`: Automatically generate `GET`, `POST`, `PUT` and JSON Merge Patch schemas for OpenAPI

**EXPERIMENTAL.** Details subject to change.

This tool extends [OpenAPI 3.1.0](https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md) with the ability to generate related schemas automatically. Specifically, you can define a single interface type `Widget`, and automatically generate:

- `Widget` (for `GET` requests)
- `WidgetPost`
- `WidgetPut`
- `WidgetMergePatch` (for `PATCH` requests using [JSON Merge Patch](https://datatracker.ietf.org/doc/html/rfc7396) format, which is basically a nicely formalized "partial `PUT`")

## Installation

To install the latest version, first make sure you have a Rust toolchain. You
can [install one using these instructions](https://rustup.rs/). Then run:

```sh
cargo install -f openapi-interfaces
```

We will provide binaries at some point.

## Usage

```sh
openapi-interfaces --help
openapi-interfaces api_with_interfaces.yml -o api.yml
```

## OpenAPI extensions

This tool defines a new `components.interfaces` section, which allows specifying "interface" types. For example:

```yaml
components:
  # You can declare schemas normally if you wish.
  schemas: {}

  # But we also support interface definitions.
  interfaces:
    Resource:
      emit: false # Do not include this in generated output.
      members:
        id:
          required: true
          schema:
            # Normal OpenAPI / JSON Schema definitions.
            type: string
            format: uuid
    Widget:
      # Include all fields from `Resource`.
      $includes: "Resource"
      members:
        # We can override properties from `Resource` using JSON
        # Merge Patch syntax.
        id:
          schema:
            example: e35a3c8d-5486-49ec-9b23-6747afc19570
        name:
          required: true
          mutable: true
          schema:
            type: string
        comment:
          mutable: true
          schema:
            type: string
        readonly:
          required: true
          # This can't be updated once the object is created.
          mutable: false
          # But we do allow this to be set at creation time.
          # If omitted, `initializable` defaults to the value
          # of the `mutable` option.
          initializable: true
          schema:
            type: string
```

This will automatically generate four `Widget` types:

```yaml
components:
  schemas:
    Widget: ...
    WidgetPost: ...
    WidgetPut: ...
    WidgetMergePatch: ...
```

For the complete definitions, see [`example_output.yml`](./examples/example_output.yml).

### Referring to interfaces

We can then refer to interfaces using the new `$interface` key, with an appropriate variant:

```yaml
paths:
  /widgets:
    post:
      requestBody:
        required: true
        content:
          application/json:
              schema:
                # This becomes `$ref: "#/components/schemas/WidgetPost`.
                $interface: "Widget#Post"
      responses:
        201:
          content:
            application/json:
              schema:
                # This becomes `$ref: "#/components/schemas/Widget`.
                $interface: "Widget"
```

Possible options are:

- `Widget`: The type returned by `GET` and other methods that provide a complete resource from the server.
- `Widget#Post`: The type passed to a `POST` request.
- `Widget#Put`: The type passed to a `PUT` request. May also be used as the base type for applying `#MergePatch` values.
- `Widget#MergePatch`: A [JSON Merge Patch](https://datatracker.ietf.org/doc/html/rfc7396) schema that can be passed to `PATCH`.
- `Widget#SameAsInterface`: **May only be used inside `components.interfaces`.** This is a shortcut value that says "When generating a `#Get` interface, include `$ref: "components.schemas.Widget`. When generating a `#Post` interface, include `$ref: "components.schemas.WidgetPost`." In other words, use the same variant selector as the containing interface. Useful for when compound types are sent over the wire.
