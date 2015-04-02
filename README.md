# Cauterize's Crucible

A tool to generate test data and automate the validation of Cauterize code
generators.

## Modes

Crucible operates under several modes.

### Generate

The `generate` mode produces a schema conforming to the parameters specified by the user.

### Example

The `example` command accepts a specification as an argument and produces
example binary strings that conform to the specification.

### Tester

The `tester` command accepts hooks into a code generator as arguments and will
test that the code generator is able to handle various schemas and data
instances in those schemas correctly.
