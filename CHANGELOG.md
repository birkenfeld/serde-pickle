# Changelog

## 1.1 - Sep 2021

- Support objects reduced to integers via `__reduce__` (#17).

## 1.0 - Sep 2021

- Require Rust 1.41.
- Support deserializing Pickle protocol 5.
- Add `DeOptions` and `SerOptions` struct for selecting options when
  deserializing and serializing.  `SerOptions` replaces the `use_proto_3`
  boolean flag for serializer functions.
- Add a `DeOptions` flag for replacing unresolvable globals by `None` (#13).
- Change default enum serialization to the "standard" representation chosen by
  e.g. `serde_json` and `serde_yaml`, using dictionaries. Add a `DeOptions`
  flag for switching back to the old representation (#9).

## 0.6 - Jan 2020

- Update to Rust 2018.
- Require Rust 1.31.

## 0.5 - Mar 2019

- Support deserializing many custom classes, by replacing them with their
  attribute dictionary.

## 0.4 - May 2017

- Update to Serde 1.0.
- Support deserializing bytes.

## 0.3 - Jan 2017

- Update to Serde 0.9.

## 0.2 - Jul 2016

- Update to Serde 0.8.

## 0.1 - Apr 2016

- Initial release.
