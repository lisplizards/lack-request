# lack-request

This library defines an alternative constructor function for `LACK/REQUEST:REQUEST` structs and extracts parsing functionality to standard functions instead of parsing cookies, query parameters, and body parameters and modifying the application environment directly from the constructor.

An additional package related to content-negotiation is also provided, `FOO.LISP.LACK/REQUEST/CONTENT-NEGOTIATION`.

## Usage

Example:

```lisp
(let ((request (foo.lisp.lack/request:make-request env)))
  (let ((cookies (foo.lisp.lack/request:request-cookies request))
        (body-parameters (foo.lisp.lack/request:request-body-parameters request))
        (query-parameters (foo.lisp.lack/request:request-query-parameters request)))
      ;; ...
     ))
```

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-request)
```

## Dependencies

* [lack-request](https://github.com/fukamachi/lack/blob/master/lack-request.asd)

### Tests

* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [rove](https://github.com/fukamachi/rove)

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

* John Newton
* Eitaro Fukamachi (author of Lack)

The functions in src/main.lisp have been adapted by John Newton from [lack/request](https://github.com/fukamachi/lack/blob/8243010b48a10edd527da4e94686b803c70731ef/src/request.lisp) (original author: Eitaro Fukamachi).

Additions by John Newton:
* Add content negotiation package (`FOO.LISP.LACK/REQUEST/CONTENT-NEGOTIATION`, file: src/content-negotiation.lisp)

## License

MIT
