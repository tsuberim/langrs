const epsilon = '';

function sep(rule, delimiter) {
  return seq(repeat(seq(rule, delimiter)), optional(rule))
}

function sep1(rule, delimiter) {
  return seq(rule, repeat(seq(delimiter, rule)))
}

module.exports = grammar({
  name: 'fun',
  supertypes: $ => [$._expr],
  word: $ => $.id,
  rules: {

    source_file: $ => $._expr,

    number: $ => /\d+/,
    string: $ => seq('\'', /[^']*/, '\''),
    id: $ => /[a-z][a-zA-Z0-9_]*/,
    constructor_id: $ => /[A-Z][a-zA-Z0-9_]*/,
    constructor: $ => seq(field('name', $.constructor_id), field('content', $._expr)),
    _lit: $ => choice($.number, $.string),
    _expr: $ => choice($._lit, $.id, $.constructor, $.application, $.lambda, $.record, $.access, $.match, $.parens),
    parens: $ => seq('(', field('expr', $._expr), ')'),
    application: $ => prec(1, seq(field('function', $._expr), '(', field('arguments', sep1($._expr, ',')), ')')),
    lambda: $ => seq('\\', field('arguments', sep1($.id, ',')), '->', field('body', $._expr)),
    record: $ => seq('{', sep(seq(field('keys', $.id), ':', field('values', $._expr)), ','), '}'),
    access: $ => prec.left(2, seq(field('expression', $._expr), '.', field('property', $.id))),
    match: $ => prec.right(1,
      seq(
        'when',
        field('expression', $._expr),
        'is',
        sep1(seq(field('patterns', $.constructor), '->', field('consequences', $._expr)), ';'),
        optional(seq('else', field('otherwise', $._expr)))
      )
    ),
  }
});