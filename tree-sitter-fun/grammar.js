function sep(rule, delimiter) {
  return seq(repeat(seq(rule, delimiter)), optional(rule))
}

function sep1(rule, delimiter) {
  return seq(rule, repeat(seq(delimiter, rule)))
}

module.exports = grammar({
    name: 'fun',
  
    word: $ => $.id,

    rules: {
      source_file: $ => field('term',$._term),
      
      id: $ => /[a-z][a-zA-Z0-9_]*/,
      tag_id: $ => /[A-Z][a-zA-Z0-9_]*/,
      tag: $ => prec.left(1, seq(field('name', $.tag_id), optional(field('payload', $._term)))),

      str: $ => seq('`', /[^`]*/, '`'),
      num: $ => /[0-9]+/,

      _lit: $ => choice($.str, $.num),

      record: $ => seq('{', sep(seq(field('keys', $.id), ':', field('values', $._term)), ','), '}'),
      list: $ => seq('[', sep(field('items', $._term), ','), ']'),
      app: $ => prec.left(1,seq(field('f', $._term), '(', sep1(field('args', $._term), ','), ')')),
      lam: $ => seq('\\', field('params', sep1($.id, ',')), '->', field('body', $._term)),
      access: $ => prec.left(2,seq(field('term', $._term), '.', field('property', $.id))),
      match: $ => prec.right(
        seq(
          'when', 
          field('term', $._term), 
          'is', 
          sep1(seq(field('case_tag', $.tag_id), field('case_id', $.id), '->', field('case_result', $._term)), ';'), 
          optional(seq('else', field('else', $._term)))
        )
      ),

      block: $ => seq('(', sep(seq(field('def_lhs', $.id), '=', field('def_rhs',$._term)) ,'\n'), field('term', $._term), ')'),
      _term: $ => choice(
        $._lit, 
        $.id, 
        $.tag, 
        $.record, 
        $.list, 
        $.match, 
        $.app, 
        $.access, 
        $.lam, 
        $.block
      ),
    }
  });