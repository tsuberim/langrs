function sep(rule, delimiter) {
  return seq(repeat(seq(rule, delimiter)), optional(rule))
}

function sep1(rule, delimiter) {
  return seq(rule, repeat(seq(delimiter, rule)))
}

module.exports = grammar({
    name: 'fun',
  
    word: $ => $.id,

    extras: $ => [
      $._comment,
      /[\s\f\uFEFF\u2060\u200B]|\r?\n/
    ],

    rules: {
      source_file: $ => field('term',$._term),

      _comment: $=> token.immediate(/#.*/),
      
      id: $ => /[_a-z][a-zA-Z0-9_]*/,
      tag_id: $ => /[A-Z][a-zA-Z0-9_]*/,
      sym: $ => /[!@#$%^&\*\+><|]+/,
      tag: $ => prec.right(3, seq(field('name', $.tag_id), optional(field('payload', $._term)))),

      str_lit: $ => /[^`\{\}]+/,
      str: $ => seq('`', repeat(choice(field('fragment', $.str_lit), seq('{', field('term',$._term) , '}'))), '`'),
      num: $ => /[0-9]+/,

      _lit: $ => choice($.str, $.num),

      record: $ => seq('{', sep(seq(field('keys', $.id), ':', field('values', $._term)), ','), '}'),
      list: $ => seq('[', sep(field('items', $._term), ','), ']'),
      app: $ => prec.left(1,seq(field('f', $._term), '(', sep1(field('args', $._term), ','), ')')),
      infix_app: $ => prec.left(2, seq(field('lhs', $._term), field('f', $.sym)  ,seq(field('rhs', $._term)))),
      lam: $ => seq('\\', sep1(field('params', $.id), ','), '->', field('body', $._term)),
      access: $ => prec.left(2,seq(field('term', $._term), '.', field('property', $.id))),
      curied_access: $ => seq('.', field('property', $.id)),
      match: $ => prec.right(
        seq(
          'when', 
          field('term', $._term), 
          'is', 
          sep1(seq(field('case_tag', $.tag_id), field('case_id', $.id), '->', field('case_result', $._term)), ';'), 
          optional(seq('else', field('else', $._term)))
        )
      ),

      def: $ => seq(field('lhs', choice($.id, $.sym)), '=', field('rhs',$._term)),
      bind: $ => seq(field('lhs', choice($.id, $.sym)), '<-', field('rhs',$._term)),
      type_def: $ => seq(field('lhs', choice($.id, $.sym)), ':', field('rhs',$._type)),
      block: $ => seq('(', 
        sep(field('statement',choice($.def, $.bind, $.type_def)) ,/\n+/),
        field('term', $._term), 
      ')'),

      _term: $ => choice(
        $._lit, 
        $.id, 
        $.sym,
        $.tag, 
        $.record, 
        $.list, 
        $.match,
        $.app, 
        $.infix_app, 
        $.curied_access,
        $.access,
        $.lam, 
        $.block
      ),
    
      type_record: $ => seq('{', sep(seq(field('keys', $.id), ':', field('types', $._type)), ','), optional(seq('|', field('rest', $.id))), '}'),
      type_union: $ => seq('[', sep(seq(field('keys', $.tag_id), field('types', $._type)), ','), optional(seq('|', field('rest', $.id))), ']'),
      type_app: $ => seq(field('f',$.tag_id), optional(seq('<', sep1(field('args', $._type), ','), '>'))),
      _type: $ => choice(
        $.id,
        $.type_record,
        $.type_union,
        $.type_app
      )
    }
  });