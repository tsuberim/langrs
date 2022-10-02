use std::{fmt::format, path::Path};

use convert_case::{Case, Casing};
use indexmap::IndexMap;
use proc_macro::TokenStream as RealTokenStream;
use quote::{
    __private::{Literal, TokenStream},
    format_ident, quote,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use syn::{parse, parse_macro_input, token::Token, Ident, LitStr};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Root {
    #[serde(rename = "type")]
    kind: String,
    named: bool,
    fields: Option<IndexMap<String, Child>>,
    subtypes: Option<Vec<Type>>,
    children: Option<Child>,
}

impl Root {
    fn fields(&self) -> IndexMap<String, Child> {
        if let Some(fields) = &self.fields {
            fields.clone()
        } else if let Some(child) = &self.children {
            IndexMap::from_iter([(String::from("arg"), child.clone())])
        } else {
            IndexMap::new()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Child {
    multiple: bool,
    required: bool,
    types: Vec<Type>,
}

impl Child {
    fn kind(&self) -> String {
        // FIXME: assumes SINGLE named type
        let ty = self
            .types
            .iter()
            .find(|x| x.named)
            .expect("Expected to find a signle named type");
        ty.kind.clone()
    }

    fn wrap_type(&self, t: TokenStream) -> TokenStream {
        if self.multiple {
            quote!(Vec<#t>)
        } else if !self.required {
            quote!(Option<#t>)
        } else {
            t
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Type {
    #[serde(rename = "type")]
    kind: String,
    named: bool,
}

struct FieldDesc {
    required: bool,
    multiple: bool,
    recursive: bool,
}

impl FieldDesc {
    fn wrap_type(&self, t: TokenStream) -> TokenStream {
        if self.multiple {
            quote!(Vec<#t>)
        } else if !self.required {
            quote!(Option<#t>)
        } else {
            t
        }
    }

    fn transform(&self, name: Ident, expr: TokenStream) -> TokenStream {
        if self.multiple {
            quote!(#name.into_iter().map(|#name| #expr).collect())
        } else if !self.required {
            quote!(#name.map(|#name| #expr))
        } else {
            expr
        }
    }
}

struct FnDesc {
    non_rec_type: TokenStream,
    rec_type: TokenStream,
    name: String,
    field_types: IndexMap<Ident, FieldDesc>,
}

impl FnDesc {
    fn field_list(&self) -> TokenStream {
        let field_names = self.field_types.keys();
        quote!(#(#field_names),*)
    }

    fn field_type_list(&self) -> TokenStream {
        let field_types = self.field_types.iter().map(|(name, field_desc)| {
            let t = field_desc.wrap_type(if field_desc.recursive {
                self.rec_type.clone()
            } else {
                self.non_rec_type.clone()
            });
            quote!(#name: #t)
        });
        quote!(#(#field_types),*)
    }

    fn enum_name(&self) -> Ident {
        format_ident!("{}", self.name.to_case(Case::UpperCamel))
    }

    fn enum_variant_declaration(&self) -> Ident {
        format_ident!("{}", self.name.to_case(Case::UpperCamel))
    }

    fn trait_fn_name(&self) -> Ident {
        format_ident!("visit_{}", self.name.to_case(Case::Snake))
    }

    fn trait_declaration(&self) -> TokenStream {
        let fn_name = self.trait_fn_name();
        let field_types = self.field_type_list();
        let rec_type = self.rec_type.clone();
        let non_rec_type = self.non_rec_type.clone();
        quote!(fn #fn_name(&mut self, node: #non_rec_type, #field_types) -> #rec_type)
    }
}

fn supertype(name: &String, subtypes: &Vec<Type>, node_map: &HashMap<String, Root>) -> TokenStream {
    let subtypes: Vec<&Root> = subtypes
        .iter()
        .map(|t| node_map.get(&t.kind).expect("Could not find node"))
        .collect();
    let fields_by_type: HashMap<String, IndexMap<String, Child>> = subtypes
        .iter()
        .map(|n| (n.kind.clone(), n.fields()))
        .collect();
    let is_recursive = |child: &Child| name == &child.kind();

    let descs: Vec<FnDesc> = fields_by_type
        .iter()
        .map(|(name, fields)| {
            let field_types: IndexMap<Ident, FieldDesc> = fields
                .iter()
                .map(|(name, child)| {
                    let field_name = format_ident!("{}", name.to_case(Case::Snake));
                    (
                        field_name,
                        FieldDesc {
                            multiple: child.multiple,
                            recursive: is_recursive(child),
                            required: child.required,
                        },
                    )
                })
                .collect();

            let desc = FnDesc {
                rec_type: quote!(O),
                non_rec_type: quote!(I),
                name: name.clone(),
                field_types,
            };

            desc
        })
        .collect();

    let declarations = descs.iter().map(|desc| desc.trait_declaration());
    let map_impls = descs.iter().map(|desc| {
        let fn_name = desc.trait_fn_name();
        let decl = desc.trait_declaration();
        let fields = desc.field_types.iter().map(|(name, field_desc)| {
            if field_desc.recursive {
                quote!(#name)
            } else {
                field_desc.transform(name.clone(), quote!(mapper(#name)))
            }
        });
        quote! {
            #decl {
                let mapper = &self.mapper;
                self.walker.#fn_name(mapper(node), #(#fields),*)
            }
        }
    });

    let memoize_impls = descs.iter().map(|desc| {
        let fn_name = desc.trait_fn_name();
        let decl = desc.trait_declaration();
        let fields = desc.field_list();
        quote! {
            #decl {
                self.remember(node, |walker, node| walker.#fn_name(node, #fields))
            }
        }
    });

    let enum_variants = descs.iter().map(|desc| {
        let variant_name = desc.enum_name();
        let field_types = desc.field_types.iter().map(|(name, field)| {
            let t = field.wrap_type(quote!(I));
            quote!(#t)
        });
        quote!(#variant_name(#(#field_types),*))
    });

    let enum_matchers = descs.iter().map(|desc| {
        let fn_name = desc.trait_fn_name();
        let variant_name = desc.enum_name();
        let field_names = desc.field_list();

        let mut block = quote!();
        for (name, field) in desc.field_types.iter() {
            if field.recursive {
                let line = field.transform(name.clone(), quote!(self.visit(#name)));
                block = quote! {
                    #block
                    let #name = #line;
                }
            }
        }

        quote!(#variant_name(#field_names) => {
            #block
            self.#fn_name(node, #field_names)
        })
    });

    let enum_name = format_ident!("{}", name.to_case(Case::UpperCamel));
    let enum_impls = descs.iter().map(|desc| {
        let kind_literal = Literal::string(desc.name.as_str());
        let variant_name = desc.enum_name();
        let field_names = desc.field_list();

        let mut block = quote!();
        for (name, field) in desc.field_types.iter() {
            // FIXME: "arg" issue in field name
            let name_lit = Literal::string(name.to_string().as_str());
            let line = if field.multiple {
                quote!(let #name: Vec<Self> = self.children_by_field_name(#name_lit, &mut self.walk()).filter(|n| n.is_named()).collect())
            } else if field.required {
                quote!(let #name = self.child_by_field_name(#name_lit).unwrap())
            } else {
                quote!(let #name = self.child_by_field_name(#name_lit))
            };
            block = quote! {
                #block
                #line;
            }
        }

        quote!(#kind_literal => {
            #block
            #enum_name::#variant_name(#field_names)
        })
    });

    let trait_name = format_ident!("{}Visitor", name.to_case(Case::UpperCamel));

    let into_enum_trait_name = format_ident!("Into{}", enum_name);
    let into_enum_trait_fn_name = format_ident!("into_{}", name.to_case(Case::Snake));
    quote! {
        #[derive(Debug, PartialEq, Clone, PartialOrd)]
        pub enum #enum_name<I> {
            #(#enum_variants),*
        }

        pub trait #into_enum_trait_name: Sized {
            fn #into_enum_trait_fn_name(&self) -> #enum_name<Self>;

            fn visit<O>(self, visitor: &mut impl #trait_name<Self, O>) -> O {
                visitor.visit(self)
            }
        }

        impl #into_enum_trait_name for Node<'_> {
            fn #into_enum_trait_fn_name(&self) -> #enum_name<Self> {
                match self.kind() {
                    #(#enum_impls),*,
                    _ => unreachable!()
                }
            }
        }

        pub trait #trait_name<I, O>: Sized {
            fn remap<K, F: Fn(K) -> I>(self, f: F) -> Remap<Self, F> {
                Remap { walker: self, mapper: f}
            }
            fn memoize(self) -> Memoize<Self, I, O> {
                Memoize { walker: self, cache: HashMap::new(), phantom: PhantomData }
            }
            fn visit(&mut self, node: I) -> O where I: #into_enum_trait_name {
                match node.#into_enum_trait_fn_name() {
                    #(#enum_name::#enum_matchers),*
                }
            }
            #(#declarations);*;
        }

        impl <I,O,W,F,K> #trait_name<I, O> for Remap<W, F> where W: #trait_name<K, O>, F: Fn(I) -> K {
            #(#map_impls)*
        }

        impl <W, I, O> Memoize<W, I, O> where I: Hash, O: Clone {
            fn remember(&mut self, node: I, f: impl FnOnce(&mut W, I) -> O) -> O {
                // TODO: find another solution for this (like Into<u64> or something)
                let hash = {
                    let mut s = DefaultHasher::new();
                    node.hash(&mut s);
                    s.finish()
                };
                let walker = &mut self.walker;
                self.cache.entry(hash).or_insert_with(|| f(walker, node)).clone()
            }
        }

        impl <W, I, O> #trait_name<I,O> for Memoize<W, I, O> where W: #trait_name<I,O>, I: Hash, O: Clone {
            #(#memoize_impls)*
        }
    }
}

#[proc_macro]
pub fn node_types(input: RealTokenStream) -> RealTokenStream {
    let literal: LitStr = parse(input).expect("Input must be a literal string");
    let filepath = literal.value();
    let src = std::fs::read_to_string(filepath).expect("File not found");
    let json: Vec<Root> = serde_json::from_str(src.as_str()).expect("Invalid json");
    let node_map: HashMap<String, Root> = json
        .into_iter()
        .filter(|x| x.named)
        .map(|x| (x.kind.clone(), x))
        .collect();

    let traits = node_map.iter().map(|(name, root)| {
        if let Some(subtypes) = &root.subtypes {
            supertype(name, subtypes, &node_map)
        } else {
            quote!()
        }
    });

    quote! {
        // pub mod ast {
            use std::marker::PhantomData;
            use std::collections::HashMap;
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            use tree_sitter::Node;

            pub struct Remap<W, F> { walker: W, mapper: F}
            pub struct Memoize<W, I, O> { walker: W, cache: HashMap<u64, O>, phantom: PhantomData<I> }

            #(#traits)*
        // }
    }
    .into()
}
