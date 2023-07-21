use std::sync::Arc;

use inkwell::{context::Context, builder::Builder, module::Module, values::{FloatValue, BasicValue, BasicValueEnum, BasicMetadataValueEnum, FunctionValue}, basic_block::BasicBlock};

use crate::term::Term;
use anyhow::{Ok, Result};



pub struct CodeGen<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    current_function: FunctionValue<'ctx>,
    curent_block: BasicBlock<'ctx>
}

impl <'a, 'ctx> CodeGen<'a, 'ctx> {
    fn gen(&mut self, term: Arc<Term>) -> Result<BasicValueEnum<'ctx>> {
        dbg!(term.clone(), self.current_function.get_name());
        match term.as_ref() {
            Term::Lit(_, lit) => match *lit {
                crate::term::Lit::Num(x) => Ok(self.context.i32_type().const_int(x as u64, false).into()),
                crate::term::Lit::Str(_) => todo!(),
            },
            Term::Var(_, name) => {
                let param = self.current_function.get_first_param().unwrap();
                Ok(param)
            },
            Term::App(_, f, args) => {
                let f = self.gen(Arc::clone(f))?;
                let args = args.into_iter().map(|arg| self.gen(Arc::clone(arg))).collect::<Result<Vec<_>, _>>()?;
                let args = args.into_iter().map(|x| BasicMetadataValueEnum::from(x)).collect::<Vec<_>>();
                let args = args.as_slice();
                                
                // fake fn type
                let i32_t = self.context.i32_type();
                let fn_type = i32_t.fn_type((0..args.len()).map(|_| i32_t.into()).collect::<Vec<_>>().as_slice(), false);

                let call = self.builder.build_indirect_call(fn_type, f.into_pointer_value(), args, "asdf");

                let val = call.try_as_basic_value().unwrap_left();

                Ok(val)
            },
            Term::Lam(_, params, body) => {
                let prev_f = self.current_function;
                let prev_block = self.curent_block;
                let i32_t = self.context.i32_type();
                let fn_type = i32_t.fn_type((0..params.len()).map(|_| i32_t.into()).collect::<Vec<_>>().as_slice(), false);

                let f = self.module.add_function("foobar", fn_type, None);
                self.current_function = f;
                
                let block = self.context.append_basic_block(f, "entry");
                self.curent_block = block;
                self.builder.position_at_end(block);

                let body = self.gen(Arc::clone(body))?;

                self.builder.build_return(Some(&body));
                
                self.current_function = prev_f;
                self.curent_block = prev_block;
                self.builder.position_at_end(self.curent_block);

                let f_ref = f.as_global_value().as_pointer_value();
                Ok(f_ref.into())
            }
            _ => todo!()
        }
    }

    pub fn compile(context: &'ctx Context, term: Arc<Term>) -> Result<Module<'ctx>> {
        let builder = context.create_builder();
        let module = context.create_module("main");

        let main = module.add_function("_main", context.i32_type().fn_type(&[], false), None);
        let block = context.append_basic_block(main, "entry");
        builder.position_at_end(block);

        let mut codegen = CodeGen { 
            context: &context, 
            builder: &builder, 
            module: &module,
            current_function: main,
            curent_block: block,
        };

        let val = codegen.gen(term)?;

        builder.build_return(Some(&val));

        Ok(module)
    }
}