use std::{collections::BTreeMap, rc::Rc};

use ::evm::{
    executor::stack::{MemoryStackState, StackExecutor, StackSubstateMetadata},
    Config, Context,
};
use anyhow::{anyhow, Result};
use evm::{
    backend::{MemoryBackend, MemoryVicinity},
    ExitError, ExitFatal, ExitReason, ExitRevert, ExitSucceed, Machine, Runtime,
};
use evm_rs::{self, encode_program, Opcode, Program};
use primitive_types::{H160, U256};

use crate::term::Term;

pub struct Compiler {
    ops: Vec<Opcode>,
}

impl Compiler {
    pub fn new() -> Self {
        Self { ops: vec![] }
    }

    pub fn compile(&mut self, term: &Term) {
        match term {
            Term::Num(_) => {}
            Term::Str(_) => {}
            Term::Var(_) => {}
            Term::Cons(_, _) => {}
            Term::Case(_, _, _) => {}
            Term::App(_, _) => {}
            Term::Abs(_, _) => {}
            Term::Rec(_) => {}
            Term::Acc(_, _) => {}
        }
    }

    pub fn result(self) -> Program {
        Program(self.ops)
    }
}

pub fn run(program: Program) -> Result<u64> {
    let code = encode_program(program);

    let data = Rc::new(code.clone());
    let context = Context {
        address: H160::from_low_u64_le(123),
        caller: H160::from_low_u64_le(456),
        apparent_value: U256::one(),
    };
    let config = Config::london();
    let mut runtime = Runtime::new(Rc::new(code), data, context, &config);
    let gas_limit = 123;
    let gas_price = U256::one();
    let metadata = StackSubstateMetadata::new(gas_limit, &config);
    let vicinity = MemoryVicinity {
        gas_price,
        origin: H160::from_low_u64_le(789),
        chain_id: U256::one(),
        block_hashes: vec![],
        block_number: U256::zero(),
        block_coinbase: H160::zero(),
        block_timestamp: U256::zero(),
        block_difficulty: U256::one(),
        block_gas_limit: U256::from(gas_limit),
        block_base_fee_per_gas: U256::one(),
    };
    let state = BTreeMap::new();
    let backend = MemoryBackend::new(&vicinity, state);
    let stack_state = MemoryStackState::new(metadata, &backend);
    let mut executor = StackExecutor::new_with_precompiles(stack_state, &config, &());
    let result = runtime.run(&mut executor);
    match result {
        ::evm::Capture::Exit(ExitReason::Succeed(ExitSucceed::Stopped)) => Ok(executor.used_gas()),
        ::evm::Capture::Exit(ExitReason::Error(err)) => Err(anyhow!("EVM Exit error: {:?}", err)),
        ::evm::Capture::Exit(ExitReason::Fatal(fatal)) => {
            Err(anyhow!("EVM FATAL error: {:?}", fatal))
        }
        _ => Err(anyhow!("EVM error")),
    }
}
