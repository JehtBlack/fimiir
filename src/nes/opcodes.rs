use super::cpu::{Cpu, CpuFlags};

/// https://www.nesdev.org/obelisk-6502-guide/reference.html
pub(crate) struct CpuOpCode {
    pub mnemonic: &'static str,
    pub addr_mode: AddressingMode,
    pub base_cycles: u8,
    pub micro_program: fn(&mut Cpu, &AddressingMode) -> u8,
}

pub(crate) enum AddressingMode {
    Implicit,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
}

impl CpuOpCode {
    fn new(
        mnemonic: &'static str,
        instruction_details: (AddressingMode, u8),
        micro_program: fn(&mut Cpu, &AddressingMode) -> u8,
    ) -> CpuOpCode {
        CpuOpCode {
            mnemonic,
            addr_mode: instruction_details.0,
            base_cycles: instruction_details.1,
            micro_program,
        }
    }
}

impl Default for CpuOpCode {
    fn default() -> Self {
        CpuOpCode {
            mnemonic: "???",
            addr_mode: AddressingMode::Implicit,
            base_cycles: 1,
            micro_program: Cpu::xxx,
        }
    }
}

lazy_static! {
    pub(crate) static ref CPU_OP_CODES: [CpuOpCode; 256] = {
        std::array::from_fn(|i| match i {
            // https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => CpuOpCode::new(
                "ADC",
                match i {
                    0x69 => (AddressingMode::Immediate, 2),
                    0x65 => (AddressingMode::ZeroPage, 3),
                    0x75 => (AddressingMode::ZeroPageX, 4),
                    0x6D => (AddressingMode::Absolute, 4),
                    0x7D => (AddressingMode::AbsoluteX, 4),
                    0x79 => (AddressingMode::AbsoluteY, 4),
                    0x61 => (AddressingMode::IndexedIndirect, 6),
                    0x71 => (AddressingMode::IndirectIndexed, 5),
                    _ => unreachable!(),
                },
                Cpu::adc,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#AND
            0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => CpuOpCode::new(
                "AND",
                match i {
                    0x29 => (AddressingMode::Immediate, 2),
                    0x25 => (AddressingMode::ZeroPage, 3),
                    0x35 => (AddressingMode::ZeroPageX, 4),
                    0x2D => (AddressingMode::Absolute, 4),
                    0x3D => (AddressingMode::AbsoluteX, 4),
                    0x39 => (AddressingMode::AbsoluteY, 4),
                    0x21 => (AddressingMode::IndexedIndirect, 6),
                    0x31 => (AddressingMode::IndirectIndexed, 5),
                    _ => unreachable!(),
                },
                Cpu::and,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL
            0x0A | 0x06 | 0x16 | 0x0E | 0x1E => CpuOpCode::new(
                "ASL",
                match i {
                    0x0A => (AddressingMode::Accumulator, 2),
                    0x06 => (AddressingMode::ZeroPage, 5),
                    0x16 => (AddressingMode::ZeroPageX, 6),
                    0x0E => (AddressingMode::Absolute, 6),
                    0x1E => (AddressingMode::AbsoluteX, 7),
                    _ => unreachable!(),
                },
                Cpu::asl,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BCC
            0x90 => CpuOpCode::new("BCC", (AddressingMode::Relative, 2), Cpu::bcc),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BCS
            0xB0 => CpuOpCode::new("BCS", (AddressingMode::Relative, 2), Cpu::bcs),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BEQ
            0xF0 => CpuOpCode::new("BEQ", (AddressingMode::Relative, 2), Cpu::beq),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BIT
            0x24 | 0x2C => CpuOpCode::new(
                "BIT",
                match i {
                    0x24 => (AddressingMode::ZeroPage, 3),
                    0x2C => (AddressingMode::Absolute, 4),
                    _ => unreachable!(),
                },
                Cpu::bit,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BMI
            0x30 => CpuOpCode::new("BMI", (AddressingMode::Relative, 2), Cpu::bmi),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BNE
            0xD0 => CpuOpCode::new("BNE", (AddressingMode::Relative, 2), Cpu::bne),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BPL
            0x10 => CpuOpCode::new("BPL", (AddressingMode::Relative, 2), Cpu::bpl),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BRK
            0x00 => CpuOpCode::new("BRK", (AddressingMode::Implicit, 7), Cpu::brk),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BVC
            0x50 => CpuOpCode::new("BVC", (AddressingMode::Relative, 2), Cpu::bvc),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#BVS
            0x70 => CpuOpCode::new("BVS", (AddressingMode::Relative, 2), Cpu::bvs),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#CLC
            0x18 => CpuOpCode::new("CLC", (AddressingMode::Implicit, 2), Cpu::clc),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#CLD
            0xD8 => CpuOpCode::new("CLD", (AddressingMode::Implicit, 2), Cpu::cld),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#CLI
            0x58 => CpuOpCode::new("CLI", (AddressingMode::Implicit, 2), Cpu::cli),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#CLV
            0xB8 => CpuOpCode::new("CLV", (AddressingMode::Implicit, 2), Cpu::clv),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP
            0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => CpuOpCode::new(
                "CMP",
                match i {
                    0xC9 => (AddressingMode::Immediate, 2),
                    0xC5 => (AddressingMode::ZeroPage, 3),
                    0xD5 => (AddressingMode::ZeroPageX, 4),
                    0xCD => (AddressingMode::Absolute, 4),
                    0xDD => (AddressingMode::AbsoluteX, 4),
                    0xD9 => (AddressingMode::AbsoluteY, 4),
                    0xC1 => (AddressingMode::IndexedIndirect, 6),
                    0xD1 => (AddressingMode::IndirectIndexed, 5),
                    _ => unreachable!(),
                },
                Cpu::cmp,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX
            0xE0 | 0xE4 | 0xEC => CpuOpCode::new(
                "CPX",
                match i {
                    0xE0 => (AddressingMode::Immediate, 2),
                    0xE4 => (AddressingMode::ZeroPage, 3),
                    0xEC => (AddressingMode::Absolute, 4),
                    _ => unreachable!(),
                },
                Cpu::cpx,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#CPY
            0xC0 | 0xC4 | 0xCC => CpuOpCode::new(
                "CPY",
                match i {
                    0xC0 => (AddressingMode::Immediate, 2),
                    0xC4 => (AddressingMode::ZeroPage, 3),
                    0xCC => (AddressingMode::Absolute, 4),
                    _ => unreachable!(),
                },
                Cpu::cpy,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#DEC
            0xC6 | 0xD6 | 0xCE | 0xDE => CpuOpCode::new(
                "DEC",
                match i {
                    0xC6 => (AddressingMode::ZeroPage, 5),
                    0xD6 => (AddressingMode::ZeroPageX, 6),
                    0xCE => (AddressingMode::Absolute, 6),
                    0xDE => (AddressingMode::AbsoluteX, 7),
                    _ => unreachable!(),
                },
                Cpu::dec,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#DEX
            0xCA => CpuOpCode::new("DEX", (AddressingMode::Implicit, 2), Cpu::dex),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#DEY
            0x88 => CpuOpCode::new("DEY", (AddressingMode::Implicit, 2), Cpu::dey),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR
            0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => CpuOpCode::new(
                "EOR",
                match i {
                    0x49 => (AddressingMode::Immediate, 2),
                    0x45 => (AddressingMode::ZeroPage, 3),
                    0x55 => (AddressingMode::ZeroPageX, 4),
                    0x4D => (AddressingMode::Absolute, 4),
                    0x5D => (AddressingMode::AbsoluteX, 4),
                    0x59 => (AddressingMode::AbsoluteY, 4),
                    0x41 => (AddressingMode::IndexedIndirect, 6),
                    0x51 => (AddressingMode::IndirectIndexed, 5),
                    _ => unreachable!(),
                },
                Cpu::eor,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#INC
            0xE6 | 0xF6 | 0xEE | 0xFE => CpuOpCode::new(
                "INC",
                match i {
                    0xE6 => (AddressingMode::ZeroPage, 5),
                    0xF6 => (AddressingMode::ZeroPageX, 6),
                    0xEE => (AddressingMode::Absolute, 6),
                    0xFE => (AddressingMode::AbsoluteX, 7),
                    _ => unreachable!(),
                },
                Cpu::inc,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#INX
            0xE8 => CpuOpCode::new("INX", (AddressingMode::Implicit, 2), Cpu::inx),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#INY
            0xC8 => CpuOpCode::new("INY", (AddressingMode::Implicit, 2), Cpu::iny),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
            0x4C | 0x6C => CpuOpCode::new(
                "JMP",
                match i {
                    0x4C => (AddressingMode::Absolute, 3),
                    0x6C => (AddressingMode::Indirect, 5),
                    _ => unreachable!(),
                },
                Cpu::jmp,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#JSR
            0x20 => CpuOpCode::new("JSR", (AddressingMode::Absolute, 6), Cpu::jsr),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA
            0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => CpuOpCode::new(
                "LDA",
                match i {
                    0xA9 => (AddressingMode::Immediate, 2),
                    0xA5 => (AddressingMode::ZeroPage, 3),
                    0xB5 => (AddressingMode::ZeroPageX, 4),
                    0xAD => (AddressingMode::Absolute, 4),
                    0xBD => (AddressingMode::AbsoluteX, 4),
                    0xB9 => (AddressingMode::AbsoluteY, 4),
                    0xA1 => (AddressingMode::IndexedIndirect, 6),
                    0xB1 => (AddressingMode::IndirectIndexed, 5),
                    _ => unreachable!(),
                },
                Cpu::lda,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#LDX
            0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => CpuOpCode::new(
                "LDX",
                match i {
                    0xA2 => (AddressingMode::Immediate, 2),
                    0xA6 => (AddressingMode::ZeroPage, 3),
                    0xB6 => (AddressingMode::ZeroPageY, 4),
                    0xAE => (AddressingMode::Absolute, 4),
                    0xBE => (AddressingMode::AbsoluteY, 4),
                    _ => unreachable!(),
                },
                Cpu::ldx,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#LDY
            0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => CpuOpCode::new(
                "LDY",
                match i {
                    0xA0 => (AddressingMode::Immediate, 2),
                    0xA4 => (AddressingMode::ZeroPage, 3),
                    0xB4 => (AddressingMode::ZeroPageX, 4),
                    0xAC => (AddressingMode::Absolute, 4),
                    0xBC => (AddressingMode::AbsoluteX, 4),
                    _ => unreachable!(),
                },
                Cpu::ldy,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#LSR
            0x4A | 0x46 | 0x56 | 0x4E | 0x5E => CpuOpCode::new(
                "LSR",
                match i {
                    0x4A => (AddressingMode::Accumulator, 2),
                    0x46 => (AddressingMode::ZeroPage, 5),
                    0x56 => (AddressingMode::ZeroPageX, 6),
                    0x4E => (AddressingMode::Absolute, 6),
                    0x5E => (AddressingMode::AbsoluteX, 7),
                    _ => unreachable!(),
                },
                Cpu::lsr,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#NOP
            0xEA => CpuOpCode::new("NOP", (AddressingMode::Implicit, 2), Cpu::nop),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#ORA
            0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => CpuOpCode::new(
                "ORA",
                match i {
                    0x09 => (AddressingMode::Immediate, 2),
                    0x05 => (AddressingMode::ZeroPage, 3),
                    0x15 => (AddressingMode::ZeroPageX, 4),
                    0x0D => (AddressingMode::Absolute, 4),
                    0x1D => (AddressingMode::AbsoluteX, 4),
                    0x19 => (AddressingMode::AbsoluteY, 4),
                    0x01 => (AddressingMode::IndexedIndirect, 6),
                    0x11 => (AddressingMode::IndirectIndexed, 5),
                    _ => unreachable!(),
                },
                Cpu::ora,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#PHA
            0x48 => CpuOpCode::new("PHA", (AddressingMode::Implicit, 3), Cpu::pha),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#PHP
            0x08 => CpuOpCode::new("PHP", (AddressingMode::Implicit, 3), Cpu::php),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#PLA
            0x68 => CpuOpCode::new("PLA", (AddressingMode::Implicit, 4), Cpu::pla),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#PLP
            0x28 => CpuOpCode::new("PLP", (AddressingMode::Implicit, 4), Cpu::plp),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#ROL
            0x2A | 0x26 | 0x36 | 0x2E | 0x3E => CpuOpCode::new(
                "ROL",
                match i {
                    0x2A => (AddressingMode::Accumulator, 2),
                    0x26 => (AddressingMode::ZeroPage, 5),
                    0x36 => (AddressingMode::ZeroPageX, 6),
                    0x2E => (AddressingMode::Absolute, 6),
                    0x3E => (AddressingMode::AbsoluteX, 7),
                    _ => unreachable!(),
                },
                Cpu::rol,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR
            0x6A | 0x66 | 0x76 | 0x6E | 0x7E => CpuOpCode::new(
                "ROR",
                match i {
                    0x6A => (AddressingMode::Accumulator, 2),
                    0x66 => (AddressingMode::ZeroPage, 5),
                    0x76 => (AddressingMode::ZeroPageX, 6),
                    0x6E => (AddressingMode::Absolute, 6),
                    0x7E => (AddressingMode::AbsoluteX, 7),
                    _ => unreachable!(),
                },
                Cpu::ror,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#RTI
            0x40 => CpuOpCode::new("RTI", (AddressingMode::Implicit, 6), Cpu::rti),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#RTS
            0x60 => CpuOpCode::new("RTS", (AddressingMode::Implicit, 6), Cpu::rts),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC
            0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => CpuOpCode::new(
                "SBC",
                match i {
                    0xE9 => (AddressingMode::Immediate, 2),
                    0xE5 => (AddressingMode::ZeroPage, 3),
                    0xF5 => (AddressingMode::ZeroPageX, 4),
                    0xED => (AddressingMode::Absolute, 4),
                    0xFD => (AddressingMode::AbsoluteX, 4),
                    0xF9 => (AddressingMode::AbsoluteY, 4),
                    0xE1 => (AddressingMode::IndexedIndirect, 6),
                    0xF1 => (AddressingMode::IndirectIndexed, 5),
                    _ => unreachable!(),
                },
                Cpu::sbc,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#SEC
            0x38 => CpuOpCode::new("SEC", (AddressingMode::Implicit, 2), Cpu::sec),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#SED
            0xF8 => CpuOpCode::new("SED", (AddressingMode::Implicit, 2), Cpu::sed),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI
            0x78 => CpuOpCode::new("SEI", (AddressingMode::Implicit, 2), Cpu::sei),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#STA
            0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => CpuOpCode::new(
                "STA",
                match i {
                    0x85 => (AddressingMode::ZeroPage, 3),
                    0x95 => (AddressingMode::ZeroPageX, 4),
                    0x8D => (AddressingMode::Absolute, 4),
                    0x9D => (AddressingMode::AbsoluteX, 5),
                    0x99 => (AddressingMode::AbsoluteY, 5),
                    0x81 => (AddressingMode::IndexedIndirect, 6),
                    0x91 => (AddressingMode::IndirectIndexed, 6),
                    _ => unreachable!(),
                },
                Cpu::sta,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#STX
            0x86 | 0x96 | 0x8E => CpuOpCode::new(
                "STX",
                match i {
                    0x86 => (AddressingMode::ZeroPage, 3),
                    0x96 => (AddressingMode::ZeroPageY, 4),
                    0x8E => (AddressingMode::Absolute, 4),
                    _ => unreachable!(),
                },
                Cpu::stx,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#STY
            0x84 | 0x94 | 0x8C => CpuOpCode::new(
                "STY",
                match i {
                    0x84 => (AddressingMode::ZeroPage, 3),
                    0x94 => (AddressingMode::ZeroPageX, 4),
                    0x8C => (AddressingMode::Absolute, 4),
                    _ => unreachable!(),
                },
                Cpu::sty,
            ),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#TAZ
            0xAA => CpuOpCode::new("TAX", (AddressingMode::Implicit, 2), Cpu::tax),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#TAY
            0xA8 => CpuOpCode::new("TAY", (AddressingMode::Implicit, 2), Cpu::tay),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#TSX
            0xBA => CpuOpCode::new("TSX", (AddressingMode::Implicit, 2), Cpu::tsx),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#TXA
            0x8A => CpuOpCode::new("TXA", (AddressingMode::Implicit, 2), Cpu::txa),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#TXS
            0x9A => CpuOpCode::new("TXS", (AddressingMode::Implicit, 2), Cpu::txs),

            // https://www.nesdev.org/obelisk-6502-guide/reference.html#TYA
            0x98 => CpuOpCode::new("TYA", (AddressingMode::Implicit, 2), Cpu::tya),

            _ => CpuOpCode::default(),
        })
    };
}

pub(crate) enum ReadOperandResult {
    AccumulatorValue(u8),
    AbsoluteAddress {
        addr: u16,
        additional_cycles_if_page_crossed: u8,
    },
    RelativeAddress(u16),
}

impl AddressingMode {
    pub fn read_operand(&self, cpu: &mut Cpu) -> ReadOperandResult {
        match self {
            AddressingMode::Accumulator => ReadOperandResult::AccumulatorValue(cpu.register_a()),
            // all other modes will read a byte from memory with a calculated address
            AddressingMode::Immediate => {
                let pc = cpu.program_counter();
                cpu.increment_program_counter(1);
                ReadOperandResult::AbsoluteAddress {
                    addr: pc,
                    additional_cycles_if_page_crossed: 0,
                }
            }
            AddressingMode::ZeroPage => {
                let zero_page_addr = cpu.read_byte(cpu.program_counter()) as u16;
                cpu.increment_program_counter(1);
                ReadOperandResult::AbsoluteAddress {
                    addr: zero_page_addr & 0x00FF,
                    additional_cycles_if_page_crossed: 0,
                }
            }
            AddressingMode::ZeroPageX => {
                let zero_page_addr = cpu.read_byte(cpu.program_counter()) as u16;
                cpu.increment_program_counter(1);
                let zero_page_addr = (zero_page_addr + cpu.register_x() as u16) & 0x00FF;
                ReadOperandResult::AbsoluteAddress {
                    addr: zero_page_addr,
                    additional_cycles_if_page_crossed: 0,
                }
            }
            AddressingMode::ZeroPageY => {
                let zero_page_addr = cpu.read_byte(cpu.program_counter()) as u16;
                cpu.increment_program_counter(1);
                let zero_page_addr = (zero_page_addr + cpu.register_y() as u16) & 0x00FF;
                ReadOperandResult::AbsoluteAddress {
                    addr: zero_page_addr,
                    additional_cycles_if_page_crossed: 0,
                }
            }
            AddressingMode::Relative => {
                let relative_addr = cpu.read_byte(cpu.program_counter()) as u16;
                cpu.increment_program_counter(1);
                let relative_addr = if (relative_addr & CpuFlags::N).into() {
                    relative_addr | 0xFF00
                } else {
                    relative_addr
                };
                ReadOperandResult::RelativeAddress(relative_addr)
            }
            AddressingMode::Absolute => {
                let address = cpu.read_short(cpu.program_counter());
                cpu.increment_program_counter(2);
                ReadOperandResult::AbsoluteAddress {
                    addr: address,
                    additional_cycles_if_page_crossed: 0,
                }
            }
            AddressingMode::AbsoluteX => {
                let base_address = cpu.read_short(cpu.program_counter());
                cpu.increment_program_counter(2);
                let address = base_address + cpu.register_x() as u16;
                let extra_cycles = if base_address & 0xFF00 != address & 0xFF00 {
                    1
                } else {
                    0
                };
                ReadOperandResult::AbsoluteAddress {
                    addr: address,
                    additional_cycles_if_page_crossed: extra_cycles,
                }
            }
            AddressingMode::AbsoluteY => {
                let base_address = cpu.read_short(cpu.program_counter());
                cpu.increment_program_counter(2);
                let address = base_address + cpu.register_y() as u16;
                let extra_cycles = if base_address & 0xFF00 != address & 0xFF00 {
                    1
                } else {
                    0
                };
                ReadOperandResult::AbsoluteAddress {
                    addr: address,
                    additional_cycles_if_page_crossed: extra_cycles,
                }
            }
            AddressingMode::Indirect => {
                let indirect_address = cpu.read_short(cpu.program_counter());
                cpu.increment_program_counter(2);
                let address = if (indirect_address & 0x00FF) == 0x00FF {
                    // bug: the 6502 has a hardware bug when reading at a page boundary, this simulates that
                    ((cpu.read_byte(indirect_address & 0xFF00) as u16) << 8)
                        | cpu.read_byte(indirect_address) as u16
                } else {
                    cpu.read_short(indirect_address)
                };
                ReadOperandResult::AbsoluteAddress {
                    addr: address,
                    additional_cycles_if_page_crossed: 0,
                }
            }
            AddressingMode::IndexedIndirect => {
                let zero_page_addr = cpu.read_byte(cpu.program_counter());
                cpu.increment_program_counter(1);
                let lo =
                    cpu.read_byte(((zero_page_addr + cpu.register_x()) & 0x00FF) as u16) as u16;
                let hi =
                    cpu.read_byte(((zero_page_addr + cpu.register_x() + 1) & 0x00FF) as u16) as u16;
                let indirect_address = (hi << 8) | lo;
                ReadOperandResult::AbsoluteAddress {
                    addr: indirect_address,
                    additional_cycles_if_page_crossed: 0,
                }
            }
            AddressingMode::IndirectIndexed => {
                let zero_page_addr = cpu.read_byte(cpu.program_counter()) as u16;
                cpu.increment_program_counter(1);
                let lo = cpu.read_byte(zero_page_addr & 0x00FF) as u16;
                let hi = cpu.read_byte((zero_page_addr + 1) & 0x00FF) as u16;
                let indirect_address = (hi << 8) | lo;
                let address = indirect_address + cpu.register_y() as u16;
                let extra_cycles = if indirect_address & 0xFF00 != address & 0xFF00 {
                    1
                } else {
                    0
                };
                ReadOperandResult::AbsoluteAddress {
                    addr: address,
                    additional_cycles_if_page_crossed: extra_cycles,
                }
            }
            AddressingMode::Implicit => {
                unreachable!("Implicit addressing mode does not read an operand")
            }
        }
    }
}
