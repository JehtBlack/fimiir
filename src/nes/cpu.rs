use std::ops::BitAnd;

use super::{
    bus::Bus,
    mappers::Mapper,
    opcodes::{AddressingMode, ReadOperandResult, CPU_OP_CODES},
};

const PRG_ROM_START: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xFFFF;
const STACK_START: u16 = 0x0100;
const STACK_RESET: u8 = 0xFD;
const RESET_VECTOR: u16 = 0xFFFC;
const NMI_VECTOR: u16 = 0xFFFA;
const IRQ_BRK_VECTOR: u16 = 0xFFFE;

bitflags! {
    #[derive(Clone)]
    pub(crate) struct CpuFlags : u8 {
        const C = 0b00000001; // Carry Bit
        const Z = 0b00000010; // Zero
        const I = 0b00000100; // Disable Interrupts
        const D = 0b00001000; // Decimal Mode (unused in the NES)
        const B = 0b00010000; // Break
        const U = 0b00100000; // Unused
        const V = 0b01000000; // Overflow
        const N = 0b10000000; // Negative
    }
}

pub(crate) struct Cpu {
    opcode: u8,
    cycles: u8,

    // Registers
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: CpuFlags,

    bus: Bus,
}

impl Cpu {
    pub fn new(mapper: Box<dyn Mapper>) -> Self {
        Self {
            opcode: 0,
            cycles: 0,
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: CpuFlags::empty(),
            bus: Bus::new(mapper),
        }
    }

    pub fn reset(&mut self) {
        self.bus.reset();

        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = STACK_RESET;
        self.status = CpuFlags::U;

        self.pc = self.read_short(RESET_VECTOR);

        self.cycles = 8; // reset takes time
    }

    pub fn register_a(&self) -> u8 {
        self.a
    }

    pub fn register_x(&self) -> u8 {
        self.x
    }

    pub fn register_y(&self) -> u8 {
        self.y
    }

    pub fn program_counter(&self) -> u16 {
        self.pc
    }

    pub fn increment_program_counter(&mut self, amount: u16) {
        self.pc += amount;
    }

    pub fn cycles_remaining(&self) -> u8 {
        self.cycles
    }

    pub fn complete(&self) -> bool {
        self.cycles == 0
    }

    pub fn fill_buffer_with_pattern_table<F>(
        &mut self,
        table_index: usize,
        colour_palette_index: usize,
        set_pixel_action: &mut F,
    ) where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        self.bus.fill_buffer_with_pattern_table(
            table_index,
            colour_palette_index,
            set_pixel_action,
        );
    }

    pub fn tick_frame<F>(&mut self, system_cycle: usize, set_pixel_action: &mut F) -> usize
    where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        let ppu_scanlines_per_frame: usize = 262;
        let ppu_pixels_per_scanline: usize = 341;
        let ppu_cycles_per_frame: usize = ppu_pixels_per_scanline * ppu_scanlines_per_frame;

        let mut cycle: usize = system_cycle;
        let system_cycle_at_end_of_frame = cycle + ppu_cycles_per_frame;
        while cycle < system_cycle_at_end_of_frame {
            let (ppu_cycles, nmi_triggered, irq_state) = self.bus.tick(cycle, set_pixel_action);
            cycle += ppu_cycles;

            // CPU ticks every 3 PPU ticks
            let cpu_should_tick = cycle % 3 == 0;
            if cpu_should_tick {
                // DMA transfer blocks the CPU for the duration of the transfer
                if self.bus.dma_transfer_active() {
                    self.bus.dma_transfer();
                } else {
                    self.tick();
                }
            }

            if nmi_triggered {
                self.non_maskable_interrupt();
            }

            if irq_state {
                self.interrupt_request();
            }
        }

        system_cycle_at_end_of_frame
    }

    fn tick(&mut self) {
        if self.cycles == 0 {
            self.opcode = self.read_byte(self.pc);
            self.status.insert(CpuFlags::U);
            self.pc += 1;

            let cpu_op_code = &CPU_OP_CODES[self.opcode as usize];
            self.cycles =
                cpu_op_code.base_cycles + (cpu_op_code.micro_program)(self, &cpu_op_code.addr_mode);
            self.status.insert(CpuFlags::U);
        }

        self.cycles -= 1;
    }

    fn interrupt_request(&mut self) {
        if !self.status.contains(CpuFlags::I) {
            self.push_stack_short(self.pc);

            self.status.remove(CpuFlags::B);
            self.status.insert(CpuFlags::U);
            self.status.insert(CpuFlags::I);
            self.push_stack_byte(self.status.bits());

            self.pc = self.read_short(IRQ_BRK_VECTOR);

            self.cycles = 7;
        }
    }

    fn non_maskable_interrupt(&mut self) {
        self.push_stack_short(self.pc);

        self.status.remove(CpuFlags::B);
        self.status.insert(CpuFlags::U);
        self.status.insert(CpuFlags::I);
        self.push_stack_byte(self.status.bits());

        self.pc = self.read_short(NMI_VECTOR);

        self.cycles = 8;
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        self.bus.read_byte(addr)
    }

    pub fn write_byte(&mut self, addr: u16, data: u8) {
        self.bus.write_byte(addr, data);
    }

    pub fn read_short(&mut self, addr: u16) -> u16 {
        let lo = self.read_byte(addr) as u16;
        let hi = self.read_byte(addr + 1) as u16;
        (hi << 8) | lo
    }

    pub fn write_short(&mut self, addr: u16, data: u16) {
        let lo = data as u8;
        let hi = (data >> 8) as u8;
        self.write_byte(addr, lo);
        self.write_byte(addr + 1, hi);
    }

    fn pop_stack_byte(&mut self) -> u8 {
        self.sp += 1;
        self.read_byte(STACK_START + self.sp as u16)
    }

    fn push_stack_byte(&mut self, data: u8) {
        self.write_byte(STACK_START + self.sp as u16, data);
        self.sp -= 1;
    }

    fn pop_stack_short(&mut self) -> u16 {
        let lo = self.pop_stack_byte() as u16;
        let hi = self.pop_stack_byte() as u16;
        (hi << 8) | lo
    }

    fn push_stack_short(&mut self, data: u16) {
        self.push_stack_byte((data >> 8) as u8);
        self.push_stack_byte(data as u8);
    }

    fn branch(&mut self, addr: u16, condition: bool) -> u8 {
        let mut additional_cycles = 0;
        if condition {
            additional_cycles += 1;
            let addr = self.pc.wrapping_add(addr);
            if (self.pc & 0xFF00) != (addr & 0xFF00) {
                additional_cycles += 1;
            }
            self.pc = addr;
        }
        additional_cycles
    }

    fn add_to_accumulator(&mut self, value: u8) {
        let sum = self.a as u16 + value as u16 + (self.status.bits() & CpuFlags::C).as_u16();
        let result = (sum & 0x00FF) as u8;
        self.status.set(CpuFlags::C, sum > 255);
        self.status.set(
            CpuFlags::V,
            // get the bits of the two operands are the same
            // then get the bits of the result that are different bits of the left operand
            // mask so only the sign bit is considered
            // thus if the sign bit is the same between the operands and different in the result then overflow
            (!(self.a as u16 ^ value as u16) & (self.a as u16 ^ sum) & CpuFlags::N).into(),
        );
        self.set_zero_and_negative_flags(result);
        self.a = result;
    }

    fn set_zero_and_negative_flags(&mut self, value: u8) {
        self.status.set(CpuFlags::Z, value == 0);
        self.status.set(CpuFlags::N, (value & CpuFlags::N).into());
    }

    pub fn adc(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                let read_value = self.read_byte(addr);
                self.add_to_accumulator(read_value);
                additional_cycles_if_page_crossed
            }
            _ => unreachable!("ADC Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn and(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                let read_value = self.read_byte(addr);
                let result = self.a & read_value;
                self.set_zero_and_negative_flags(result);
                self.a = result;

                additional_cycles_if_page_crossed
            }
            _ => unreachable!("AND Doesn't support relative or accumulator addressing."),
        }
    }

    fn asl_impl(&mut self, value: u16) -> u8 {
        let temp = value << 1;
        let result = temp as u8;
        self.status.set(CpuFlags::C, (temp & 0xFF00) > 0);
        self.set_zero_and_negative_flags(result);
        result
    }

    pub fn asl(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AccumulatorValue(value) => {
                let result = self.asl_impl(value as u16);
                self.a = result;
                0
            }
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr) as u16;
                let result = self.asl_impl(read_value);
                self.write_byte(addr, result);
                0 // ASL never incurs a page crossed cycle penalty
            }
            _ => unreachable!("ASL Doesn't support relative addressing."),
        }
    }

    pub fn bcc(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, !self.status.contains(CpuFlags::C))
            }
            _ => unreachable!("BCC Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn bcs(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, self.status.contains(CpuFlags::C))
            }
            _ => unreachable!("BCS Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn beq(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, self.status.contains(CpuFlags::Z))
            }
            _ => unreachable!("BEQ Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn bit(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr);
                let result = self.a & read_value;
                self.status.set(CpuFlags::Z, result == 0);
                self.status
                    .set(CpuFlags::N, (read_value & CpuFlags::N).into());
                self.status
                    .set(CpuFlags::V, (read_value & CpuFlags::V).into());
                0 // BIT never incurs a page crossed cycle penalty
            }
            _ => unreachable!("BIT Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn bmi(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, self.status.contains(CpuFlags::N))
            }
            _ => unreachable!("BMI Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn bne(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, !self.status.contains(CpuFlags::Z))
            }
            _ => unreachable!("BNE Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn bpl(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, !self.status.contains(CpuFlags::N))
            }
            _ => unreachable!("BPL Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn brk(&mut self, _addr_mode: &AddressingMode) -> u8 {
        // BRK is a 2 byte instruction, so we need to increment the PC by 1
        // https://www.nesdev.org/the%20%27B%27%20flag%20&%20BRK%20instruction.txt
        self.pc += 1;
        self.push_stack_short(self.pc);
        self.push_stack_byte((self.status.clone() | CpuFlags::B).bits());
        self.pc = self.read_short(IRQ_BRK_VECTOR);
        0
    }

    pub fn bvc(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, !self.status.contains(CpuFlags::V))
            }
            _ => unreachable!("BVC Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn bvs(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::RelativeAddress(addr) => {
                self.branch(addr, self.status.contains(CpuFlags::V))
            }
            _ => unreachable!("BVS Doesn't support absolute or accumulator addressing."),
        }
    }

    pub fn clc(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status.remove(CpuFlags::C);
        0
    }

    pub fn cld(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status.remove(CpuFlags::D);
        0
    }

    pub fn cli(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status.remove(CpuFlags::I);
        0
    }

    pub fn clv(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status.remove(CpuFlags::V);
        0
    }

    pub fn cmp(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                let read_value = self.read_byte(addr);
                let result = self.a.wrapping_sub(read_value);
                self.status.set(CpuFlags::C, self.a >= read_value);
                self.set_zero_and_negative_flags(result);

                additional_cycles_if_page_crossed
            }
            _ => unreachable!("CMP Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn cpx(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr);
                let result = self.x.wrapping_sub(read_value);
                self.status.set(CpuFlags::C, self.x >= read_value);
                self.set_zero_and_negative_flags(result);
                0 // CPX never incurs a page crossed cycle penalty
            }
            _ => unreachable!("CPX Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn cpy(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr);
                let result = self.y.wrapping_sub(read_value);
                self.status.set(CpuFlags::C, self.y >= read_value);
                self.set_zero_and_negative_flags(result);
                0 // CPY never incurs a page crossed cycle penalty
            }
            _ => unreachable!("CPY Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn dec(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr);
                let result = read_value.wrapping_sub(1);
                self.set_zero_and_negative_flags(result);
                self.write_byte(addr, result);
                0 // DEC never incurs a page crossed cycle penalty
            }
            _ => unreachable!("DEC Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn dex(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.x = self.x.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.x);
        0
    }

    pub fn dey(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.y = self.y.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.y);
        0
    }

    pub fn eor(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                let read_value = self.read_byte(addr);
                self.a = self.a ^ read_value;
                self.set_zero_and_negative_flags(self.a);

                additional_cycles_if_page_crossed
            }
            _ => unreachable!("EOR Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn inc(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr);
                let result = read_value.wrapping_add(1);
                self.set_zero_and_negative_flags(result);
                self.write_byte(addr, result);
                0 // INC never incurs a page crossed cycle penalty
            }
            _ => unreachable!("INC Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn inx(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.x = self.x.wrapping_add(1);
        self.set_zero_and_negative_flags(self.x);
        0
    }

    pub fn iny(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.y = self.y.wrapping_add(1);
        self.set_zero_and_negative_flags(self.y);
        0
    }

    pub fn jmp(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                self.pc = addr;
                0 // JMP never incurs a page crossed cycle penalty
            }
            _ => unreachable!("JMP Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn jsr(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                self.push_stack_short(self.pc - 1);
                self.pc = addr;
                0 // JSR never incurs a page crossed cycle penalty
            }
            _ => unreachable!("JSR Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn lda(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                self.a = self.read_byte(addr);
                self.set_zero_and_negative_flags(self.a);

                additional_cycles_if_page_crossed
            }
            _ => unreachable!("LDA Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn ldx(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                self.x = self.read_byte(addr);
                self.set_zero_and_negative_flags(self.x);

                additional_cycles_if_page_crossed
            }
            _ => unreachable!("LDX Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn ldy(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                self.y = self.read_byte(addr);
                self.set_zero_and_negative_flags(self.y);

                additional_cycles_if_page_crossed
            }
            _ => unreachable!("LDY Doesn't support relative or accumulator addressing."),
        }
    }

    fn lsr_impl(&mut self, value: u8) -> u8 {
        let result = value >> 1;
        self.status.set(CpuFlags::C, (value & CpuFlags::C).into());
        self.set_zero_and_negative_flags(result);
        result
    }

    pub fn lsr(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AccumulatorValue(value) => {
                let result = self.lsr_impl(value);
                self.a = result;
                0
            }
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr);
                let result = self.lsr_impl(read_value);
                self.write_byte(addr, result);

                0 // LSR never incurs a page crossed cycle penalty
            }
            _ => unreachable!("LSR Doesn't support relative addressing."),
        }
    }

    pub fn nop(&mut self, _addr_mode: &AddressingMode) -> u8 {
        match self.opcode {
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => 1, // unnoficial opcodes that map to a nop but waste an extra cycle compared with nop
            _ => 0,
        }
    }

    pub fn ora(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                let read_value = self.read_byte(addr);
                self.a = self.a | read_value;
                self.set_zero_and_negative_flags(self.a);

                additional_cycles_if_page_crossed
            }
            _ => unreachable!("ORA Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn pha(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.push_stack_byte(self.a);
        0
    }

    pub fn php(&mut self, _addr_mode: &AddressingMode) -> u8 {
        let status = self.status.clone() | CpuFlags::B | CpuFlags::U;
        self.push_stack_byte(status.bits());
        self.status.remove(CpuFlags::B);
        self.status.remove(CpuFlags::U);
        0
    }

    pub fn pla(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.a = self.pop_stack_byte();
        self.set_zero_and_negative_flags(self.a);
        0
    }

    pub fn plp(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status = CpuFlags::from_bits_truncate(self.pop_stack_byte());
        0
    }

    fn rol_impl(&mut self, value: u16) -> u8 {
        let temp = ((value as u16) << 1) | (self.status.bits() & CpuFlags::C).bits() as u16;
        let result = temp as u8;
        self.status.set(CpuFlags::C, (temp & 0xFF00) > 0);
        self.set_zero_and_negative_flags(result);
        result
    }

    pub fn rol(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AccumulatorValue(value) => {
                self.a = self.rol_impl(value as u16);
                0
            }
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr) as u16;
                let result = self.rol_impl(read_value);
                self.write_byte(addr, result);
                0 // ROL never incurs a page crossed cycle penalty
            }
            _ => unreachable!("ROL Doesn't support relative addressing."),
        }
    }

    fn ror_impl(&mut self, value: u8) -> u8 {
        let result = (((self.status.bits() & CpuFlags::C).bits()) << 7) | (value >> 1);
        self.status.set(CpuFlags::C, (value & CpuFlags::C).into());
        self.set_zero_and_negative_flags(result);
        result
    }

    pub fn ror(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AccumulatorValue(value) => {
                self.a = self.ror_impl(value);
                0
            }
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                let read_value = self.read_byte(addr);
                let result = self.ror_impl(read_value);
                self.write_byte(addr, result);
                0 // ROR never incurs a page crossed cycle penalty
            }
            _ => unreachable!("ROR Doesn't support relative addressing."),
        }
    }

    pub fn rti(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status = CpuFlags::from_bits_truncate(self.pop_stack_byte());
        self.status.remove(CpuFlags::B);
        self.status.remove(CpuFlags::U);

        self.pc = self.pop_stack_short();
        0
    }

    pub fn rts(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.pc = self.pop_stack_short() + 1;
        0
    }

    pub fn sbc(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress {
                addr,
                additional_cycles_if_page_crossed,
            } => {
                let read_value = self.read_byte(addr);
                self.add_to_accumulator(!read_value);
                additional_cycles_if_page_crossed
            }
            _ => unreachable!("SBC Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn sec(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status.insert(CpuFlags::C);
        0
    }

    pub fn sed(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status.insert(CpuFlags::D);
        0
    }

    pub fn sei(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.status.insert(CpuFlags::I);
        0
    }

    pub fn sta(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                self.write_byte(addr, self.a);
                0 // STA never incurs a page crossed cycle penalty
            }
            _ => unreachable!("STA Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn stx(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                self.write_byte(addr, self.x);
                0 // STX never incurs a page crossed cycle penalty
            }
            _ => unreachable!("STX Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn sty(&mut self, addr_mode: &AddressingMode) -> u8 {
        let operand = addr_mode.read_operand(self);
        match operand {
            ReadOperandResult::AbsoluteAddress { addr, .. } => {
                self.write_byte(addr, self.y);
                0 // STY never incurs a page crossed cycle penalty
            }
            _ => unreachable!("STY Doesn't support relative or accumulator addressing."),
        }
    }

    pub fn tax(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.x = self.a;
        self.set_zero_and_negative_flags(self.x);
        0
    }

    pub fn tay(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.y = self.a;
        self.set_zero_and_negative_flags(self.y);
        0
    }

    pub fn tsx(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.x = self.sp;
        self.set_zero_and_negative_flags(self.x);
        0
    }

    pub fn txa(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.a = self.x;
        self.set_zero_and_negative_flags(self.a);
        0
    }

    pub fn txs(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.sp = self.x;
        0
    }

    pub fn tya(&mut self, _addr_mode: &AddressingMode) -> u8 {
        self.a = self.y;
        self.set_zero_and_negative_flags(self.a);
        0
    }

    pub fn xxx(&mut self, _addr_mode: &AddressingMode) -> u8 {
        unreachable!("Not really but useful for debugging I suppose for now.")
    }
}

impl CpuFlags {
    fn as_u16(&self) -> u16 {
        self.bits() as u16
    }
}

impl BitAnd<CpuFlags> for u8 {
    type Output = CpuFlags;

    fn bitand(self, rhs: CpuFlags) -> CpuFlags {
        CpuFlags::from_bits_truncate(self) & rhs
    }
}

impl BitAnd<CpuFlags> for u16 {
    type Output = CpuFlags;

    fn bitand(self, rhs: CpuFlags) -> CpuFlags {
        CpuFlags::from_bits_truncate(self as u8) & rhs
    }
}

impl From<CpuFlags> for bool {
    fn from(flags: CpuFlags) -> bool {
        flags.bits() != 0
    }
}

#[cfg(test)]
impl Default for Cpu {
    fn default() -> Self {
        Cpu {
            opcode: 0,
            cycles: 0,
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: CpuFlags::empty(),
            bus: Bus::default(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_memory_read_write() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.write_byte(0x0000, 0x01);
        assert_eq!(cpu.read_byte(0x0000), 0x01);
        cpu.write_byte(0x0001, 0x02);
        assert_eq!(cpu.read_byte(0x0001), 0x02);
        cpu.write_byte(0x0002, 0x03);
        assert_eq!(cpu.read_byte(0x0002), 0x03);

        cpu.write_short(0x0003, 0x0405);
        assert_eq!(cpu.read_short(0x0003), 0x0405);
        cpu.write_short(0x0005, 0x0607);
        assert_eq!(cpu.read_short(0x0005), 0x0607);
        cpu.write_short(0x0007, 0x0809);
        assert_eq!(cpu.read_short(0x0007), 0x0809);
    }

    #[test]
    fn test_adc() {
        let mut cpu = Cpu::default();
        cpu.reset();
        for i in 0..10 {
            cpu.write_byte(cpu.program_counter() + i, 0x02)
        }

        for i in 1..=10 {
            assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
            assert_eq!(cpu.a, 0x02 * i);
        }

        cpu.reset();
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xFF;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x60);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.write_byte(cpu.program_counter(), 0x50);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xA0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.write_byte(cpu.program_counter(), 0x90);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xE0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.write_byte(cpu.program_counter(), 0xD0);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x20);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xE0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.write_byte(cpu.program_counter(), 0x50);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x20);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.write_byte(cpu.program_counter(), 0x90);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x60);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.write_byte(cpu.program_counter(), 0xD0);
        assert_eq!(cpu.adc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xA0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_and() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0b10101010;
        cpu.write_byte(cpu.program_counter(), 0b10101010);
        assert_eq!(cpu.and(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0b10101010);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xFF;
        cpu.write_byte(cpu.program_counter(), 0x00);
        assert_eq!(cpu.and(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x7F;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.and(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_asl() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x01;
        assert_eq!(cpu.asl(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x02);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.a = 0x7F;
        assert_eq!(cpu.asl(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0xFE);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.a = 0x80;
        assert_eq!(cpu.asl(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.a = 0xFF;
        assert_eq!(cpu.asl(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0xFE);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));
    }

    #[test]
    fn test_bcc() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bcc(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::C);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bcc(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.remove(CpuFlags::C);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.bcc(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.remove(CpuFlags::C);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.bcc(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::C);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.bcc(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_bcs() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.remove(CpuFlags::C);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bcs(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bcs(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.bcs(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.bcs(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.bcs(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_beq() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.remove(CpuFlags::Z);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.beq(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::Z);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.beq(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.insert(CpuFlags::Z);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.beq(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.insert(CpuFlags::Z);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.beq(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::Z);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.beq(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_bit() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.pc = 0x0000;
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x01);
        assert_eq!(cpu.bit(&AddressingMode::ZeroPage), 0);
        assert!(cpu.status.contains(CpuFlags::Z));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.a = 0x03;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x03);
        assert_eq!(cpu.bit(&AddressingMode::ZeroPage), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x3F);
        assert_eq!(cpu.bit(&AddressingMode::ZeroPage), 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0xBF);
        assert_eq!(cpu.bit(&AddressingMode::ZeroPage), 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x7F);
        assert_eq!(cpu.bit(&AddressingMode::ZeroPage), 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0xFF);
        assert_eq!(cpu.bit(&AddressingMode::ZeroPage), 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.a = 0xC3;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0xC3);
        assert_eq!(cpu.bit(&AddressingMode::ZeroPage), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_bmi() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.remove(CpuFlags::N);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bmi(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::N);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bmi(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.insert(CpuFlags::N);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.bmi(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.insert(CpuFlags::N);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.bmi(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::N);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.bmi(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_bne() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.insert(CpuFlags::Z);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bne(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::Z);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bne(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.remove(CpuFlags::Z);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.bne(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.remove(CpuFlags::Z);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.bne(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::Z);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.bne(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_bpl() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.insert(CpuFlags::N);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bpl(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::N);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bpl(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.remove(CpuFlags::N);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.bpl(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.remove(CpuFlags::N);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.bpl(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::N);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.bpl(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_brk() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.pc = 0x0000;
        cpu.status = CpuFlags::from_bits_truncate(0x00);
        assert_eq!(cpu.brk(&AddressingMode::Implicit), 0);
        let v = cpu.read_short(IRQ_BRK_VECTOR);
        assert_eq!(cpu.pc, v);
        assert_eq!(cpu.status.bits(), CpuFlags::from_bits_truncate(0x00).bits());

        let stack_status = CpuFlags::from_bits_truncate(cpu.pop_stack_byte());
        let stack_pc = cpu.pop_stack_short();
        assert_eq!(stack_pc, 0x0001);
        assert!(stack_status.contains(CpuFlags::B));

        cpu.reset();
        cpu.pc = 0x8102;
        cpu.status = CpuFlags::N | CpuFlags::C;
        assert_eq!(cpu.brk(&AddressingMode::Implicit), 0);
        let v = cpu.read_short(IRQ_BRK_VECTOR);
        assert_eq!(cpu.pc, v);
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        let stack_status = CpuFlags::from_bits_truncate(cpu.pop_stack_byte());
        let stack_pc = cpu.pop_stack_short();
        assert_eq!(stack_pc, 0x8103);
        assert!(stack_status.contains(CpuFlags::B));
        assert!(stack_status.contains(CpuFlags::N));
        assert!(stack_status.contains(CpuFlags::C));
    }

    #[test]
    fn test_bvc() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.insert(CpuFlags::V);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bvc(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::V);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bvc(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.remove(CpuFlags::V);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.bvc(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.remove(CpuFlags::V);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.bvc(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.remove(CpuFlags::V);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.bvc(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_bvs() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status.remove(CpuFlags::V);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bvs(&AddressingMode::Relative), 0);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::V);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x10);
        assert_eq!(cpu.bvs(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0011);

        cpu.reset();
        cpu.status.insert(CpuFlags::V);
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.bvs(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0000);

        cpu.reset();
        cpu.status.insert(CpuFlags::V);
        cpu.pc = 0x0080;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.bvs(&AddressingMode::Relative), 1);
        assert_eq!(cpu.pc, 0x0001);

        cpu.reset();
        cpu.status.insert(CpuFlags::V);
        cpu.pc = 0x00FE;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.bvs(&AddressingMode::Relative), 2);
        assert_eq!(cpu.pc, 0x0100);
    }

    #[test]
    fn test_clc() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.status.insert(CpuFlags::C);
        assert_eq!(cpu.clc(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert_eq!(cpu.status.bits(), status.bits());

        cpu.reset();
        cpu.status.remove(CpuFlags::C);
        let status = cpu.status.clone();
        assert_eq!(cpu.clc(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_cld() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.status.insert(CpuFlags::D);
        assert_eq!(cpu.cld(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::D));
        assert_eq!(cpu.status.bits(), status.bits());

        cpu.reset();
        let status = cpu.status.clone();
        cpu.status.remove(CpuFlags::D);
        assert_eq!(cpu.cld(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::D));
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_cli() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.status.insert(CpuFlags::I);
        assert_eq!(cpu.cli(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::I));
        assert_eq!(cpu.status.bits(), status.bits());

        cpu.reset();
        let status = cpu.status.clone();
        cpu.status.remove(CpuFlags::I);
        assert_eq!(cpu.cli(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::I));
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_clv() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.status.insert(CpuFlags::V);
        assert_eq!(cpu.clv(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::V));
        assert_eq!(cpu.status.bits(), status.bits());

        cpu.reset();
        let status = cpu.status.clone();
        cpu.status.remove(CpuFlags::V);
        assert_eq!(cpu.clv(&AddressingMode::Implicit), 0);
        assert!(!cpu.status.contains(CpuFlags::V));
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_cmp() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cmp(&AddressingMode::Immediate), 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.a = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x02);
        assert_eq!(cpu.cmp(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.a = 0x02;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cmp(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.a = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.cmp(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.a = 0x81;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cmp(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));
    }

    #[test]
    fn test_cpx() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.x = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cpx(&AddressingMode::Immediate), 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.x = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x02);
        assert_eq!(cpu.cpx(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.x = 0x02;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cpx(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.x = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.cpx(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.x = 0x81;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cpx(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));
    }

    #[test]
    fn test_cpy() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.y = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cpy(&AddressingMode::Immediate), 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.y = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x02);
        assert_eq!(cpu.cpy(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.y = 0x02;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cpy(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.y = 0x01;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.cpy(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(!cpu.status.contains(CpuFlags::C));

        cpu.reset();
        cpu.y = 0x81;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.cpy(&AddressingMode::Immediate), 0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
        assert!(cpu.status.contains(CpuFlags::C));
    }

    #[test]
    fn test_dec() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x02);
        assert_eq!(cpu.dec(&AddressingMode::ZeroPage), 0);
        assert_eq!(cpu.read_byte(0x0080), 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x01);
        assert_eq!(cpu.dec(&AddressingMode::ZeroPage), 0);
        assert_eq!(cpu.read_byte(0x0080), 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x00);
        assert_eq!(cpu.dec(&AddressingMode::ZeroPage), 0);
        assert_eq!(cpu.read_byte(0x0080), 0xFF);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_dex() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.x = 0x02;
        assert_eq!(cpu.dex(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x01;
        assert_eq!(cpu.dex(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x00;
        assert_eq!(cpu.dex(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0xFF);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_dey() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.y = 0x02;
        assert_eq!(cpu.dey(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x01;
        assert_eq!(cpu.dey(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x00;
        assert_eq!(cpu.dey(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0xFF);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_eor() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0b10101010;
        cpu.write_byte(cpu.program_counter(), 0b10101010);
        assert_eq!(cpu.eor(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xFF;
        cpu.write_byte(cpu.program_counter(), 0x00);
        assert_eq!(cpu.eor(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xFF);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x7F;
        cpu.write_byte(cpu.program_counter(), 0xFF);
        assert_eq!(cpu.eor(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xFF;
        cpu.write_byte(cpu.program_counter(), 0x7F);
        assert_eq!(cpu.eor(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_inc() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x00);
        assert_eq!(cpu.inc(&AddressingMode::ZeroPage), 0);
        assert_eq!(cpu.read_byte(0x0080), 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0xFF);
        assert_eq!(cpu.inc(&AddressingMode::ZeroPage), 0);
        assert_eq!(cpu.read_byte(0x0080), 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_byte(cpu.program_counter(), 0x80);
        cpu.write_byte(0x0080, 0x7F);
        assert_eq!(cpu.inc(&AddressingMode::ZeroPage), 0);
        assert_eq!(cpu.read_byte(0x0080), 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_inx() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.x = 0x00;
        assert_eq!(cpu.inx(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0xFF;
        assert_eq!(cpu.inx(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x7F;
        assert_eq!(cpu.inx(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_iny() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.y = 0x00;
        assert_eq!(cpu.iny(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0xFF;
        assert_eq!(cpu.iny(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x7F;
        assert_eq!(cpu.iny(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_jmp() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_short(cpu.program_counter(), 0x8000);
        assert_eq!(cpu.jmp(&AddressingMode::Absolute), 0);
        assert_eq!(cpu.pc, 0x8000);
    }

    #[test]
    fn test_jsr() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.pc = 0x0000;
        cpu.write_short(cpu.program_counter(), 0x8000);
        assert_eq!(cpu.jsr(&AddressingMode::Absolute), 0);
        assert_eq!(cpu.pc, 0x8000);
        // the stack contains the program counter minus one, this being the return point
        // minus one because the program counter is incremented every clock cycle to read the next instruction
        assert_eq!(cpu.pop_stack_short(), 0x0001);
    }

    #[test]
    fn test_lda() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.lda(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.lda(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x7F);
        assert_eq!(cpu.lda(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x00);
        assert_eq!(cpu.lda(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_ldx() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.x = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.ldx(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.x, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.ldx(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.x, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x7F);
        assert_eq!(cpu.ldx(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.x, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x00);
        assert_eq!(cpu.ldx(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.x, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_ldy() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.y = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.ldy(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.y, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x80);
        assert_eq!(cpu.ldy(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.y, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x7F);
        assert_eq!(cpu.ldy(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.y, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x00;
        cpu.write_byte(cpu.program_counter(), 0x00);
        assert_eq!(cpu.ldy(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.y, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_lsr() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x03;
        assert_eq!(cpu.lsr(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x01;
        assert_eq!(cpu.lsr(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x02;
        assert_eq!(cpu.lsr(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_nop() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        assert_eq!(cpu.nop(&AddressingMode::Implicit), 0);
        assert_eq!(status.bits(), cpu.status.bits());
    }

    #[test]
    fn test_ora() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0b10101010;
        cpu.write_byte(cpu.program_counter(), 0b10101010);
        assert_eq!(cpu.ora(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0b10101010);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0b10101010;
        cpu.write_byte(cpu.program_counter(), 0b01010101);
        assert_eq!(cpu.ora(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0b11111111);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0b00000000;
        cpu.write_byte(cpu.program_counter(), 0b00000000);
        assert_eq!(cpu.ora(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0b00000000);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_pha() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x00;
        let status = cpu.status.clone();
        assert_eq!(cpu.pha(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.pop_stack_byte(), 0x00);
        assert_eq!(status.bits(), cpu.status.bits());

        cpu.reset();
        cpu.a = 0x01;
        let status = cpu.status.clone();
        assert_eq!(cpu.pha(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.pop_stack_byte(), 0x01);
        assert_eq!(status.bits(), cpu.status.bits());

        cpu.reset();
        cpu.a = 0xFF;
        let status = cpu.status.clone();
        assert_eq!(cpu.pha(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.pop_stack_byte(), 0xFF);
        assert_eq!(status.bits(), cpu.status.bits());
    }

    #[test]
    fn test_php() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.status = CpuFlags::from_bits_truncate(0x00);
        assert_eq!(cpu.php(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.pop_stack_byte(), 0x30);

        cpu.reset();
        cpu.status = CpuFlags::from_bits_truncate(0xFF);
        assert_eq!(cpu.php(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.pop_stack_byte(), 0xFF);
    }

    #[test]
    fn test_pla() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.push_stack_byte(0x00);
        assert_eq!(cpu.pla(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.push_stack_byte(0x80);
        assert_eq!(cpu.pla(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.push_stack_byte(0x7F);
        assert_eq!(cpu.pla(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_plp() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.push_stack_byte(0x00);
        assert_eq!(cpu.plp(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.status.bits(), 0x00);

        cpu.reset();
        cpu.push_stack_byte(0xFF);
        assert_eq!(cpu.plp(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.status.bits(), 0xFF);

        cpu.reset();
        cpu.push_stack_byte(0x30);
        assert_eq!(cpu.plp(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.status.bits(), 0x30);
    }

    #[test]
    fn test_rol() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x01;
        assert_eq!(cpu.rol(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x02);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x90;
        assert_eq!(cpu.rol(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x20);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x80;
        assert_eq!(cpu.rol(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x40;
        assert_eq!(cpu.rol(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.a = 0x40;
        assert_eq!(cpu.rol(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x81);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        assert_eq!(cpu.rol(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xC0;
        assert_eq!(cpu.rol(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_ror() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x02;
        assert_eq!(cpu.ror(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x03;
        assert_eq!(cpu.ror(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x01;
        assert_eq!(cpu.ror(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.a = 0x00;
        assert_eq!(cpu.ror(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        assert_eq!(cpu.ror(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.status.insert(CpuFlags::C);
        cpu.a = 0x01;
        assert_eq!(cpu.ror(&AddressingMode::Accumulator), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_rti() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.push_stack_byte(CpuFlags::C.bits());
        cpu.push_stack_byte(0x80);
        cpu.push_stack_byte(0x01);
        assert_eq!(cpu.rti(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.status.bits(), CpuFlags::C.bits());
        assert_eq!(cpu.pc, 0x0180);
    }

    #[test]
    fn test_rts() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.push_stack_short(0x0180);
        assert_eq!(cpu.rts(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.pc, 0x0181);
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_sbc() {
        let mut cpu = Cpu::default();
        cpu.reset();
        for i in 0..10 {
            cpu.write_byte(cpu.program_counter() + i, 0x02)
        }

        cpu.a = 0x14;
        for i in (0..10).rev() {
            cpu.status.insert(CpuFlags::C);
            assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
            assert_eq!(cpu.a, 0x02 * i);
        }

        cpu.reset();
        cpu.a = 0x01;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0x01);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xFF);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0xF0);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x60);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0xB0);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xA0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0x70);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xE0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x50;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0x30);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x20);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0xF0);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xE0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0xB0);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x20);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0x70);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0x60);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(cpu.status.contains(CpuFlags::V));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0xD0;
        cpu.status.insert(CpuFlags::C);
        cpu.write_byte(cpu.program_counter(), 0x30);
        assert_eq!(cpu.sbc(&AddressingMode::Immediate), 0);
        assert_eq!(cpu.a, 0xA0);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::C));
        assert!(!cpu.status.contains(CpuFlags::V));
        assert!(cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_sec() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        assert_eq!(cpu.sec(&AddressingMode::Implicit), 0);
        assert!(cpu.status.contains(CpuFlags::C));
        assert_eq!(cpu.status.bits(), (status | CpuFlags::C).bits());
    }

    #[test]
    fn test_sed() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        assert_eq!(cpu.sed(&AddressingMode::Implicit), 0);
        assert!(cpu.status.contains(CpuFlags::D));
        assert_eq!(cpu.status.bits(), (status | CpuFlags::D).bits());
    }

    #[test]
    fn test_sei() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        assert_eq!(cpu.sei(&AddressingMode::Implicit), 0);
        assert!(cpu.status.contains(CpuFlags::I));
        assert_eq!(cpu.status.bits(), (status | CpuFlags::I).bits());
    }

    #[test]
    fn test_sta() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.a = 0xFF;
        cpu.write_short(cpu.program_counter(), 0x0100);
        assert_eq!(cpu.sta(&AddressingMode::Absolute), 0);
        assert_eq!(cpu.read_byte(0x0100), 0xFF);
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_stx() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.x = 0xFF;
        cpu.write_short(cpu.program_counter(), 0x0100);
        assert_eq!(cpu.stx(&AddressingMode::Absolute), 0);
        assert_eq!(cpu.read_byte(0x0100), 0xFF);
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_sty() {
        let mut cpu = Cpu::default();
        cpu.reset();
        let status = cpu.status.clone();
        cpu.y = 0xFF;
        cpu.write_short(cpu.program_counter(), 0x0100);
        assert_eq!(cpu.sty(&AddressingMode::Absolute), 0);
        assert_eq!(cpu.read_byte(0x0100), 0xFF);
        assert_eq!(cpu.status.bits(), status.bits());
    }

    #[test]
    fn test_tax() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x01;
        assert_eq!(cpu.tax(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        assert_eq!(cpu.tax(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x80;
        assert_eq!(cpu.tax(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x7F;
        assert_eq!(cpu.tax(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_tay() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.a = 0x01;
        assert_eq!(cpu.tay(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x00;
        assert_eq!(cpu.tay(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x80;
        assert_eq!(cpu.tay(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.a = 0x7F;
        assert_eq!(cpu.tay(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.y, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_tsx() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.sp = 0x01;
        assert_eq!(cpu.tsx(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.sp = 0x00;
        assert_eq!(cpu.tsx(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.sp = 0x80;
        assert_eq!(cpu.tsx(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.sp = 0x7F;
        assert_eq!(cpu.tsx(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.x, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_txa() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.x = 0x01;
        assert_eq!(cpu.txa(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x00;
        assert_eq!(cpu.txa(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x80;
        assert_eq!(cpu.txa(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.x = 0x7F;
        assert_eq!(cpu.txa(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }

    #[test]
    fn test_txs() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.x = 0x01;
        let status = cpu.status.clone();
        assert_eq!(cpu.txs(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.sp, 0x01);
        assert_eq!(status.bits(), cpu.status.bits());

        cpu.reset();
        cpu.x = 0x00;
        let status = cpu.status.clone();
        assert_eq!(cpu.txs(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.sp, 0x00);
        assert_eq!(status.bits(), cpu.status.bits());

        cpu.reset();
        cpu.x = 0x80;
        let status = cpu.status.clone();
        assert_eq!(cpu.txs(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.sp, 0x80);
        assert_eq!(status.bits(), cpu.status.bits());

        cpu.reset();
        cpu.x = 0x7F;
        let status = cpu.status.clone();
        assert_eq!(cpu.txs(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.sp, 0x7F);
        assert_eq!(status.bits(), cpu.status.bits());
    }

    #[test]
    fn test_tya() {
        let mut cpu = Cpu::default();
        cpu.reset();
        cpu.y = 0x01;
        assert_eq!(cpu.tya(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x01);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x00;
        assert_eq!(cpu.tya(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x00);
        assert!(cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x80;
        assert_eq!(cpu.tya(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x80);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(cpu.status.contains(CpuFlags::N));

        cpu.reset();
        cpu.y = 0x7F;
        assert_eq!(cpu.tya(&AddressingMode::Implicit), 0);
        assert_eq!(cpu.a, 0x7F);
        assert!(!cpu.status.contains(CpuFlags::Z));
        assert!(!cpu.status.contains(CpuFlags::N));
    }
}
