#[derive(Debug)]
pub struct Rom {
    pub header: RomHeader,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub title: String,
}

impl Rom {
    pub fn from_bytes(mut bytes: impl Iterator<Item=u8>)
        -> Result<Self, RomError>
    {
        let mut bytes = &mut bytes;

        let header = RomHeader::from_bytes(&mut bytes)?;
        match header {
            RomHeader {
                prg_rom_size_bytes: _,
                chr_rom_size_bytes: _,
                mirror_mode: _,
                mapper: _,
                prg_ram_size_bytes: 0,
                has_persistence: false,
                has_trainer: false,
                is_vs_unisystem: false,
                is_playchoice_10: false,
                tv_system: TvSystem::Ntsc,
                has_bus_conflicts: false
            } => { }
            header => {
                unimplemented!("ROM with header: {:#?}", header);
            }
        }

        let prg_rom: Vec<_> = bytes.take(header.prg_rom_size_bytes).collect();
        if prg_rom.len() != header.prg_rom_size_bytes {
            return Err(RomError::UnexpectedEof);
        }

        let chr_rom: Vec<_> = bytes.take(header.chr_rom_size_bytes).collect();
        if chr_rom.len() != header.chr_rom_size_bytes {
            return Err(RomError::UnexpectedEof);
        }

        let title: Vec<_> = bytes.take(128).collect();
        let title = String::from_utf8_lossy(&title).into_owned();

        let eof = bytes.next();
        if eof != None {
            return Err(RomError::ExpectedEof);
        }

        Ok(Rom {
            header,
            prg_rom,
            chr_rom,
            title,
        })
    }
}

#[derive(Debug)]
pub enum RomError {
    InvalidHeader,
    UnexpectedEof,
    ExpectedEof,
}

#[derive(Debug)]
pub struct RomHeader {
    prg_rom_size_bytes: usize,
    chr_rom_size_bytes: usize,
    prg_ram_size_bytes: usize,
    pub mirror_mode: MirrorMode,
    pub has_persistence: bool,
    has_trainer: bool,
    pub mapper: u8,
    pub is_vs_unisystem: bool,
    pub is_playchoice_10: bool,
    pub tv_system: TvSystem,
    pub has_bus_conflicts: bool,
}

impl RomHeader {
    fn from_bytes(mut bytes: impl Iterator<Item=u8>)
        -> Result<Self, RomError>
    {
        let expected_magic_str = b"NES\x1A".iter().cloned();
        let actual_magic_str = (&mut bytes).take(4);
        if expected_magic_str.ne(actual_magic_str) {
            return Err(RomError::InvalidHeader);
        }

        let prg_rom_size_field = bytes.next()
            .ok_or_else(|| RomError::InvalidHeader)?;
        let chr_rom_size_field = bytes.next()
            .ok_or_else(|| RomError::InvalidHeader)?;
        let flags_6 = bytes.next()
            .ok_or_else(|| RomError::InvalidHeader)?;
        let flags_7 = bytes.next()
            .ok_or_else(|| RomError::InvalidHeader)?;
        let prg_ram_size_field = bytes.next()
            .ok_or_else(|| RomError::InvalidHeader)?;
        let flags_9 = bytes.next()
            .ok_or_else(|| RomError::InvalidHeader)?;
        let flags_10 = bytes.next()
            .ok_or_else(|| RomError::InvalidHeader)?;

        let expected_zeros = [0, 0, 0, 0, 0].iter().cloned();
        let actual_zeros = (&mut bytes).take(5);
        if expected_zeros.ne(actual_zeros) {
            return Err(RomError::InvalidHeader)?;
        }



        let flag_mirror_bit           =  flags_6 & 0b_0000_0001 != 0;
        let flag_persistent_bit       =  flags_6 & 0b_0000_0010 != 0;
        let flag_trainer_bit          =  flags_6 & 0b_0000_0100 != 0;
        let flag_four_screen_vram_bit =  flags_6 & 0b_0000_1000 != 0;
        let flag_mapper_lo            = (flags_6 & 0b_1111_0000) >> 4;

        let flag_vs_unisystem         =  flags_7 & 0b_0000_0001 != 0;
        let flag_playchoice_10        =  flags_7 & 0b_0000_0010 != 0;
        let flag_rom_format           = (flags_7 & 0b_0000_1100) >> 2;
        let flag_mapper_hi            = (flags_7 & 0b_1111_0000) >> 4;

        let _flag_tv_system_ignored   =  flags_9 & 0b_0000_0001;
        let _flag_reserved            =  flags_9 & 0b_1111_1110;

        let flag_tv_system            =  flags_10 & 0b_0000_0011;
        let _flag_unused              =  flags_10 & 0b_1100_1100;
        let flag_prg_ram_bit          = (flags_10 & 0b_0001_0000) != 0;
        let flag_bus_conflicts_bit    = (flags_10 & 0b_0010_0000) != 0;



        let prg_rom_size_bytes = prg_rom_size_field as usize * 16_384;

        let chr_rom_size_bytes =
            if chr_rom_size_field > 0 {
                chr_rom_size_field as usize * 8_192
            }
            else {
                unimplemented!("ROM uses CHR RAM!");
            };

        let prg_ram_size_bytes =
            match (prg_ram_size_field, flag_prg_ram_bit) {
                (_, false) => { 0 },
                (0, true) => {
                    // When a ROM has the PRG RAM bit set but has a PRG RAM
                    // size of 0, then a fallback size of 8KB is used
                    8_192
                },
                (prg_ram_size_field, true) => {
                    prg_ram_size_field as usize * 8_192
                }
            };

        let mirror_mode = match (flag_mirror_bit, flag_four_screen_vram_bit) {
            (false, false) => MirrorMode::Horizontal,
            (true, false) => MirrorMode::Vertical,
            (_, true) => MirrorMode::FourScreenVram
        };
        let has_persistence = flag_persistent_bit;
        let has_trainer = flag_trainer_bit;
        let mapper = flag_mapper_lo & (flag_mapper_hi << 4);

        let is_vs_unisystem = flag_vs_unisystem;
        let is_playchoice_10 = flag_playchoice_10;
        match flag_rom_format {
            2 => { unimplemented!("ROM uses NES 2.0 ROM format!"); }
            _ => { }
        };

        let tv_system = match flag_tv_system {
            0 => { TvSystem::Ntsc },
            2 => { TvSystem::Pal },
            1 | 3 => { TvSystem::Dual },
            _ => { unreachable!(""); }
        };

        let has_bus_conflicts = flag_bus_conflicts_bit;

        Ok(RomHeader {
            prg_rom_size_bytes,
            chr_rom_size_bytes,
            prg_ram_size_bytes,
            mirror_mode,
            has_persistence,
            has_trainer,
            mapper,
            is_vs_unisystem,
            is_playchoice_10,
            tv_system,
            has_bus_conflicts,
        })
    }
}

#[derive(Debug)]
pub enum MirrorMode {
    Horizontal,
    Vertical,
    FourScreenVram
}

#[derive(Debug)]
pub enum TvSystem {
    Ntsc,
    Pal,
    Dual
}
