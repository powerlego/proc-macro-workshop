// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::{bitfield, Basic, SetGet};

pub trait Specifier {
    const BITS: usize;

    type T: Sized;

    fn set<const ACC: usize, const SIZE: usize>(arr: &mut [u8], num: <Self as Specifier>::T);
    fn get<const ACC: usize, const SIZE: usize>(arr: &[u8]) -> <Self as Specifier>::T;
}

const ERR: &str = "Failed to convert slice to array";
type Range = std::ops::RangeInclusive<usize>;

pub trait Basic<const WIDTH: usize, const ACC: usize> {
    const OFFSET: usize = ACC % 8;
    const OFFSET_END: usize = WIDTH - Self::OFFSET_END_;
    const OFFSET_END_: usize = (ACC + WIDTH) % 8;
    const RANGE: Range = Self::RANGE_LHS..=Self::RANGE_RHS;
    const RANGE_BITS: u32 = (Self::RANGE_LEN * 8) as u32;
    const RANGE_LEN: usize = Self::RANGE_RHS - Self::RANGE_LHS + 1;
    const RANGE_LHS: usize = ACC / 8;
    const RANGE_RHS: usize = (WIDTH + ACC - 1) / 8;
}

pub trait SetGet {
    type Target;
    const ACROSS: bool;
    const GET: fn(&[u8]) -> Self::Target;
    const LIMIT: Self::Target;
    const RANGE_ALT: Range;
    const RANGE_RHS2: Range;
    const RANGE_ACROSS: Range;
    const SET: fn(&mut [u8], Self::Target);
    const U8_MAX_OFFSET: u8;
    fn set_across(arr: &mut [u8], num: Self::Target);
    fn set_no_across(arr: &mut [u8], num: Self::Target);
    fn get_across(arr: &[u8]) -> Self::Target;
    fn get_no_across(arr: &[u8]) -> Self::Target;
    fn across_end(arr: &mut [u8], num_end: u8) {
        let p = &mut arr[Self::RANGE_RHS2];
        let num_old = u8::from_ne_bytes(p.try_into().expect(ERR));
        let num_new = num_old & Self::U8_MAX_OFFSET | num_end;
        p.copy_from_slice(&num_new.to_ne_bytes());
    }
    fn get_across_helper<'a, T: TryFrom<&'a [u8]>>(arr: &'a [u8]) -> (T, u8)
    where
        <T as TryFrom<&'a [u8]>>::Error: std::fmt::Debug,
    {
        (
            T::try_from(&arr[Self::RANGE_ACROSS]).expect(ERR),
            u8::from_ne_bytes(arr[Self::RANGE_RHS2].try_into().expect(ERR)),
        )
    }
}

bitfield_impl::gen! {}

#[derive(Basic, SetGet)]
pub struct BitsU8<const WIDTH: usize, const ACC: usize, const SIZE: usize>;

#[derive(Basic, SetGet)]
pub struct BitsU16<const WIDTH: usize, const ACC: usize, const SIZE: usize>;

#[derive(Basic, SetGet)]
#[set_get(
    range_alt = "if Self::RANGE_LEN == 4 { Self::RANGE } else { Self::RANGE_LHS..=Self::RANGE_LHS + 3 }",
    set_no_across = "{Self::SET2(arr, num)}",
    get_no_across = "{ let num = Self::U32(arr); (num & Self::LIMIT) >> Self::OFFSET }"
)]
pub struct BitsU32<const WIDTH: usize, const ACC: usize, const SIZE: usize>;
impl<const WIDTH: usize, const ACC: usize, const SIZE: usize> BitsU32<WIDTH, ACC, SIZE> {
    const HOLD: bool = !(Self::HOLD_LEN < 4);
    const HOLD_LEN: usize = SIZE - Self::RANGE_LHS;
    const RANGE_SIZED: Range = Self::RANGE_LHS..=SIZE - 1;
    const SET2: fn(&mut [u8], u32) = if Self::HOLD { Self::can_hold } else { Self::not_hold };
    const U32: fn(&[u8]) -> u32 = if Self::HOLD {
        Self::u32_can_hold
    }
    else {
        Self::u32_not_hold
    };

    fn can_hold(arr: &mut [u8], num: u32) {
        let num_old = Self::u32_can_hold(arr);
        let p = &mut arr[Self::RANGE_ALT];
        let num_new = num_old & !Self::LIMIT | (num << Self::OFFSET);
        p.copy_from_slice(&num_new.to_ne_bytes());
    }

    fn not_hold(arr: &mut [u8], num: u32) {
        let num_old = Self::u32_not_hold(arr);
        let p = &mut arr[Self::RANGE_SIZED];
        let num_new = num_old & !Self::LIMIT | (num << Self::OFFSET);
        p.copy_from_slice(&num_new.to_le_bytes()[..Self::HOLD_LEN]);
    }

    fn u32_can_hold(arr: &[u8]) -> u32 {
        u32::from_ne_bytes(arr[Self::RANGE_ALT].try_into().expect(ERR))
    }

    fn u32_not_hold(arr: &[u8]) -> u32 {
        assert_eq!(Self::HOLD_LEN, arr[Self::RANGE_LHS..].len());
        let mut tmp = [0; 4];
        tmp[..Self::HOLD_LEN].copy_from_slice(&arr[Self::RANGE_SIZED]);
        u32::from_le_bytes(tmp)
    }
}

#[derive(Basic, SetGet)]
// #[range_alt = "if Self::RANGE_LEN == 8 { Self::RANGE } else { Self::RANGE_LHS..=Self::RANGE_LHS + 7 }"]
pub struct BitsU64<const WIDTH: usize, const ACC: usize, const SIZE: usize>;
