use std::{collections::*, ffi::OsString, num::NonZeroU128, str::FromStr};
use std::{hint::black_box, io::stdin, num::NonZeroU8, ptr::NonNull};
#[derive(Debug)]
pub enum Number {
    One = 57,
    Two = 99,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Container {
    First(u32),
    Second { val: u64, val2: i8 },
    Third,
}

pub struct Point {
    x: f32,
    y: f32,
}

impl Point {
    fn increase(&mut self, x: f32, y: f32) {
        self.x += x;
        self.y += y;
    }
}

pub union Reg {
    rax: u64,
    eax: u32,
}

fn func(val: u8) -> u8 {
    dbg!(val)
}

const EEF: u8 = 0;

enum MyEnum {
    A,
    B { x: i32, y: f32 },
    C(Vec<i32>),
}

struct TupleStruct(u8, f64);

const GLOBAL_VAR: u8 = 17;
static mut GLOBAL_MUT: u8 = 17;

fn main() {
    let val1 = 1u8;
    let mut val2 = 100i32;
    let ref_val = &val1;
    let ref_val2 = &val2;
    dbg!(ref_val as *const _ as usize);
    dbg!(ref_val2 as *const _ as usize);
    let tuple = (val1, val2);
    let tuple2 = (val1, ref_val2);
    let tuple3 = (ref_val, val2);
    let tuple4 = (ref_val, ref_val2);
    let tuple5 = (val1, ref_val2 as *const _);

    let signed = -1i8;
    let chr = 'c';
    let ref_val = &val1;
    let mut_val = &mut val2;
    let ref_ref = &&val1;
    let mut_mut = &mut &mut val2;
    let ref_mut = & &mut val2;
    let mut_ref = &mut &val2;

    let ref_ref_ref = &&&val1;
    let ref_ref_ref_ref = &&&&val1;
    let t = *ref_ref_ref_ref;
    let mut_mut_mut = &mut &mut &mut val2;
    let mut_mut_mut_mut = &mut &mut &mut &mut val2;

    let ref_mut_mut = &&mut &mut val2;
    let mut_ref_ref = &mut &&val2;
    let ref_mut_ref = & &mut &val2;
    let mut_ref_mut = &mut &&mut val2;

    let box_ptr = Box::new(0u8);
    let const_ptr = &val1 as *const _;
    let mut_ptr = &mut val2 as *mut _;
    let ptr_ptr = (&const_ptr) as *const _;
    let ptr_ref = &&val1 as *const &u8;
    let ref_ptr = &const_ptr;
    let ref_to_mut_ptr = &mut_ptr;
    let ptr_ref_ref = &ref_ref as *const _;

    // let u8_val = b'a';
    // let i8_val = -1i8;
    // let usize_val = 100usize;
    let float = 42.78000000000001;
    // let float2: f32 = 31.0;

    let str_val = "eef";
    let mut string = "freef".to_owned();
    let mut_str = string.as_mut_str();
    let string_val = "eef".to_owned();
    let empty_string = "".to_string();
    let k = "IAMA string!".to_string();

    // let tuple = (u8_val, usize_val);
    let array: [u32; 7] = [1, 2, 3, 4, 255, 254, 243];
    let mut array2: [u32; 5] = [1, 2, 3, 4, u32::MAX];
    let mut_array = array2.as_mut_slice();
    let vec_val = [&Number::One, &Number::Two, &Number::One].to_vec();
    let mut vec_array = vec![array.as_slice(), array2.as_slice()];
    let slice_val = array.as_slice();
    // let slice_val_mut: &mut [i32] = vec_val.as_mut_slice();

    let enum_val = Number::One;
    let mut enum_val2 = Number::Two;
    let sum_val = Container::First(15);
    let sum_val_2 = Container::Second { val: 1000, val2: 10 };
    let sum_val_3 = Container::Third;
    // let box_val = Box::new(enum_val);

    let non_zero = NonZeroU128::new(100).unwrap();
    let large_discr = NonZeroU128::new(255);
    let os_string = OsString::from_str("sbubby").unwrap();

    let mut struct_val = Point { x: 12.3, y:32.1 };
    struct_val.increase(1.0, 1.0);
    let union_val = Reg { rax: 20 };
    let thing = [0, 1, 2, 3].to_vec();

    let range = 0..159;
    let range_incl = 200..=300;

    let tuple_struct = TupleStruct(95, 64.0);

    let ret_val = do_thing(enum_val);


    let asdlkfj = 3000;
    let mut map = HashMap::new();
    map.insert("eef", (30.0f64, &asdlkfj));
    // map.insert("freef", 200);

    let ref_map = &map;

    let mut set = HashSet::new();
    set.insert("asdf");
    set.insert("jkl;");

    let mut_set = &mut set;

    dbg!(GLOBAL_VAR);
    // let mut buf = String::new();

    // stdin().read_line(&mut buf).unwrap();
}

// pub fn eef(thing: &[u8]) {
//     print!("{thing:?}");
// }

pub fn do_thing(num: Number) -> u8 {
    match num {
        Number::One => 1,
        Number::Two => 2,
    }
}