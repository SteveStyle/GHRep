enum SnafuDigit {
    DoubleMinus = -2,
    Minus = -1,
    Zero = 0,
    One = 1,
    Two = 2,
}

impl SnafuDigit {
    fn from_char(c: char) -> Option<SnafuDigit> {
        match c {
            '=' => Some(SnafuDigit::DoubleMinus),
            '-' => Some(SnafuDigit::Minus),
            '0' => Some(SnafuDigit::Zero),
            '1' => Some(SnafuDigit::One),
            '2' => Some(SnafuDigit::Two),
            _ => None,
        }
    }
    fn to_char(&self) -> char {
        match self {
            SnafuDigit::DoubleMinus => '=',
            SnafuDigit::Minus => '-',
            SnafuDigit::Zero => '0',
            SnafuDigit::One => '1',
            SnafuDigit::Two => '2',
        }
    }
    fn from_value(v: isize) -> Option<SnafuDigit> {
        match v {
            -2 => Some(SnafuDigit::DoubleMinus),
            -1 => Some(SnafuDigit::Minus),
            0 => Some(SnafuDigit::Zero),
            1 => Some(SnafuDigit::One),
            2 => Some(SnafuDigit::Two),
            _ => None,
        }
    }
    fn to_value(&self) -> isize {
        match self {
            SnafuDigit::DoubleMinus => -2,
            SnafuDigit::Minus => -1,
            SnafuDigit::Zero => 0,
            SnafuDigit::One => 1,
            SnafuDigit::Two => 2,
        }
    }
}

pub struct Snafu {
    value: isize,
}

impl Snafu {
    pub fn new() -> Snafu {
        Snafu { value: 0 }
    }
    pub fn from_string(s: &str) -> Snafu {
        let mut result = Snafu::new();
        for c in s.chars() {
            let digit = SnafuDigit::from_char(c).unwrap();
            result.value = result.value * 5 + digit.to_value();
        }
        result
    }
    pub fn to_string(&self) -> String {
        if self.value == 0 {
            return String::from("");
        } else {
            let digit_value = ((self.value +2) % 5) - 2;
            let mut result = Snafu::from_value((self.value - digit_value) / 5).to_string();
            result.push(SnafuDigit::from_value(digit_value).unwrap().to_char());
            return result;
        }        
    }
    pub fn to_value(&self) -> isize {
        self.value
    }
    pub fn from_value(v: isize) -> Snafu {
        Snafu { value: v }
    }
}

pub fn snafu_to_value(s: &str) -> isize {
    let mut result = 0;
    for c in s.chars() {
        let digit = match c {
            '=' => -2,
            '-' => -1,
            '0' => 0,
            '1' => 1,
            '2' => 2,
            _ => panic!("Invalid character in snafu string"),
        };
        result = result * 5 + digit;
    }
    result
}

pub fn value_to_snafu(v: isize) -> String {
    if v == 0 {
        return String::from("");
    } else {
        let digit_value = ((v +2) % 5) - 2;
        let mut result = value_to_snafu((v - digit_value) / 5);
        result.push(match digit_value {
            -2 => '=',
            -1 => '-',
            0 => '0',
            1 => '1',
            2 => '2',
            _ => panic!("Invalid digit value"),
        });
        return result;
    }
}