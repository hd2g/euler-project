use std::env;

fn main() -> Result<(), String> {
  let x: u32 = env::args().nth(1).unwrap().parse().unwrap();
  println!("{}", solve(x));

  Ok(())
}

fn solve(x: u32) -> u32 {
  (1..=x).filter(|x| { x % 3 == 0 || x % 5 == 0 }).sum()
}

