pub fn is_leap_year(year: u64) -> bool {
  if year % 400 == 0 { return true; }
  if year % 200 == 0 { return false; }
  if year % 4 == 0 { return true; }
  return false;
}
