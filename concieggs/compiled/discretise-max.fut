-- Summarises an n-element input in k bins, with each of the k bins
-- containing the maximum of the input in that span.
let main [n] (k: i64) (samples: [n]i32) =
  let bin_size = f32.i64 n / f32.i64 k
  let index i = i64.f32 (f32.i64 i / bin_size)
  in reduce_by_index (replicate k 0) i32.max i32.highest
                     (map index (iota n)) samples
