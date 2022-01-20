use criterion::{black_box, criterion_group, criterion_main, Criterion};
use serde_pickle;
use std::io::Read;

fn bench_picklefile(c: &mut Criterion, filename: &str) {
    // Load the picklefile
    let mut contents = vec![];
    let mut f = std::fs::File::open(filename).unwrap();
    f.read_to_end(&mut contents).unwrap();

    // Run the benchmark
    c.bench_function(filename, |b| {
        b.iter(|| {
            serde_pickle::de::value_from_slice(
                black_box(&contents),
                serde_pickle::de::DeOptions::new(),
            )
            .unwrap()
        })
    });
}

pub fn criterion_benchmark(c: &mut Criterion) {
    bench_picklefile(c, "benches/data/biglist.pickle");
    bench_picklefile(c, "benches/data/manyrefs.pickle");
    bench_picklefile(c, "benches/data/manystrings.pickle");

    for i in 0..=5 {
        bench_picklefile(c, &format!("test/data/tests_py3_proto{}.pickle", i));
    }
    for i in 0..=2 {
        bench_picklefile(c, &format!("test/data/tests_py2_proto{}.pickle", i));
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
