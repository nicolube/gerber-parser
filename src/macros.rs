/// Builds a hashmap of enum variants to their values.
macro_rules! build_enum_map {
    ($name: ident, $t:ident ) => {
        static $name: LazyLock<HashMap<String, $t>> = LazyLock::new(|| {
            use strum::VariantArray;
            use strum::VariantNames;

            let values = <$t as VariantArray>::VARIANTS;
            let names = <$t as VariantNames>::VARIANTS;

            HashMap::from_iter(
                names
                    .iter()
                    .zip(values.iter())
                    .map(|(name, &value)| (name.to_lowercase(), value))
                    .collect::<Vec<(String, _)>>(),
            )
        });
    };
}
