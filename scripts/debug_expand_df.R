devtools::load_all()


# read in data



df <- read.csv("vignettes/data_examples/my_data.csv")

# create codebook

structural_metadata <- create_structural_metadata(df,primary_key = "key",
                                       foreign_key = c("measured_by","site_name"))

# create data package


dp <- frictionless::create_package() |>
      frictionless::add_resource(resource_name = "my_data",
                                 data = df)

frictionless::write_package(package = dp,
                            directory = "vignettes/data_examples")

expand_frictionless_metadata(structural_metadata = structural_metadata,
                             resource_name = "my_data",
                             resource_path = "vignettes/data_examples/my_data.csv",
                             data_package_path = "vignettes/data_examples/datapackage.json"
                            )

## drop a measured_by column

df <- read.csv("vignettes/data_examples/my_data.csv")

# create codebook

structural_metadata_2 <- create_structural_metadata(df,
                                                  primary_key = "key",
                                                  foreign_key = c("site_name"))


expand_frictionless_metadata(structural_metadata = structural_metadata_2,
                             resource_name = "my_data",
                             resource_path = "vignettes/data_examples/my_data.csv",
                             data_package_path = "vignettes/data_examples/datapackage.json"
)
