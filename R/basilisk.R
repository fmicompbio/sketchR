#' @importFrom basilisk BasiliskEnvironment
geosketchenv <- BasiliskEnvironment(
    envname = "geosketch", pkgname = "sketchR",
    packages = c("joblib==1.1.0", "numpy==1.21.5",
                 "scikit-learn==1.0.2", "scipy==1.7.3", "threadpoolctl==3.0.0"),
    channels = c("conda-forge"),
    pip = c("fbpca==1.0", "geosketch==1.2")
)

scsamplerenv <- BasiliskEnvironment(
    envname = "scsampler", pkgname = "sketchR",
    packages = c("numpy==1.21.5", "pandas==1.3.5", "scanpy==1.7.2"),
    channels = c("bioconda", "conda-forge"),
    pip = c("scsampler==1.0.0")
)

