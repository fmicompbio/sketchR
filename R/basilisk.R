#' @importFrom basilisk BasiliskEnvironment
geosketchenv <- BasiliskEnvironment(
    envname = "env", pkgname = "geosketchR",
    packages = c("joblib==1.1.0", "numpy==1.21.5",
                 "scikit-learn==1.0.2", "scipy==1.7.3", "threadpoolctl==3.0.0"),
    channels = c("conda-forge"),
    pip = c("fbpca==1.0", "geosketch==1.2")
)

