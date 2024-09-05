# ohcleandat 0.3.8

# ohcleandat 0.3.7

* Fixing bug in expand metadata - function now allows for updates

# ohcleandat 0.3.6

* Adds a function to update the descriptive metadata in a frictionless datapackage

# ohcleandat 0.3.5

# ohcleandat 0.3.4

* Setups up a minimal structural metadata framework for tabular datasets. 

# ohcleandat 0.3.3

* Adds more control over the function used in `get_precision` and `obfuscate_gps`

# ohcleandat 0.3.2

* Fixing issue with `download_google_drive` where search pattern is over applied.

# ohcleandat 0.3.1

* Explicitly adding `set_diff` function which was previously a hidden dependency on the {ecohealthalliance/airtabler} package

# ohcleandat 0.3.0

* Adding GPS obfuscation function - this function uses two methods to reduce the
accuracy of GPS points. The first is adding some amount of error to the measurement
from a user defined random uniform distribution. The second is by rounding to
remove precision from the measurement. 
