# ohcleandat 0.3.1

* Explicitly adding `set_diff` function which was previously a hidden dependency on the {ecohealthalliance/airtabler} package

# ohcleandat 0.3.0

* Adding GPS obfuscation function - this function uses two methods to reduce the
accuracy of GPS points. The first is adding some amount of error to the measurement
from a user defined random uniform distribution. The second is by rounding to
remove precision from the measurement. 