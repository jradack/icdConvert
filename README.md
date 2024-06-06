# icdConvert Package

This R package converts between ICD 9 and ICD 10 diagnosis and procedure codes.
The Centers for Medicare and Medicaid Services (CMS) implemented the switch to ICD 10 on October 1, 2015, and the old diagnosis and procedure codes did not have a simple one-to-one relationship with the new ones.
CMS created the General Equivalence Mapping (GEM) files, a crosswalk for translating a diagnosis or procedure code from ICD 9 to ICD 10, and vice versa.

GEM files were created for fiscal years 2016, 2017, and 2018.
`icdConvert` utilizes the GEM files from 2018 to perform the mapping.

The inspiration for this package was the [`touch`](https://cran.r-project.org/web/packages/touch/index.html) package, with the additional capability of mapping procedure codes.
Similar to the `touch` package, `icdConvert` provides several mapping methods, including forward, backward, bidirectional, and multi-stage.
`icdConvert` also allows for matching codes based on just a prefix, which can be useful in the case of finding matches for a code with multiple subcodes.

# Reference Links

Further information on GEMs can be found here:

- [General Equivalence Mappings](https://www.nber.org/research/data/icd-9-cm-and-icd-10-cm-and-icd-10-pcs-crosswalk-or-general-equivalence-mappings)
- [GEM NBER FAQ](https://data.nber.org/gem/GEMs-CrosswalksBasicFAQ.pdf)
- [GEM ASCO Tutorial](https://society.asco.org/practice-policy/billing-coding-reporting/icd-10/general-equivalence-mappings-gems)
