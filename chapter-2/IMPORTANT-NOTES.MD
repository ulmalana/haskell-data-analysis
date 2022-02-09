# IMPORTANT NOTES
## Some important notes regarding chapter 2:

* Typo in 'applyToColumnInCSVFile' function 'handleCSVError =='
    Solution 'handleCSVError csv ='
* Be carefule with deprecated packages
	In this chapter, it was 'sqlite'.
	Solution 'sqlite-simple'
* Make sure to check duplicated packages
	This gave me a quite nasty bug which was difficult to figure out.
* check the # fields in the CSV file
	Additional fields were added in the current version format of the CSV file (in 2022). This is not compatible with the code in the book.
