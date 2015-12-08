# ANRecognitionSystem
###accepted criteria
  - single digit:			90%                    
  - single alphabet:  75%                     
  - sequence:         50%

###current progress
  - single characters:
  
    digit | alphabet
    ----- | --------
    97.8% | 92.3%

    :arrow_right: a combined single accuracy of 93.8%

  - sequence: 
  
    2 characters | 3 characters | 4 characters | 5 characters | 6 characters
    ------------ | ------------ | ------------ | ------------ | ------------
    75% | 75% | 70% | 60% | 55%
    
    :arrow_right: a combined sequence accuracy of 67%

####notes:
  1. training and testing pics are not included here due to the volume
  2. total accuracy meets criteria
  3. both arraybuffer && actor version are included under the src directory
  4. further data analysis on the results (output into csv file) is conducted on Zeppelin (shared the .json version of our notebook, you might need json editors like http://jsoneditoronline.org/ to save the .json file locally in order to import into Zeppelin)
  5. in order for any of you who might wanna try out the test file, those that does not require main resources are tagged as "PartialTest", which can be run using:
    `$ sbt`
    `> test-only -- -n PartialTest`
While if sufficient number of images have been put into the main resource folder, you will be able to execute the whole test file simply using `> test`. Remember, you'll need to delete the file `001.txt` under directory `test/resources/specText` (if it was generated from the previous test) before you launch each test.
