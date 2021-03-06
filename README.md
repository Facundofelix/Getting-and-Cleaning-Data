# Getting-and-Cleaning-Data

### Independent_data
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

**The data set includes:**

"1" "Subject"

"2" "Activity"

"3" "Time Body  Accelerometer.Mean of X"

"4" "Time Body  Accelerometer.Mean of Y"

"5" "Time Body  Accelerometer.Mean of Z"

"6" "Time Gravity Accelerometer.Mean of X"

"7" "Time Gravity Accelerometer.Mean of Y"

"8" "Time Gravity Accelerometer.Mean of Z"

"9" "Time Body  Accelerometer Jerk.Mean of X"

"10" "Time Body  Accelerometer Jerk.Mean of Y"

"11" "Time Body  Accelerometer Jerk.Mean of Z"

"12" "Time Body Gyroscope .Mean of X"

"13" "Time Body Gyroscope .Mean of Y"

"14" "Time Body Gyroscope .Mean of Z"

"15" "Time Body Gyroscope  Jerk.Mean of X"

"16" "Time Body Gyroscope  Jerk.Mean of Y"

"17" "Time Body Gyroscope  Jerk.Mean of Z"

"18" "Time Body  AccelerometerMagnitude.Mean.."

"19" "Time Gravity AccelerometerMagnitude.Mean.."

"20" "Time Body  Accelerometer JerkMagnitude.Mean.."

"21" "Time Body Gyroscope Magnitude.Mean.."

"22" "Time Body Gyroscope  JerkMagnitude.Mean.."

"23" "Frecuency Body Accelerometer.Mean of X"

"24" "Frecuency Body Accelerometer.Mean of Y"

"25" "Frecuency Body Accelerometer.Mean of Z"

"26" "Frecuency Body Accelerometer.MeanFrequency of X"

"27" "Frecuency Body Accelerometer.MeanFrequency of Y"

"28" "Frecuency Body Accelerometer.MeanFrequency of Z"

"29" "Frecuency Body Accelerometer Jerk.Mean of X"

"30" "Frecuency Body Accelerometer Jerk.Mean of Y"

"31" "Frecuency Body Accelerometer Jerk.Mean of Z"

"32" "Frecuency Body Accelerometer Jerk.MeanFrequency of X"

"33" "Frecuency Body Accelerometer Jerk.MeanFrequency of Y"

"34" "Frecuency Body Accelerometer Jerk.MeanFrequency of Z"

"35" "Frecuency BodyGyroscope .Mean of X"

"36" "Frecuency BodyGyroscope .Mean of Y"

"37" "Frecuency BodyGyroscope .Mean of Z"

"38" "Frecuency BodyGyroscope .MeanFrequency of X"

"39" "Frecuency BodyGyroscope .MeanFrequency of Y"

"40" "Frecuency BodyGyroscope .MeanFrequency of Z"

"41" "Frecuency Body AccelerometerMagnitude.Mean.."

"42" "Frecuency Body AccelerometerMagnitude.MeanFrequency.."

"43" "Frecuency Body Accelerometer JerkMagnitude.Mean.."

"44" "Frecuency Body Accelerometer JerkMagnitude.MeanFrequency.."

"45" "Frecuency BodyGyroscope Magnitude.Mean.."

"46" "Frecuency BodyGyroscope Magnitude.MeanFrequency.."

"47" "Frecuency BodyGyroscope  JerkMagnitude.Mean.."

"48" "Frecuency BodyGyroscope  JerkMagnitude.MeanFrequency.."

"49" "Angle.Time Body  AccelerometerMean.Gravity."

"50" "Angle.Time Body  Accelerometer JerkMean..GravityMean."

"51" "Angle.Time Body Gyroscope Mean.GravityMean."

"52" "Angle.Time Body Gyroscope  JerkMean.GravityMean."

"53" "Ang of X.GravityMean."

"54" "Ang of Y.GravityMean."

"55" "Ang of Z.GravityMean."

"56" "Time Body  Accelerometer.STD of X"

"57" "Time Body  Accelerometer.STD of Y"

"58" "Time Body  Accelerometer.STD of Z"

"59" "Time Gravity Accelerometer.STD of X"

"60" "Time Gravity Accelerometer.STD of Y"

"61" "Time Gravity Accelerometer.STD of Z"

"62" "Time Body  Accelerometer Jerk.STD of X"

"63" "Time Body  Accelerometer Jerk.STD of Y"

"64" "Time Body  Accelerometer Jerk.STD of Z"

"65" "Time Body Gyroscope .STD of X"

"66" "Time Body Gyroscope .STD of Y"

"67" "Time Body Gyroscope .STD of Z"

"68" "Time Body Gyroscope  Jerk.STD of X"

"69" "Time Body Gyroscope  Jerk.STD of Y"

"70" "Time Body Gyroscope  Jerk.STD of Z"

"71" "Time Body  AccelerometerMagnitude.STD.."

"72" "Time Gravity AccelerometerMagnitude.STD.."

"73" "Time Body  Accelerometer JerkMagnitude.STD.."

"74" "Time Body Gyroscope Magnitude.STD.."

"75" "Time Body Gyroscope  JerkMagnitude.STD.."

"76" "Frecuency Body Accelerometer.STD of X"

"77" "Frecuency Body Accelerometer.STD of Y"

"78" "Frecuency Body Accelerometer.STD of Z"

"79" "Frecuency Body Accelerometer Jerk.STD of X"

"80" "Frecuency Body Accelerometer Jerk.STD of Y"

"81" "Frecuency Body Accelerometer Jerk.STD of Z"

"82" "Frecuency BodyGyroscope .STD of X"

"83" "Frecuency BodyGyroscope .STD of Y"

"84" "Frecuency BodyGyroscope .STD of Z"

"85" "Frecuency Body AccelerometerMagnitude.STD.."

"86" "Frecuency Body Accelerometer JerkMagnitude.STD.."

"87" "Frecuency BodyGyroscope Magnitude.STD.."

"88" "Frecuency BodyGyroscope  JerkMagnitude.STD.."
