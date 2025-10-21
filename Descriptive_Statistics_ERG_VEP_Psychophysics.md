# Descriptive Statistics for ERG/VEP and Psychophysics

This summary provides descriptive statistics for the flash ERG dataset, the flicker ERG/VEP dataset, and psychophysical measurements. Counts reflect the number of non-missing observations per measure.

> **Publishable tables:** Run `Rscript generate_descriptive_statistics_tables.R` to produce `Descriptive_Statistics_ERG_VEP_Psychophysics_Table.html`, an HTML report with journal-ready styling (requires the `readxl` and `knitr` packages; styling is enhanced when `kableExtra` is installed).

## Flash ERG Timing and Amplitude Metrics

Subjects included: 33

| Measure | Count | Mean | Std Dev | Min | Max |
| --- | ---: | ---: | ---: | ---: | ---: |
| OFF_Flash_ERG_b_wave_Time | 33 | 20.79 | 3.560 | 19.00 | 40.00 |
| OFF_Flash_ERG_PhNR_Time | 33 | 55.48 | 23.34 | 29.00 | 134.00 |
| ON_Flash_ERG_a_wave_Time | 33 | 16.48 | 0.7550 | 16.00 | 19.00 |
| ON_Flash_ERG_b_wave_Time | 33 | 33.42 | 4.108 | 27.00 | 38.00 |
| ON_Flash_ERG_PhNR_Time | 33 | 65.00 | 9.314 | 52.00 | 91.00 |
| OFF_Flash_ERG_b_wave_Amp | 33 | 31,719.5 | 25,660.3 | 2,790.6 | 151,140.0 |
| OFF_Flash_ERG_PhNR_Amp | 33 | 29,782.9 | 19,548.3 | 5,108.4 | 75,195.7 |
| ON_Flash_ERG_a_wave_Amp | 33 | 26,795.7 | 9,064.4 | 11,783.5 | 51,040.1 |
| ON_Flash_ERG_b_wave_Amp | 33 | 52,991.8 | 18,856.4 | 16,102.1 | 100,937.1 |
| ON_Flash_ERG_PhNR_Amp | 33 | 61,962.4 | 17,709.0 | 24,455.4 | 93,330.9 |
| Axial_Length_OD | 33 | 25.00 | 1.162 | 22.50 | 27.51 |
| re | 33 | -3.020 | 2.311 | -6.880 | 1.150 |

## Flicker ERG/VEP Amplitude, Phase, and SNR Metrics

Subjects included: 33

| Measure | Count | Mean | Std Dev | Min | Max |
| --- | ---: | ---: | ---: | ---: | ---: |
| OFF_Flicker_ERG_Amp_9Hz | 33 | 429.05 | 354.41 | 60.18 | 1,444.6 |
| ON_Flicker_ERG_Amp_9Hz | 33 | 469.83 | 362.61 | 35.45 | 1,457.9 |
| OFF_Flicker_VEP_Amp_9Hz | 33 | 490.52 | 525.94 | 53.86 | 2,771.7 |
| ON_Flicker_VEP_Amp_9Hz | 33 | 626.07 | 535.33 | 92.44 | 2,167.8 |
| OFF_Flicker_ERG_Amp_10Hz | 33 | 6,083.7 | 2,757.1 | 2,106.1 | 13,710.5 |
| OFF_Flicker_ERG_Phase_10Hz | 33 | -1.223 | 0.3731 | -1.836 | -0.4717 |
| ON_Flicker_ERG_Amp_10Hz | 33 | 4,851.6 | 1,702.1 | 1,958.6 | 8,815.7 |
| ON_Flicker_ERG_Phase_10Hz | 33 | -0.7892 | 0.5986 | -1.858 | 0.9752 |
| OFF_Flicker_VEP_Amp_10Hz | 33 | 4,374.8 | 2,740.6 | 576.29 | 10,229.3 |
| OFF_Flicker_VEP_Phase_10Hz | 33 | -0.5122 | 1.450 | -3.076 | 1.681 |
| ON_Flicker_VEP_Amp_10Hz | 33 | 5,142.0 | 3,000.9 | 628.45 | 14,623.4 |
| ON_Flicker_VEP_Phase_10Hz | 33 | -1.072 | 1.277 | -2.814 | 3.053 |
| OFF_Flicker_ERG_Amp_20Hz | 33 | 6,863.0 | 2,548.2 | 3,207.0 | 14,358.1 |
| OFF_Flicker_ERG_Phase_20Hz | 33 | -2.549 | 1.043 | -3.135 | 3.103 |
| ON_Flicker_ERG_Amp_20Hz | 33 | 6,966.3 | 1,903.0 | 3,974.1 | 10,909.7 |
| ON_Flicker_ERG_Phase_20Hz | 33 | 1.404 | 0.2120 | 0.9060 | 1.829 |
| OFF_Flicker_VEP_Amp_20Hz | 33 | 1,932.8 | 1,519.5 | 140.93 | 6,978.7 |
| OFF_Flicker_VEP_Phase_20Hz | 33 | -0.1269 | 1.743 | -3.005 | 2.908 |
| ON_Flicker_VEP_Amp_20Hz | 33 | 2,566.3 | 1,533.3 | 136.39 | 7,324.8 |
| ON_Flicker_VEP_Phase_20Hz | 33 | -0.8566 | 1.355 | -3.100 | 2.902 |
| OFF_Flicker_ERG_Amp_40Hz | 33 | 4,454.2 | 1,333.0 | 2,425.9 | 8,790.3 |
| OFF_Flicker_ERG_Phase_40Hz | 33 | 1.615 | 0.3089 | 0.8936 | 2.328 |
| ON_Flicker_ERG_Amp_40Hz | 33 | 6,389.2 | 1,935.5 | 2,786.4 | 11,472.0 |
| ON_Flicker_ERG_Phase_40Hz | 33 | -1.807 | 0.2820 | -2.670 | -1.234 |
| OFF_Flicker_VEP_Amp_40Hz | 33 | 465.98 | 312.52 | 79.16 | 1,260.7 |
| OFF_Flicker_VEP_Phase_40Hz | 33 | -0.4808 | 1.549 | -3.064 | 2.885 |
| ON_Flicker_VEP_Amp_40Hz | 33 | 618.59 | 386.99 | 173.28 | 1,521.4 |
| ON_Flicker_VEP_Phase_40Hz | 33 | 0.1331 | 1.796 | -3.098 | 2.993 |
| OFF_Flicker_ERG_SNR_10_9 | 33 | 23.68 | 20.11 | 2.602 | 90.22 |
| ON_Flicker_ERG_SNR_10_9 | 33 | 19.52 | 23.14 | 1.980 | 129.20 |
| OFF_Flicker_VEP_SNR_10_9 | 33 | 16.39 | 17.96 | 2.141 | 79.39 |
| ON_Flicker_VEP_SNR_10_9 | 33 | 15.63 | 18.83 | 2.997 | 73.90 |
| OFF_Flicker_ERG_Amp_Ratio_10_20Hz | 33 | 0.8917 | 0.2643 | 0.5572 | 1.682 |
| ON_Flicker_ERG_Amp_Ratio_10_20Hz | 33 | 0.7244 | 0.2862 | 0.2995 | 1.558 |
| OFF_Flicker_VEP_Amp_Ratio_10_20Hz | 33 | 3.155 | 2.211 | 0.6425 | 9.009 |
| ON_Flicker_VEP_Amp_Ratio_10_20Hz | 33 | 3.402 | 4.507 | 0.2073 | 24.61 |
| Axial_Length_OD | 33 | 25.00 | 1.162 | 22.50 | 27.51 |
| re | 33 | -3.020 | 2.311 | -6.880 | 1.150 |

## Psychophysical Performance Metrics

Subjects included: 34

| Measure | Count | Mean | Std Dev | Min | Max |
| --- | ---: | ---: | ---: | ---: | ---: |
| avg_ax_length | 34 | 24.97 | 1.203 | 22.41 | 27.41 |
| od_ax_length | 34 | 25.00 | 1.197 | 22.50 | 27.51 |
| od_re | 34 | -3.183 | 2.267 | -6.565 | 0.06500 |
| near_on | 34 | 0.02068 | 0.01093 | 0.01054 | 0.07550 |
| near_off | 34 | 0.02121 | 8.94e-03 | 0.01063 | 0.04973 |
| far_on | 34 | 0.09644 | 0.04866 | 0.05000 | 0.2000 |
| far_off | 34 | 0.1006 | 0.04999 | 0.05000 | 0.2000 |
| SPH | 34 | -2.768 | 2.197 | -6.750 | 0.7500 |
| CYL | 34 | -0.8453 | 0.6505 | -2.250 | 1.000 |
| AXIS | 34 | 64.29 | 74.02 | 0.0000 | 178.00 |
| SE | 34 | -3.191 | 2.240 | -6.875 | 0.5000 |
