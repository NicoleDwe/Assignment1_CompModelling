Assignment 1: Eye Tracking Anaylsis
===================================

### Study Group: Astrid Rybner, Kata Molnar, Nicole Dwenger and Sofie Rødkjær

### Feburary 27, 2020

``` r
#load data
Samples <- read_csv("Samples_merged.csv", col_types = cols(
  ParticipantID = col_character(),
  ParticipantGender = col_character(),
  EyeTracked = col_character(),
  Task = col_character(),
  SearchOrder = col_double(),
  ForagingType = col_character(),
  Trial = col_double(),
  Stimulus = col_character(),
  Video = col_character(),
  Time = col_double(),
  GazeX = col_double(),
  #GazeY = col_double(),
  PupilSize = col_double(),
  FixationNo = col_double(),
  Fix_StartTime = col_double(),
  Fix_EndTime = col_double(),
  Fix_Duration = col_double(),
  Fix_MeanX = col_double(),
  #Fix_MeanY = col_double(),
  Fix_MeanPupilSize = col_double(),
  SaccadeNo = col_double(),
  Sac_StartTime = col_double(),
  Sac_EndTime = col_double(),
  Sac_Duration = col_double(),
  Sac_StartX = col_double(),
  Sac_StartY = col_double(),
  Sac_EndX = col_double(),
  Sac_EndY = col_double(),
  Sac_PeakVelocity = col_double(),
  Sac_MeanVelocity = col_double(),
  Sac_Blink = col_logical(),
  Sac_Direction = col_character(),
  Sac_Amplitude = col_double()
  )) %>% 
  mutate(GazeY = 1051-GazeY, Fix_MeanY = 1051-Fix_MeanY) %>% 
  filter(Time<=41202)
```

Experiment 1: Top Down Effects on Eye Movement
----------------------------------------------

### Analysis

``` r
#question: do top down constraints (i.e. task) affect eye movement?
#dependent variable: saccade amplitude 
#fixed effect: condition (search vs count)
#random effects: participant, stimulus, independent asevery participant gets every stimulus + every stimulus gets every id, i.e. condition|stimulus, condition|ID

#saccade amplitude is a log-normal distribution == family = Gaussion(link = log)
#saccade amplitude ~ 1 + condition + (1 + condition|ID) + (1 + condition|Stimulus), family = Gaussian(link = Log)
#1 if interested if there is a significant difference, if interested in the difference 0 

#subset of data with only foraging data for the saccade-information 
Saccades <- Samples[!is.na(Samples$SaccadeNo) & Samples$Task == "Foraging",] %>% #no NAs in Saccadenumber
  group_by(ParticipantID, Trial, SaccadeNo) %>% 
  summarize(SaccadeAmplitude = mean(Sac_Amplitude), ForagingType = ForagingType[1], Stimulus = Stimulus[1]) %>% 
  filter(!is.na(SaccadeAmplitude))

#plot density plot
ggplot(Saccades, aes(SaccadeAmplitude, color = ForagingType)) + geom_density()
```

![](Data_analysis_files/figure-markdown_github/VISUAL%20FORAGING:%20SACCADE%20AMPLITUDE%20ANALYSIS-1.png)

``` r
### MODELS ###
#model with log-normal distribution
m1 <- glmer(SaccadeAmplitude ~ ForagingType + (1+ForagingType|ParticipantID) + (1+ForagingType|Stimulus),
            family = gaussian(link = "log"), data = Saccades)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(m1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: 
    ## SaccadeAmplitude ~ ForagingType + (1 + ForagingType | ParticipantID) +  
    ##     (1 + ForagingType | Stimulus)
    ##    Data: Saccades
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  20434.4  20490.5 -10208.2  20416.4     3753 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5870 -0.6024 -0.3034  0.3149  6.2025 
    ## 
    ## Random effects:
    ##  Groups        Name               Variance Std.Dev. Corr
    ##  Stimulus      (Intercept)         0.0000  0.0000       
    ##                ForagingTypeSearch  0.3844  0.6200    NaN
    ##  ParticipantID (Intercept)         0.0000  0.0000       
    ##                ForagingTypeSearch  0.1257  0.3545    NaN
    ##  Residual                         13.0516  3.6127       
    ## Number of obs: 3762, groups:  Stimulus, 10; ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)         0.92669    0.03553  26.082  < 2e-16 ***
    ## ForagingTypeSearch  0.53761    0.07907   6.799 1.05e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## FrgngTypSrc -0.449
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
#R squared
MuMIn::r.squaredGLMM(m1)
```

    ##              R2m       R2c
    ## [1,] 0.005284707 0.0269387

``` r
#model to get abolute values
m0 <- glmer(SaccadeAmplitude ~ 0 + ForagingType + (0 + ForagingType|ParticipantID) + (0 + ForagingType|Stimulus),
            family = gaussian(link = "log"), data = Saccades)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(m0)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: 
    ## SaccadeAmplitude ~ 0 + ForagingType + (0 + ForagingType | ParticipantID) +  
    ##     (0 + ForagingType | Stimulus)
    ##    Data: Saccades
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  20434.4  20490.5 -10208.2  20416.4     3753 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5870 -0.6024 -0.3034  0.3149  6.2025 
    ## 
    ## Random effects:
    ##  Groups        Name               Variance  Std.Dev.  Corr 
    ##  Stimulus      ForagingTypeCount  5.915e-09 7.691e-05      
    ##                ForagingTypeSearch 3.844e-01 6.200e-01 -1.00
    ##  ParticipantID ForagingTypeCount  0.000e+00 0.000e+00      
    ##                ForagingTypeSearch 1.257e-01 3.545e-01  NaN 
    ##  Residual                         1.305e+01 3.613e+00      
    ## Number of obs: 3762, groups:  Stimulus, 10; ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error t value Pr(>|z|)    
    ## ForagingTypeCount   0.92669    0.03553   26.08   <2e-16 ***
    ## ForagingTypeSearch  1.46430    0.07064   20.73   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             FrgnTC
    ## FrgngTypSrc 0.000 
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
#turn log values into normal scale
exp(0.92669) 
```

    ## [1] 2.526134

``` r
#2.526134 for count
exp(1.46430)
```

    ## [1] 4.324515

``` r
#4.324515 for search
```

### Visualisations

``` r
#density plot for saccade amplitude in the 2 conditions by participant
ggplot(Saccades, aes(SaccadeAmplitude, color = ParticipantID)) + geom_density() + facet_wrap(.~ForagingType)
```

![](Data_analysis_files/figure-markdown_github/VISUAL%20FORAGING:%20SACCADE%20AMPLITUDE%20VISUALISATIONS-1.png)

``` r
#boxplot for saccade amplitude for each condition and each stimulus
ggplot(Saccades, aes(y = SaccadeAmplitude, x = Stimulus, color = ForagingType)) + 
  geom_boxplot()
```

![](Data_analysis_files/figure-markdown_github/VISUAL%20FORAGING:%20SACCADE%20AMPLITUDE%20VISUALISATIONS-2.png)

``` r
#lineplot for saccade amplitude for each condition and ech stimulus
ggplot(Saccades, aes(y = SaccadeAmplitude, x = ForagingType, color = Stimulus, group = Stimulus)) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")
```

![](Data_analysis_files/figure-markdown_github/VISUAL%20FORAGING:%20SACCADE%20AMPLITUDE%20VISUALISATIONS-3.png)

``` r
#scanpaths
## get the picture
img <- jpeg::readJPEG('sheep.jpg')  
img <- grid::rasterGrob(img, width=unit(1, "npc"), height = unit(1,"npc"),
                        interpolate = FALSE)

#F7_2 for count
##subet
x = subset(Samples, ParticipantID ==    'F7_2' & Stimulus == 'sheep.jpg')
##summary dataset
Fix <- x[!is.na(x$FixationNo),] %>% 
  group_by(FixationNo) %>% # since I only have one participant and one trial
  summarize(MeanX = Fix_MeanX[1], MeanY = Fix_MeanY[1], Duration = Fix_Duration[1]) %>% 
  filter(Duration>=200) # only keep fixations > 300 ms
#plot
ggplot(Fix, aes(MeanX, MeanY, color = Fix$FixationNo)) + 
  annotation_custom(img, xmin = 0, xmax = 1680, ymin = 0, ymax = 1050) +
  geom_path(color = "yellow") +
  geom_point(size = Fix$Duration*.02, alpha = .6) +
  ggrepel::geom_text_repel(aes(label = Fix$Duration), size = 3, color = "white") +
  xlim(0,1680) + ylim(0,1050) + 
  theme(legend.position = "none", axis.title = element_blank()) +
  ggtitle("Counting")
```

![](Data_analysis_files/figure-markdown_github/VISUAL%20FORAGING:%20SACCADE%20AMPLITUDE%20VISUALISATIONS-4.png)

``` r
#F8_1 for search 
x = subset(Samples, ParticipantID ==    'F8_1' & Stimulus == 'sheep.jpg')
##summary dataset
Fix <- x[!is.na(x$FixationNo),] %>% 
  group_by(FixationNo) %>% # since I only have one participant and one trial
  summarize(MeanX = Fix_MeanX[1], MeanY = Fix_MeanY[1], Duration = Fix_Duration[1]) %>% 
  filter(Duration>=200) # only keep fixations > 300 ms
#plot
ggplot(Fix, aes(MeanX, MeanY, color = Fix$FixationNo)) + 
  annotation_custom(img, xmin = 0, xmax = 1680, ymin = 0, ymax = 1050) +
  geom_path(color = "yellow") +
  geom_point(size = Fix$Duration*.02, alpha = .6) +
  ggrepel::geom_text_repel(aes(label = Fix$Duration), size = 3, color = "white") +
  xlim(0,1680) + ylim(0,1050) + 
  theme(legend.position = "none", axis.title = element_blank()) +
  ggtitle("Search Task")
```

![](Data_analysis_files/figure-markdown_github/VISUAL%20FORAGING:%20SACCADE%20AMPLITUDE%20VISUALISATIONS-5.png)

Experiment 2: Social Engagement
-------------------------------

### Analysis

``` r
#question: does increased engagement (through directionality and eyebrow movement) have an effect on pupil size (arousal)?
#dependent variable: pupil size
#fixed effect: directionality (away, direct), eyebrow (no eyebrow, eyebrow)
#random effects: participant

#assume that pupil size is a log-normal distribution == family = Gaussion(link = log)
#saccade amplitude ~ 1 + directionality * eyebrow  + (1 + directionality * eyebrow|ID), family = Gaussian(link = Log)
#saccade amplitude ~ 1 + directionality + eyebrow  + (1 + directionality + eyebrow|ID), family = Gaussian(link = Log)
#1 if interested if there is a significant difference, if interested in the difference 0 

#subset of social data
Social <- Samples %>% 
  subset(Task == "SocialEngagement" & !is.na(PupilSize)) %>%
  group_by(ParticipantID, Trial, Video) %>% 
  summarize(PupilSize = mean(PupilSize))

#get information from video name
Divide_Video <- function(df){
  df$Video_Eyebrow[grepl("+o", df$Video)]='eyebrow'
  df$Video_Eyebrow[grepl("-o", df$Video)]='no eyebrow'
  df$Video_Direction[grepl("div",df$Video)]='away'
  df$Video_Direction[grepl("dir",df$Video)]='direct'
  return(df)}
Social <- Divide_Video(Social) #apply function

#turn variables into factors
Social$ParticipantID <- as.factor(Social$ParticipantID)
Social$Video_Eyebrow <- as.factor(Social$Video_Eyebrow)
Social$Video_Direction <- as.factor(Social$Video_Direction)

### MODELS ###
#with interaction
ms1 <- lme4::glmer(PupilSize ~ 1 + Video_Eyebrow*Video_Direction + (1+Video_Eyebrow*Video_Direction|ParticipantID), 
                  family = gaussian(link = "log"), data = Social)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(ms1) #interaction effect is not significant
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: 
    ## PupilSize ~ 1 + Video_Eyebrow * Video_Direction + (1 + Video_Eyebrow *  
    ##     Video_Direction | ParticipantID)
    ##    Data: Social
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    888.3    916.4   -429.2    858.3       33 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.01151 -0.77197  0.00001  0.48444  2.03957 
    ## 
    ## Random effects:
    ##  Groups        Name                                          Variance Std.Dev.
    ##  ParticipantID (Intercept)                                     209.42  14.471 
    ##                Video_Eyebrowno eyebrow                         908.65  30.144 
    ##                Video_Directiondirect                            10.04   3.169 
    ##                Video_Eyebrowno eyebrow:Video_Directiondirect  1091.34  33.035 
    ##  Residual                                                    14160.23 118.997 
    ##  Corr             
    ##                   
    ##   0.08            
    ##   0.44  0.93      
    ##  -0.06 -1.00 -0.92
    ##                   
    ## Number of obs: 48, groups:  ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                    8.79038    5.90785   1.488
    ## Video_Eyebrowno eyebrow                       -0.16163   12.30614  -0.013
    ## Video_Directiondirect                          0.00974    1.29361   0.008
    ## Video_Eyebrowno eyebrow:Video_Directiondirect  0.18557   13.48668   0.014
    ##                                               Pr(>|z|)
    ## (Intercept)                                      0.137
    ## Video_Eyebrowno eyebrow                          0.990
    ## Video_Directiondirect                            0.994
    ## Video_Eyebrowno eyebrow:Video_Directiondirect    0.989
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Vd_Eye Vd_Drc
    ## Vd_Eybrwney  0.081              
    ## Vd_Drctndrc  0.444  0.929       
    ## Vd_Eeyb:V_D -0.063 -0.995 -0.918
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
#without interaction
ms2 <- lme4::glmer(PupilSize ~ 1 + Video_Eyebrow+Video_Direction + (1+Video_Eyebrow+Video_Direction|ParticipantID), 
                  family = gaussian(link = "log"), data = Social)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(ms2) 
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: 
    ## PupilSize ~ 1 + Video_Eyebrow + Video_Direction + (1 + Video_Eyebrow +  
    ##     Video_Direction | ParticipantID)
    ##    Data: Social
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    810.7    829.4   -395.3    790.7       38 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.99438 -0.53558  0.09454  0.61748  2.36508 
    ## 
    ## Random effects:
    ##  Groups        Name                    Variance  Std.Dev. Corr     
    ##  ParticipantID (Intercept)             3.037e+02  17.4263          
    ##                Video_Eyebrowno eyebrow 5.002e-02   0.2237 1.00     
    ##                Video_Directiondirect   1.583e+01   3.9790 0.36 0.36
    ##  Residual                              2.402e+04 154.9733          
    ## Number of obs: 48, groups:  ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value Pr(>|z|)    
    ## (Intercept)             8.759895   0.046044 190.249  < 2e-16 ***
    ## Video_Eyebrowno eyebrow 0.002306   0.007030   0.328  0.74292    
    ## Video_Directiondirect   0.029297   0.010482   2.795  0.00519 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Vd_Eye
    ## Vd_Eybrwney 0.006        
    ## Vd_Drctndrc 0.359  0.036 
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
#get r2
MuMIn::r.squaredGLMM(ms2)
```

    ##               R2m        R2c
    ## [1,] 9.052476e-09 0.01398612

``` r
#0 model to get absolute values 
ms0 <- lme4::glmer(PupilSize ~ 0 + Video_Eyebrow+Video_Direction + (0+Video_Eyebrow+Video_Direction|ParticipantID), 
                  family = gaussian(link = "log"), data = Social)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(ms0) 
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: gaussian  ( log )
    ## Formula: 
    ## PupilSize ~ 0 + Video_Eyebrow + Video_Direction + (0 + Video_Eyebrow +  
    ##     Video_Direction | ParticipantID)
    ##    Data: Social
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    810.7    829.4   -395.3    790.7       38 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.99438 -0.53558  0.09454  0.61748  2.36508 
    ## 
    ## Random effects:
    ##  Groups        Name                    Variance Std.Dev. Corr     
    ##  ParticipantID Video_Eyebroweyebrow      303.67  17.426           
    ##                Video_Eyebrowno eyebrow   311.52  17.650  1.00     
    ##                Video_Directiondirect      15.83   3.979  0.36 0.36
    ##  Residual                              24016.73 154.973           
    ## Number of obs: 48, groups:  ParticipantID, 6
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value Pr(>|z|)    
    ## Video_Eyebroweyebrow     8.75990    0.04603 190.308  < 2e-16 ***
    ## Video_Eyebrowno eyebrow  8.76220    0.04661 187.996  < 2e-16 ***
    ## Video_Directiondirect    0.02930    0.01048   2.795  0.00519 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             Vd_Eyb Vd_Eye
    ## Vd_Eybrwney 0.989        
    ## Vd_Drctndrc 0.359  0.360 
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
#go back from log scale
exp(8.76) #6374.112 away, eyebrow and also away, no eyebrow
```

    ## [1] 6374.112

``` r
exp(8.76+0.03) #6568.232 away, direct
```

    ## [1] 6568.232

``` r
#faster way: or make predictions and then exp
Social$Preds <- predict(ms2)
Goback <- Social %>%
  group_by(Video_Eyebrow, Video_Direction) %>% 
  summarize((exp(mean(Preds))))
Goback
```

    ## # A tibble: 4 x 3
    ## # Groups:   Video_Eyebrow [2]
    ##   Video_Eyebrow Video_Direction `(exp(mean(Preds)))`
    ##   <fct>         <fct>                          <dbl>
    ## 1 eyebrow       away                           6373.
    ## 2 eyebrow       direct                         6563.
    ## 3 no eyebrow    away                           6388.
    ## 4 no eyebrow    direct                         6578.

### Visualisations

``` r
#add column combining the two factors
Social$Direction_Eyebrow <- paste(Social$Video_Direction, Social$Video_Eyebrow)

#density of pupil sizes in the 4 conditions
ggplot(Social, aes(PupilSize, color = Direction_Eyebrow)) + geom_density()
```

![](Data_analysis_files/figure-markdown_github/SOCIAL%20ENGAGMENT%20VISUALISATIONS-1.png)

``` r
#boxplot for the 4 conditions
#relevel factor
Social$Video_Eyebrow <- relevel(Social$Video_Eyebrow, "no eyebrow", "eyebrow")
ggplot(aes(y = PupilSize, x = Video_Direction, color = Video_Eyebrow), data = Social) + 
  geom_boxplot() 
```

![](Data_analysis_files/figure-markdown_github/SOCIAL%20ENGAGMENT%20VISUALISATIONS-2.png)

``` r
#another boxplot for the 4 conditions 
ggplot(Social, aes(Direction_Eyebrow, PupilSize, color=Direction_Eyebrow)) + geom_boxplot()
```

![](Data_analysis_files/figure-markdown_github/SOCIAL%20ENGAGMENT%20VISUALISATIONS-3.png)

``` r
#lineplot for each participant, all conditions
ggplot(Social, aes(y = PupilSize, x = Video_Direction, color = ParticipantID, group = ParticipantID)) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  facet_wrap(~Video_Eyebrow)
```

![](Data_analysis_files/figure-markdown_github/SOCIAL%20ENGAGMENT%20VISUALISATIONS-4.png)
