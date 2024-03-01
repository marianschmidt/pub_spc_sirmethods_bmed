# project:      SPC - Data Analysis
# script:       01.x1.fn_analysis.helper.functions.R
# author:       Marian Eberl
# recent date:  2022-03-25
# versions:     2022-03-25 first version
# req packages: tidyverse
# content:		  create helper functions for all SPC analyses
# used in:
# dependend on:
# comments:
# execution time:
# quality check: (version 2017-07-28, YYYY-MM-DD, Name Programmer)
#----------------------------------------------------------------------------


#---------- Define functions -----------------------------

## -------- Site codings -------------

cr_subsite_lung <- function(df, site_var, site_values, hist_var, new_var_subsite = t_sublung, 
                            na_to_unspecified = FALSE){
  #t_sublung: Histologic subtype of lung cancer 
  #Coding table: for definition see SPN - Cancer Coding Systems_V1.0_20200217.xlsx, sheet 5_Lung_Subsite
  #Source t_sublung: old codings: Barclay ME, Lyratzopoulos G, Walter FM, Jefferies S, Peake MD, Rintoul RC. 
  #                               Incidence of second and higher order smoking-related primary cancers following 
  #                               lung cancer: a population-based cohort study. Thorax. 2019;74(5):466–72; 
  #                  new codings adapted from ICD-O-3 SEER Site/Histology Validation List 2015 in combination with 
  #                               Fritz AG, Percy C, Jack A, Shanmugaratnam K, Sobin L, Parkin DM, et al., editors. 
  #                               International classification of diseases for oncology: ICD-O [Internet]. First 
  #                               Revision. Geneva: World Health Organization; 2013 [cited 2019 Aug 14]. 
  #                               Available from: http://public.eblib.com/choice/publicfullrecord.aspx?p=1681147
  
  
   df %>%
    tidytable::mutate(new_var := tidytable::case_when(
      {{site_var}} %in% site_values & {{hist_var}} == "8041"  ~ 1,
      {{site_var}} %in% site_values & {{hist_var}} == "8042"  ~ 1,
      {{site_var}} %in% site_values & {{hist_var}} == "8043"  ~ 1,
      {{site_var}} %in% site_values & {{hist_var}} == "8044"  ~ 1,
      {{site_var}} %in% site_values & {{hist_var}} == "8045"  ~ 1,
      {{site_var}} %in% site_values & {{hist_var}} == "8002"  ~ 1,
      {{site_var}} %in% site_values & {{hist_var}} == "8140"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8141"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8143"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8144"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8147"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8190"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8201"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8211"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8250"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8251"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8252"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8253"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8254"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8260"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8310"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8320"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8323"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8401"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8440"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8470"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8480"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8481"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8490"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8510"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8550"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8551"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8570"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8571"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8572"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8573"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8574"  ~ 2,
      {{site_var}} %in% site_values & {{hist_var}} == "8575"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8576"  ~ 2, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8052"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8070"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8071"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8072"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8073"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8074"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8076"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8078"  ~ 3, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8083"  ~ 3,
      {{site_var}} %in% site_values & {{hist_var}} == "8084"  ~ 3, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8240"  ~ 4,
      {{site_var}} %in% site_values & {{hist_var}} == "8241"  ~ 4,
      {{site_var}} %in% site_values & {{hist_var}} == "8242"  ~ 4, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8243"  ~ 4,
      {{site_var}} %in% site_values & {{hist_var}} == "8244"  ~ 4,
      {{site_var}} %in% site_values & {{hist_var}} == "8245"  ~ 4,
      {{site_var}} %in% site_values & {{hist_var}} == "8249"  ~ 4,
      {{site_var}} %in% site_values & {{hist_var}} == "8012"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8013"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8014"  ~ 5, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8015"  ~ 5, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8022"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8030"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8031"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8032"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8033"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8034"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8035"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8046"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8050"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8051"  ~ 5, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8075"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8082"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8123"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8145"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8200"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8230"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8231"  ~ 5, #new coding - Carcinoma simplex
      {{site_var}} %in% site_values & {{hist_var}} == "8246"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8255"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8430"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8500"  ~ 5, #new coding                             
      {{site_var}} %in% site_values & {{hist_var}} == "8560"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8562"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8940"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8972"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8973"  ~ 5, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8980"  ~ 5,
      {{site_var}} %in% site_values & {{hist_var}} == "8981"  ~ 5, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8000"  ~ 6,
      {{site_var}} %in% site_values & {{hist_var}} == "8001"  ~ 6,
      {{site_var}} %in% site_values & {{hist_var}} == "8003"  ~ 6,
      {{site_var}} %in% site_values & {{hist_var}} == "8004"  ~ 6,
      {{site_var}} %in% site_values & {{hist_var}} == "8005"  ~ 6, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8010"  ~ 6,
      {{site_var}} %in% site_values & {{hist_var}} == "8020"  ~ 6,
      {{site_var}} %in% site_values & {{hist_var}} == "8021"  ~ 6,
      {{site_var}} %in% site_values & {{hist_var}} == "8800"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8801"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8802"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8803"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8804"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8805"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8806"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8810"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8811"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8814"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8830"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8840"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8850"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8851"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8852"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8853"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8854"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8855"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8857"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8858"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8890"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8891"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8894"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8895"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8896"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8900"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8901"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8902"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8910"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8912"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8920"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8921"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8933"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8934"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8935"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8936"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8963"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8990"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9040"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "9041"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9042"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9043"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9120"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "9125"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9150"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9170"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "9180"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "9220"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "9260"  ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "9540"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9560"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9561"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9571"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9580"  ~ 91, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8011"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8040"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8090"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8094"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8095"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8097"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8120"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8124"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8130"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8146"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8150"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8154"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8155"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8160"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8162"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8170"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8210"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8247"  ~ 92, #new coding - Merkel Cell Carcinoma
      {{site_var}} %in% site_values & {{hist_var}} == "8262"  ~ 92, #new coding - villous adenocarcinoma
      {{site_var}} %in% site_values & {{hist_var}} == "8263"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8270"  ~ 92, #new coding - chromophobe
      {{site_var}} %in% site_values & {{hist_var}} == "8290"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8312"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8332"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8340"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8341"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8342"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8345"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8350"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8370"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8410"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8441"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8450"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8460"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8471"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8503"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8507"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8520"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8524"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8525"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8530"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8552"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8580"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8581"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8582"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8584"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8585"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8650"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8680"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8693"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8710"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8711"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8720"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8721"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8730"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8743"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8770"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8771"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8772"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8815"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8824"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8941"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8951"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8970"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8971"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8982"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9015"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9020"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9050"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9061"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9062"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9064"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9065"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9070"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9071"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9072"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9080"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9084"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9085"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9100"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9101"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9130"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9133"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9181"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9182"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9231"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9240"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9250"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9251"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9310"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9364"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9365"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9370"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9473"  ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9500"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9503"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9522"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9530"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "9581"  ~ 92, #new coding
      {{site_var}} %in% site_values & {{hist_var}} == "8333"  ~ 93,
      na_to_unspecified == TRUE & {{site_var}} %in% site_values ~ 6, #enforce option na_to_unspecific
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(new_var = "Subsite of lung cancer") %>%
    sjlabelled::set_labels(new_var, labels = c("Small-cell carcinoma" = 1,
                                               "Adenocarcinoma" = 2,
                                               "Squamous cell carcinoma" = 3,
                                               "Carcinoid" = 4,
                                               "Other NSCLC" = 5,
                                               "Unspecified lung" = 6,
                                               "Excluded - sarcoma" = 91,
                                               "Excluded - unusual" = 92,
                                               "Excluded - benign" = 93)) %>%
    tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    tidytable::rename({{new_var_subsite}} := new_var)
}


cr_subsiteiarc_lung <- function(df, site_var, site_values, hist_var, new_var_subsite = t_sublungiarc, 
                            na_to_unspecified = FALSE){
  #t_sublungiarc: Histologic subtype of lung cancer IARC groups
  #Source:  IARC Histological Groups t_sublungiarc: based on Ferlay J, Rous B. Chapter 4: Histological groups. In: Cancer incidence in five continents Volume X. 2021.
  #Coding table: for definition see SPN - Cancer Coding Systems_V1.0_20200217.xlsx, sheet 5_Lung_Subsite
   
  #histology codes valid according to SEER ICD-O-3 Site/Histology Validation List 2022
  seer_valid_codes <- c(8000, 8001, 8002, 8003, 8004, 8005, 8010, 8010, 8011, 8012, 8013, 8014, 8015, 8020, 8021, 8022, 8023, 
                        8030, 8031, 8032, 8033, 8034, 8035, 8046, 8041, 8042, 8043, 8044, 8045, 8050, 8050, 8051, 8052, 8052, 
                        8070, 8070, 8071, 8072, 8073, 8074, 8075, 8076, 8076, 8078, 8083, 8120, 8120, 8121, 8122, 8123, 8124, 
                        8140, 8140, 8141, 8143, 8147, 8200, 8201, 8201, 8230, 8230, 8231, 8240, 8241, 8242, 8243, 8244, 8245, 
                        8246, 8249, 8250, 8250, 8251, 8252, 8253, 8253, 8254, 8255, 8256, 8257, 8260, 8265, 8310, 8320, 8323, 
                        8333, 8430, 8480, 8481, 8490, 8510, 8550, 8551, 8560, 8562, 8570, 8571, 8572, 8573, 8574, 8575, 8576, 
                        8714, 8800, 8801, 8802, 8803, 8804, 8805, 8806, 8810, 8811, 8813, 8814, 8815, 8825, 8830, 8842, 8890, 
                        8891, 8894, 8895, 8896, 8900, 8901, 8902, 8910, 8912, 8972, 8973, 8980, 8981, 8982, 8990, 8991, 9050, 
                        9050, 9051, 9052, 9053, 9120, 9133, 9137, 9140, 9174, 9590, 9591, 9596, 9650, 9651, 9652, 9653, 9654, 
                        9655, 9659, 9661, 9662, 9663, 9664, 9665, 9667, 9670, 9671, 9673, 9675, 9678, 9679, 9680, 9684, 9687, 
                        9688, 9690, 9691, 9695, 9698, 9699, 9701, 9702, 9705, 9712, 9714, 9715, 9719, 9724, 9727, 9728, 9729, 
                        9731, 9734, 9735, 9737, 9738, 9740, 9741, 9749, 9750, 9751, 9754, 9755, 9756, 9757, 9758, 9759, 9766, 
                        9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9823, 9831, 9837, 9965, 9967, 9971, 9975)
  
  #additional codes valid according to Barclay et al 2019
  barclay_valid_add <- c(8082, 8084, 8144, 8145, 8190, 8211, 8401, 8440, 8470, 8850, 8851, 8852, 8854, 8855, 8920, 8921, 8933, 
                         8940, 8963, 9040, 9180, 9220, 9260)
  
  valid <- c(seer_valid_codes, barclay_valid_add)
  
  df %>%
    tidytable::mutate(new_var := tidytable::case_when(
      {{site_var}} %in% site_values & {{hist_var}} == "8050" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8051" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8052" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8070" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8071" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8072" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8073" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8074" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8075" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8076" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8078" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8083" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8084" ~ 11,
      {{site_var}} %in% site_values & {{hist_var}} == "8140" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8211" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8230" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8231" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8250" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8251" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8252" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8253" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8254" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8255" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8260" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8323" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8480" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8481" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8490" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8550" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8551" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8570" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8571" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8572" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8573" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8574" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8576" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8552" ~ 12,
      {{site_var}} %in% site_values & {{hist_var}} == "8041" ~ 13,
      {{site_var}} %in% site_values & {{hist_var}} == "8042" ~ 13,
      {{site_var}} %in% site_values & {{hist_var}} == "8043" ~ 13,
      {{site_var}} %in% site_values & {{hist_var}} == "8044" ~ 13,
      {{site_var}} %in% site_values & {{hist_var}} == "8045" ~ 13,
      {{site_var}} %in% site_values & {{hist_var}} == "8010" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8011" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8012" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8014" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8015" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8020" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8021" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8022" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8030" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8031" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8035" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8310" ~ 14,
      {{site_var}} %in% site_values & {{hist_var}} == "8013" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8032" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8033" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8034" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8040" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8046" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8082" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8090" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8094" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8095" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8097" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8120" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8123" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8124" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8130" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8141" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8143" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8144" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8145" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8146" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8147" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8150" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8154" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8155" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8160" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8162" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8170" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8190" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8200" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8201" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8210" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8240" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8241" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8242" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8243" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8244" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8245" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8246" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8247" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8249" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8262" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8263" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8270" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8290" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8312" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8320" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8332" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8333" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8340" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8341" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8342" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8345" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8350" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8370" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8401" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8410" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8430" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8440" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8441" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8450" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8460" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8470" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8471" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8500" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8503" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8507" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8510" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8520" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8524" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8525" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8530" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8560" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8562" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8575" ~ 15,
      {{site_var}} %in% site_values & {{hist_var}} == "8800" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8801" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8802" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8803" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8804" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8805" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8806" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8810" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8811" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8830" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8840" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8850" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8851" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8852" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8853" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8854" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8855" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8857" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8858" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8890" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8891" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8894" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8895" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8896" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8900" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8901" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8902" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8910" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8912" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8920" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8921" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8980" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8981" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9040" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9041" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9042" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9043" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9120" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9125" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9130" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9133" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9150" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9540" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9560" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9561" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9571" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9580" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "9044" ~ 21,
      {{site_var}} %in% site_values & {{hist_var}} == "8580" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8582" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8584" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8585" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8650" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8680" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8693" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8710" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8711" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8720" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8721" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8730" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8743" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8770" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8771" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8772" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8814" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8815" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8824" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8934" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8935" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8936" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8941" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8951" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8970" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8971" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8972" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8973" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8982" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8990" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9015" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9020" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9061" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9064" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9065" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9070" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9071" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9072" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9080" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9084" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9085" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9100" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9101" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9170" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9181" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9182" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9231" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9240" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9250" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9251" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9310" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9364" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9365" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9370" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9473" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9500" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9503" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9522" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9530" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "9581" ~ 31,
      {{site_var}} %in% site_values & {{hist_var}} == "8000" ~ 41,
      {{site_var}} %in% site_values & {{hist_var}} == "8001" ~ 41,
      {{site_var}} %in% site_values & {{hist_var}} == "8002" ~ 41,
      {{site_var}} %in% site_values & {{hist_var}} == "8003" ~ 41,
      {{site_var}} %in% site_values & {{hist_var}} == "8004" ~ 41,
      {{site_var}} %in% site_values & {{hist_var}} == "8005" ~ 41,
      {{site_var}} %in% site_values & {{hist_var}} == "9050" ~ 91,
      {{site_var}} %in% site_values & {{hist_var}} == "8933" ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8940" ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "8963" ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9180" ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9220" ~ 92,
      {{site_var}} %in% site_values & {{hist_var}} == "9260" ~ 92,
      {{site_var}} %in% site_values & !(as.numeric({{hist_var}}) %in% valid) ~ 92,
      na_to_unspecified == TRUE & {{site_var}} %in% site_values ~ 6, #enforce option na_to_unspecific
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(new_var = "Subsite of lung cancer IARC groups") %>%
    sjlabelled::set_labels(new_var, labels = c("Squamous cell carcinoma" = 11,
                                               "Adenocarcinoma" = 12,
                                               "Small cell carcinoma"	= 13,
                                               "Large cell carcinoma" = 14,
                                               "Other specified carcinoma (incl Carcinoid)" = 15,
                                               "Sarcoma" = 21,
                                               "Other specified malignant neoplasm" = 31,
                                               "Unspecified" = 41,
                                               "Excluded" = 91,
                                               "Unusual" = 92
                                               )) %>%
    tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    tidytable::rename({{new_var_subsite}} := new_var)
}

cr_subsiteiarcgr_lung <- function(df, subsite_var = t_sublungiarc, new_var_subsite = t_sublungiarcgroup){
  #group small categories of t_sublungiarc
  
  df %>%
    tidytable::mutate(new_var := tidytable::case_when(
      {{subsite_var}} == "Squamous cell carcinoma" ~ 1,
      {{subsite_var}} == "Adenocarcinoma" ~ 2,
      {{subsite_var}} == "Small cell carcinoma" ~ 3,
      {{subsite_var}} == "Large cell carcinoma" ~ 4,
      {{subsite_var}} == "Other specified carcinoma (incl Carcinoid)" ~ 5,
      {{subsite_var}} == "Sarcoma" ~ 5,
      {{subsite_var}} == "Other specified malignant neoplasm" ~ 5,
      {{subsite_var}} == "Unspecified" ~ 5,
      {{subsite_var}} == "Excluded" ~ 91,
      {{subsite_var}} == "Unusual" ~ 92,
      .default = NA )) %>%
    sjlabelled::var_labels(new_var = "Subsite of lung cancer IARC groups (grouped)") %>%
    sjlabelled::set_labels(new_var, labels = c("Squamous cell carcinoma (SCC)" = 1,
                                               "Adenocarcinoma (AC)" = 2,
                                               "Small cell carcinoma (SCLC)"	= 3,
                                               "Large cell carcinoma (LCC)" = 4,
                                               "Other & unspecified (O&U)" = 5,
                                               "Excluded" = 91,
                                               "Unusual" = 92
    )) %>%
    tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    tidytable::rename({{new_var_subsite}} := new_var)
}


cr_histgroupiarc <- function(df, hist_var, new_var_hist = t_histgroupiarc){
  
  #Coding table: for definition see SPN - Cancer Coding Systems_V1.0_20200217.xlsx, sheet 2a_Histology IARC
  #Histologically different groups 
  #Source: Table 25 Fritz AG, Percy C, Jack A, Shanmugaratnam K, Sobin L, Parkin DM, et al., 
  #        editors. International classification of diseases for oncology: ICD-O [Internet]. 
  #        First Revision. Geneva: World Health Organization; 2013 [cited 2019 Aug 14]. 
  #          Available from: http://public.eblib.com/choice/publicfullrecord.aspx?p=1681147
  
  
  df %>%
    tidytable::mutate(new_var := tidytable::case_when(
      as.numeric({{hist_var}}) >= 8051 & as.numeric({{hist_var}}) <= 8084 ~ 1,
      as.numeric({{hist_var}}) >= 8120 & as.numeric({{hist_var}}) <= 8131 ~ 1,
      as.numeric({{hist_var}}) >= 8090 & as.numeric({{hist_var}}) <= 8110 ~ 2,
      as.numeric({{hist_var}}) >= 8140 & as.numeric({{hist_var}}) <= 8149 ~ 3,
      as.numeric({{hist_var}}) >= 8160 & as.numeric({{hist_var}}) <= 8162 ~ 3,
      as.numeric({{hist_var}}) >= 8190 & as.numeric({{hist_var}}) <= 8221 ~ 3,
      as.numeric({{hist_var}}) >= 8260 & as.numeric({{hist_var}}) <= 8337 ~ 3,
      as.numeric({{hist_var}}) >= 8350 & as.numeric({{hist_var}}) <= 8551 ~ 3,
      as.numeric({{hist_var}}) >= 8570 & as.numeric({{hist_var}}) <= 8576 ~ 3,
      as.numeric({{hist_var}}) >= 8940 & as.numeric({{hist_var}}) <= 8941 ~ 3,
      as.numeric({{hist_var}}) >= 8030 & as.numeric({{hist_var}}) <= 8046 ~ 4,
      as.numeric({{hist_var}}) >= 8150 & as.numeric({{hist_var}}) <= 8157 ~ 4,
      as.numeric({{hist_var}}) >= 8170 & as.numeric({{hist_var}}) <= 8180 ~ 4,
      as.numeric({{hist_var}}) >= 8230 & as.numeric({{hist_var}}) <= 8255 ~ 4,
      as.numeric({{hist_var}}) >= 8340 & as.numeric({{hist_var}}) <= 8347 ~ 4,
      as.numeric({{hist_var}}) >= 8560 & as.numeric({{hist_var}}) <= 8562 ~ 4,
      as.numeric({{hist_var}}) >= 8580 & as.numeric({{hist_var}}) <= 8671 ~ 4,
      as.numeric({{hist_var}}) >= 8010 & as.numeric({{hist_var}}) <= 8015 ~ 5,
      as.numeric({{hist_var}}) >= 8020 & as.numeric({{hist_var}}) <= 8022 ~ 5,
      as.numeric({{hist_var}}) >= 8050 & as.numeric({{hist_var}}) <= 8050 ~ 5,
      as.numeric({{hist_var}}) >= 8680 & as.numeric({{hist_var}}) <= 8713 ~ 6,
      as.numeric({{hist_var}}) >= 8800 & as.numeric({{hist_var}}) <= 8921 ~ 6,
      as.numeric({{hist_var}}) >= 8990 & as.numeric({{hist_var}}) <= 8991 ~ 6,
      as.numeric({{hist_var}}) >= 9040 & as.numeric({{hist_var}}) <= 9044 ~ 6,
      as.numeric({{hist_var}}) >= 9120 & as.numeric({{hist_var}}) <= 9125 ~ 6,
      as.numeric({{hist_var}}) >= 9130 & as.numeric({{hist_var}}) <= 9136 ~ 6,
      as.numeric({{hist_var}}) >= 9141 & as.numeric({{hist_var}}) <= 9252 ~ 6,
      as.numeric({{hist_var}}) >= 9370 & as.numeric({{hist_var}}) <= 9373 ~ 6,
      as.numeric({{hist_var}}) >= 9540 & as.numeric({{hist_var}}) <= 9582 ~ 6,
      as.numeric({{hist_var}}) >= 9050 & as.numeric({{hist_var}}) <= 9055 ~ 7,
      as.numeric({{hist_var}}) >= 9840 & as.numeric({{hist_var}}) <= 9840 ~ 8,
      as.numeric({{hist_var}}) >= 9861 & as.numeric({{hist_var}}) <= 9931 ~ 8,
      as.numeric({{hist_var}}) >= 9945 & as.numeric({{hist_var}}) <= 9946 ~ 8,
      as.numeric({{hist_var}}) >= 9950 & as.numeric({{hist_var}}) <= 9950 ~ 8,
      as.numeric({{hist_var}}) >= 9961 & as.numeric({{hist_var}}) <= 9964 ~ 8,
      as.numeric({{hist_var}}) >= 9965 & as.numeric({{hist_var}}) <= 9967 ~ 8, #new coding
      as.numeric({{hist_var}}) >= 9980 & as.numeric({{hist_var}}) <= 9987 ~ 8,
      as.numeric({{hist_var}}) >= 9992 & as.numeric({{hist_var}}) <= 9992 ~ 8, #new coding
      as.numeric({{hist_var}}) >= 9670 & as.numeric({{hist_var}}) <= 9699 ~ 9,
      as.numeric({{hist_var}}) >= 9728 & as.numeric({{hist_var}}) <= 9728 ~ 9,
      as.numeric({{hist_var}}) >= 9731 & as.numeric({{hist_var}}) <= 9734 ~ 9,
      as.numeric({{hist_var}}) >= 9735 & as.numeric({{hist_var}}) <= 9738 ~ 9,
      as.numeric({{hist_var}}) >= 9761 & as.numeric({{hist_var}}) <= 9767 ~ 9,
      as.numeric({{hist_var}}) >= 9769 & as.numeric({{hist_var}}) <= 9769 ~ 9,
      as.numeric({{hist_var}}) >= 9823 & as.numeric({{hist_var}}) <= 9826 ~ 9,
      as.numeric({{hist_var}}) >= 9833 & as.numeric({{hist_var}}) <= 9833 ~ 9,
      as.numeric({{hist_var}}) >= 9836 & as.numeric({{hist_var}}) <= 9836 ~ 9,
      as.numeric({{hist_var}}) >= 9940 & as.numeric({{hist_var}}) <= 9940 ~ 9,
      as.numeric({{hist_var}}) >= 9700 & as.numeric({{hist_var}}) <= 9719 ~ 10,
      as.numeric({{hist_var}}) >= 9726 & as.numeric({{hist_var}}) <= 9726 ~ 10,
      as.numeric({{hist_var}}) >= 9729 & as.numeric({{hist_var}}) <= 9729 ~ 10,
      as.numeric({{hist_var}}) >= 9768 & as.numeric({{hist_var}}) <= 9768 ~ 10,
      as.numeric({{hist_var}}) >= 9827 & as.numeric({{hist_var}}) <= 9831 ~ 10,
      as.numeric({{hist_var}}) >= 9834 & as.numeric({{hist_var}}) <= 9834 ~ 10,
      as.numeric({{hist_var}}) >= 9837 & as.numeric({{hist_var}}) <= 9837 ~ 10,
      as.numeric({{hist_var}}) >= 9948 & as.numeric({{hist_var}}) <= 9948 ~ 10,
      as.numeric({{hist_var}}) >= 9650 & as.numeric({{hist_var}}) <= 9667 ~ 11,
      as.numeric({{hist_var}}) >= 9740 & as.numeric({{hist_var}}) <= 9742 ~ 12,
      as.numeric({{hist_var}}) >= 9750 & as.numeric({{hist_var}}) <= 9758 ~ 13,
      as.numeric({{hist_var}}) >= 9590 & as.numeric({{hist_var}}) <= 9591 ~ 14,
      as.numeric({{hist_var}}) >= 9596 & as.numeric({{hist_var}}) <= 9596 ~ 14,
      as.numeric({{hist_var}}) >= 9597 & as.numeric({{hist_var}}) <= 9597 ~ 14,
      as.numeric({{hist_var}}) >= 9727 & as.numeric({{hist_var}}) <= 9727 ~ 14,
      as.numeric({{hist_var}}) >= 9760 & as.numeric({{hist_var}}) <= 9760 ~ 14,
      as.numeric({{hist_var}}) >= 9800 & as.numeric({{hist_var}}) <= 9801 ~ 14,
      as.numeric({{hist_var}}) >= 9805 & as.numeric({{hist_var}}) <= 9805 ~ 14,
      as.numeric({{hist_var}}) >= 9806 & as.numeric({{hist_var}}) <= 9809 ~ 14, #new coding
      as.numeric({{hist_var}}) >= 9811 & as.numeric({{hist_var}}) <= 9816 ~ 14, #new coding
      as.numeric({{hist_var}}) >= 9820 & as.numeric({{hist_var}}) <= 9820 ~ 14,
      as.numeric({{hist_var}}) >= 9832 & as.numeric({{hist_var}}) <= 9832 ~ 14,
      as.numeric({{hist_var}}) >= 9835 & as.numeric({{hist_var}}) <= 9835 ~ 14,
      as.numeric({{hist_var}}) >= 9860 & as.numeric({{hist_var}}) <= 9860 ~ 14,
      as.numeric({{hist_var}}) >= 9960 & as.numeric({{hist_var}}) <= 9960 ~ 14,
      as.numeric({{hist_var}}) >= 9970 & as.numeric({{hist_var}}) <= 9970 ~ 14,
      as.numeric({{hist_var}}) >= 9971 & as.numeric({{hist_var}}) <= 9971 ~ 14, #new coding
      as.numeric({{hist_var}}) >= 9975 & as.numeric({{hist_var}}) <= 9975 ~ 14,
      as.numeric({{hist_var}}) >= 9989 & as.numeric({{hist_var}}) <= 9989 ~ 14,
      as.numeric({{hist_var}}) >= 9140 & as.numeric({{hist_var}}) <= 9140 ~ 15,
      as.numeric({{hist_var}}) >= 8720 & as.numeric({{hist_var}}) <= 8790 ~ 16,
      as.numeric({{hist_var}}) >= 8930 & as.numeric({{hist_var}}) <= 8936 ~ 16,
      as.numeric({{hist_var}}) >= 8950 & as.numeric({{hist_var}}) <= 8983 ~ 16,
      as.numeric({{hist_var}}) >= 9000 & as.numeric({{hist_var}}) <= 9030 ~ 16,
      as.numeric({{hist_var}}) >= 9060 & as.numeric({{hist_var}}) <= 9110 ~ 16,
      as.numeric({{hist_var}}) >= 9260 & as.numeric({{hist_var}}) <= 9365 ~ 16,
      as.numeric({{hist_var}}) >= 9380 & as.numeric({{hist_var}}) <= 9539 ~ 16,
      as.numeric({{hist_var}}) >= 8000 & as.numeric({{hist_var}}) <= 8005 ~ 17,
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(new_var = "IARC Histology groups (Morphology ICD-O-3 based recoding IARC 'histologically different' groups)") %>%
    sjlabelled::set_labels(new_var, labels = c("Squamous carcinomas" = 1,
                                               "Basal cell carcinomas" = 2,
                                               "Adenocarcinomas" = 3,
                                               "Other specific carcinomas" = 4,
                                               "Unspecified carcinomas (NOS)" = 5,
                                               "Sarcomas and soft tissue tumours" = 6,
                                               "Mesothelioma" = 7,
                                               "Myeloid" = 8,
                                               "B-cell neoplasms" = 9,
                                               "T-cell and NK-cell neoplasms" = 10,
                                               "Hodgkin lymphoma" = 11,
                                               "Mast-cell Tumours" = 12,
                                               "Histiocytes and Accessory Lymphoid cells" = 13,
                                               "Unspecified haematopoietic cancers" = 14,
                                               "Kaposi sarcoma" = 15,
                                               "Other specified types of cancer" = 16,
                                               "Unspecified types of cancer" = 17)) %>%
    tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    tidytable::rename({{new_var_hist}} := new_var)
}

cr_unusualhist_lung <- function(df, hist_var, site_var, site_values, new_var_unusual = t_unusuallung){
  
  #codes valid according to  SEER ICD-O-3 Site/Histology Validation List 2022
  seer_valid_codes <- c(8000, 8001, 8002, 8003, 8004, 8005, 8010, 8010, 8011, 8012, 8013, 8014, 8015, 8020, 8021, 8022, 8023, 
                        8030, 8031, 8032, 8033, 8034, 8035, 8046, 8041, 8042, 8043, 8044, 8045, 8050, 8050, 8051, 8052, 8052, 
                        8070, 8070, 8071, 8072, 8073, 8074, 8075, 8076, 8076, 8078, 8083, 8120, 8120, 8121, 8122, 8123, 8124, 
                        8140, 8140, 8141, 8143, 8147, 8200, 8201, 8201, 8230, 8230, 8231, 8240, 8241, 8242, 8243, 8244, 8245, 
                        8246, 8249, 8250, 8250, 8251, 8252, 8253, 8253, 8254, 8255, 8256, 8257, 8260, 8265, 8310, 8320, 8323, 
                        8333, 8430, 8480, 8481, 8490, 8510, 8550, 8551, 8560, 8562, 8570, 8571, 8572, 8573, 8574, 8575, 8576, 
                        8714, 8800, 8801, 8802, 8803, 8804, 8805, 8806, 8810, 8811, 8813, 8814, 8815, 8825, 8830, 8842, 8890, 
                        8891, 8894, 8895, 8896, 8900, 8901, 8902, 8910, 8912, 8972, 8973, 8980, 8981, 8982, 8990, 8991, 9050, 
                        9050, 9051, 9052, 9053, 9120, 9133, 9137, 9140, 9174, 9590, 9591, 9596, 9650, 9651, 9652, 9653, 9654, 
                        9655, 9659, 9661, 9662, 9663, 9664, 9665, 9667, 9670, 9671, 9673, 9675, 9678, 9679, 9680, 9684, 9687, 
                        9688, 9690, 9691, 9695, 9698, 9699, 9701, 9702, 9705, 9712, 9714, 9715, 9719, 9724, 9727, 9728, 9729, 
                        9731, 9734, 9735, 9737, 9738, 9740, 9741, 9749, 9750, 9751, 9754, 9755, 9756, 9757, 9758, 9759, 9766, 
                        9811, 9812, 9813, 9814, 9815, 9816, 9817, 9818, 9819, 9823, 9831, 9837, 9965, 9967, 9971, 9975)
  
  df %>%
    tidytable::mutate(new_var := tidytable::case_when(
      {{site_var}} %in% site_values & as.numeric({{hist_var}} %in% seer_valid_codes) ~ 1,
      {{site_var}} %in% site_values & !(as.numeric({{hist_var}}) %in% seer_valid_codes) ~ 0,
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(new_var = "Unusual Morphology Code for Lung Cancer (according to SEER ICD-O-3 Site/Histology Validation List 2022)") %>%
    sjlabelled::set_labels(new_var, labels = c("Lung cancer - unusual" = 0,
                                               "Lung cancer - valid histology" = 1)
                                               ) %>%
    tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    tidytable::rename({{new_var_unusual}} := new_var)
}

## -------- Visualization -------------

#plot_sir_byfutime: Create new ggplots for figures in spc_lung publication

plot_sir_byfutime2 <- function(results_df, sites_to_plot, facet_vars = t_smokeiarc ~ t_site, timecats_to_plot = c("6-12 months", "1-5 years", "5-10 years", "Total"), 
                              colors_sex = c("Male" = "blue", "Female" = "red"), y_lim = NA, vlab_x_off = -0.5, vlab_y_pos = 0.5, vlab_y_diff = .1,
                              ylab = element_blank(), xlab = element_blank()){
  
  n_sites <- length(sites_to_plot)
  
  pd <- position_dodge(-0.35)
  
  #enforce y_lim and adjust upper CI
  if(!is.na(y_lim)){
    results_df <- results_df %>% 
      mutate(sir_uci = case_when(sir_uci > y_lim ~ y_lim,
                                 TRUE ~ sir_uci))
  }
  
  
  df <- results_df %>% 
    #filter for most frequent sites
    filter(t_site %in% sites_to_plot) %>%
    #for small case numbers,create flag 
    mutate(fewcases = case_when(expected >= 5 ~ "many",
                                TRUE ~ "few"),
           sir = case_when(expected >= 5 ~ sir,
                           TRUE ~ 0.3),
           sir_lci = case_when(expected >= 5 & sir_lci > 0.3 ~ sir_lci,
                               TRUE ~ 0.3),
           sir_uci = case_when(expected >= 5 ~ sir_uci,
                               TRUE ~ 0.3)) %>%
    #rename Total fu_time to make label shorter
    mutate(fu_time = recode(fu_time, 
                            "Total 0.5 to Inf years" = "Total",
                            "Total 0 to Inf years" = "Total"
    )) %>%
    #rename Total t_site
    mutate(t_site := recode(t_site, 
                            "smoking-related IARC" = "Smoking-related cancers*",
                            "other cancer" = "Other cancers**",
                            "Non-Hodgkin lymphoma (C82-C88)" = "Non-Hodgkin lymph. (C82-88)"
    ))
  #start ggplot
  df %>%
    ggplot(data =., aes(x=fu_time, y=sir, colour=sex, group=sex, shape = fewcases)) + 
    geom_errorbar(aes(ymin=sir_lci, ymax=sir_uci), colour="black", width=.2, position=pd) +
    geom_point(position=pd, size=3, stroke=1.3) + 
    #add overall SIR for males
    geom_text(
      aes(x=fu_time, y=vlab_y_pos, colour=sex, group=sex,
          label=ifelse(
            (fu_time == "Total" & sex == "Male" & sir!=0.3), paste0("Overall SIR Males: ", format(sir, nsmall = 2), " (", format(sir_lci, nsmall = 2), "–", format(sir_uci, nsmall = 2), ")"), #paste sir with ci
            "")),                                                                                                                                                       #or nothing for few_cases
      nudge_x = vlab_x_off, size = 4, hjust = 1) +
    #add overall SIR for females
    geom_text(
      aes(x=fu_time, y=vlab_y_pos-vlab_y_diff, colour=sex, group=sex,
          label=ifelse(
            (fu_time == "Total" & sex == "Female" & sir!=0.3), paste0("Overall SIR Females: ", format(sir, nsmall = 2), " (", format(sir_lci, nsmall = 2), "–", format(sir_uci, nsmall = 2), ")"), 
            "")),
      nudge_x = vlab_x_off, size = 4, hjust = 1) +
    #define estimate categories (shape)
    scale_shape_manual(name="Estimate", 
                       values = c(many = 21, few = 4),                     # 21 is filled circle
                       breaks=c("many", "few"), 
                       labels=c("SIR", "no estimate (<5 expected cases)")) + 
    #define sex categories (group)
    scale_colour_manual(name="Sex",    # Legend label, use darker colors,
                        #breaks = c("Male", "Female"),
                        #labels = c("Male", "Female"),
                        values = colors_sex) +                     
    geom_hline(yintercept = 1) +
    #y axis - make log scale (current scaling depends on minimum value set above for fewcases = few --> sir == 0.3)
    scale_y_log10(limits = c(0.3, y_lim)) +
    #facetting
    facet_grid({{facet_vars}}) +
    #x axis - select wanted categories for x-axis
    xlim(timecats_to_plot) +
    guides(x = guide_axis(n.dodge = 2)) +
    theme_bw() + 
    theme(legend.position="none",
          axis.title.x = xlab,
          axis.title.y = ylab)
  
  
}


#custom dotplot

plot_sir_dotplot <- function(val1, val2, col1, col2, x_lim = 10, 
                             plot_width = 70,
                             line_width = 2, show_nulleffect = TRUE
                             ) {
  
  #this function is based on row values only and creates a dummy tibble from it to plot
  dat <- dplyr::tibble(
    val1 = val1,
    val2 = val2,
    change = val1 - val2,
    y = rep(1)
  )
  
  #set width of standard line
  lw <- line_width
  
  p <- ggplot() +
    geom_segment(
      data = dat,
      aes(y = y, yend = y, x = val1, xend = val2),
      color = 'grey60',
      size = lw
    ) +
    geom_point(
      data = dat,
      aes(x = val1, y = y), 
      color = col1,
      size = lw*3
    ) +
    geom_point(
      data = dat,
      aes(x = val2, y = y), 
      color = col2,
      size = lw*3
    ) +
    scale_x_log10(limits = c(0.3, x_lim)) +
    theme_void()
  
  #draw line of nulleffect
  if(show_nulleffect == TRUE){
    p <- p + 
      geom_vline(xintercept = 1, linewidth = lw/2) 
  }
  
  #process if used inside gt table
  if(use_in_gt == TRUE){
    
    out_name <- file.path(tempfile(
      pattern = "file",
      tmpdir = tempdir(),
      fileext = ".svg"
    ))
    
    ggsave(
      out_name,
      plot = plot_out,
      dpi = 25.4,
      height = 5,
      width = plot.width,
      units = "mm",
      device = "svg"
      )
    
    p <- readLines(out_name) %>%
      paste0(collapse = "") %>%
      gt::html()
    
    on.exit(file.remove(out_name), add = TRUE)
  }
  
  p
}

#modification of gtExtras::gt_plt_dumbbell

#' Add a dumbbell plot in place of two columns
#'
#' @param gt_object an existing gt_tbl or pipeline
#' @param var1 column 1, plot will replace this column
#' @param var2 column 2, will be hidden
#' @param label an optional new label for the transformed column
#' @param palette must be 3 colors in order of col1, col2, bar color
#' @param width width in mm, defaults to 70
#' @param text_args A list of named arguments. Optional text arguments passed as a list to `scales::label_number`.
#' @param text_size A number indicating the size of the text indicators in the plot. Defaults to 1.5. Can also be set to `0` to "remove" the text itself.
#'
#' @return a gt_object table
#' @export
#'
#' @section Examples:
#' ```r
#' head(mtcars) %>%
#'   gt() %>%
#'   gt_plt_dumbbell(disp, mpg)
#' ```
#' @section Figures:
#' \if{html}{\figure{gt_plt_dumbell.png}{options: width=70\%}}
plot_gt_sircomp_dotplot <- function(
    gt_object,
    var1 = NULL,
    var2 = NULL,
    var3 = NULL,
    label = NULL,
    col1 = "#5AC997",
    col2 = "#833C97",
    col3 = "#CDEFE0",
    label_x1 = NULL,
    label_x2 = NULL,
    label_x3 = NULL,
    col_line = "lightgrey",
    width = 70,
    text_args = list(accuracy = 1),
    text_size = 2.5,
    x_min = 0.1,
    x_max = 10, 
    line_width = 1, 
    show_nulleffect = TRUE
) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  # extract the values from specified columns
  df_in <- gtExtras::gt_index(gt_object, {{ var1 }}, as_vector = FALSE) %>%
    dplyr::select(x1 = {{ var1 }}, x2 = {{ var2 }}, x3 = {{ var3 }})
  
  if (length(df) == 0) {
    return(gt_object)
  }
  
  #set width of standard line
  lw <- line_width
  
  all_vals <- df_in %>%
    unlist() %>%
    as.vector()
  
  rng_val <- range(all_vals, na.rm = TRUE)
  
  tab_out <- gt_object %>%
    text_transform(
      locations = cells_body({{ var1 }}),
      fn = function(x) {
        dumbbell_fx <- function(col1_vals, col2_vals, col3_vals, text_args, text_size) {
          all_df_in_vals <- c(col1_vals, col2_vals, col3_vals)
          
          if (any(is.na(all_df_in_vals)) | any(is.null(all_df_in_vals))) {
            return("<div></div>")
          }
          
          df_vals <- dplyr::tibble(x1 = col1_vals, x2 = col2_vals, x3 = col3_vals)
          
          # TODO: revisit horizontal adjustment
          hjust_val <- ifelse(col1_vals >= col2_vals, list(1,0), list(0,1))
          
          plot_obj <- ggplot(df_vals, aes(y = "y1")) +
            geom_segment(
              aes(x = x1, xend = x2, yend = "y1"),
              linewidth = lw,
              color = col_line
            ) +
            geom_point(
              aes(x = x1),
              color = "white",
              pch = 21,
              fill = col1,
              size = lw*3,
              stroke = 1.25
            ) +
            geom_point(
              aes(x = x2),
              color = "white",
              pch = 21,
              fill = col2,
              size = lw*3,
              stroke = 1.25
            ) +
            geom_text(
              aes(
                x = x1, y = 1.05,
                label = ifelse(is.null({{label_x1}}), x1, {{label_x1}})
              ),
              # TODO: revisit horizontal adjustment
              # hjust = hjust_val[[1]],
              family = "mono",
              color = col1,
              size = text_size,
              
            ) +
            geom_text(
              aes(
                x = x2, y = 1.05,
                label = ifelse(is.null({{label_x2}}), x2, {{label_x2}})
              ),
              # TODO: revisit horizontal adjustment
              # hjust = hjust_val[[2]],
              family = "mono",
              color = col2,
              size = text_size
            ) +
            #coord_cartesian(xlim = rng_val) +
            scale_x_log10(limits = c(x_min, x_max), expand = expansion(mult = c(0.1, 0.1))) +
            scale_y_discrete(expand = expansion(mult = c(0.03, 0.095))) +
            theme(
              legend.position = "none",
              plot.margin = margin(0, 0, 0, 0, "pt"),
              plot.background = element_blank(),
              panel.background = element_blank()
            ) +
            theme_void()
          
          #only plot 3rd value, if provided
          if(!is.null(col3_vals)){
          plot_obj <- plot_obj +
            geom_point(
              aes(x = x3),
              color = "white",
              pch = 21,
              fill = col3,
              size = lw*3,
              stroke = 1.25
            ) +
            geom_text(
              aes(
                x = x3, y = 1.05,
                label = ifelse(is.null({{label_x3}}), x3, {{label_x3}})
              ),
              # TODO: revisit horizontal adjustment
              # hjust = hjust_val[[2]],
              family = "mono",
              color = col3,
              size = text_size
            )
          }
          
          #draw line of nulleffect
          if(show_nulleffect == TRUE){
            plot_obj <- plot_obj + 
              geom_vline(xintercept = 1, linewidth = lw/2, color = "black") 
          }
          
         gtExtras:::save_svg(plot_obj, height = 7, width = width, units = "mm")
        }
        
        tab_built <- mapply(
          dumbbell_fx,
          df_in$x1,
          df_in$x2,
          df_in$x3,
          list(text_args),
          text_size,
          SIMPLIFY = FALSE
        )
        tab_built
      }
    ) %>%
    gt::cols_align(align = "left", columns = {{ var1 }})
  
  if(!is.null(label)){
    
    return(
      tab_out %>%
        cols_label({{ var1 }} := label)
    )
  }
  
  tab_out
  
  
}




## -------- Tabulation   -------------

## -------- Calculations -------------

# helper function to calc_refrates to split by additional variable

calc_refrates_byvar <- function(val, var, df, ...){
  
  print(paste("Calculating rates for ", rlang::as_label(rlang::enquo(var)), "==", val))
  
  df %>%
    tidytable::filter({{var}} == val) %>%
    msSPChelpR::calc_refrates(...) %>%
    tidytable::mutate({{var}} := val)
}

# helper function to calculate overall sir

calc_overall_sir <- function(df, refrates, summarize_groups = c("region", "age", "year"), ...){
  
  all_summary_vars <- c("sex","region", "age", "year")
  vars_not_summarized <- all_summary_vars[!(all_summary_vars %in% summarize_groups)]
  
  df %>%
    #count all SPC in dataset; tvar only as workaround since ybreak_var is needed
    tidytable::mutate(
      tvar = 1,
      count_spc = tidytable::case_when(p_spc.1 == "SPC developed" ~ 1,
                                        TRUE                       ~ 0)) %>%
    #calculate SIR
    msSPChelpR::sir_byfutime(.,
                             dattype = NULL,
                             ybreak_vars = c("tvar"),
                             xbreak_var = "none",
                             futime_breaks = c(0, 0.5, 1, 5, 10, Inf),
                             count_var = "count_spc",
                             refrates_df = refrates,
                             calc_total_row = TRUE,
                             calc_total_fu = TRUE,
                             region_var = "p_region.1",
                             age_var = "t_agegroupdiag.1",
                             sex_var = "p_sex.1",
                             year_var = "t_yeardiag.1",
                             race_var = NULL,
                             site_var = "t_sitewhogen.2",
                             futime_var = "p_futimeyrs.1",
                             alpha = 0.05) %>%
    #summarize SIR
    tidytable::filter(!is.na(ref_population_pyar)) %>% 
    msSPChelpR::summarize_sir_results(.,
                                      summarize_groups = summarize_groups,
                                      summarize_site = FALSE,
                                      output = "long",  output_information = "reduced",
                                      add_total_row = "start",  add_total_fu = "start",
                                      shorten_total_cols = TRUE,
                                      fubreak_var_name = "fu_time", ybreak_var_name = "yvar_name",
                                      xbreak_var_name = "none", site_var_name = "t_site",
                                      alpha = 0.05
    ) %>%
    #extract SIR, LCI, UCI
    tidytable::filter(yvar_name == "total_var" & str_detect(fu_time, "^Total")) %>%
    tidytable::select(tidyselect::any_of(vars_not_summarized), t_site, sir, sir_lci, sir_uci)
  
}

# helper function to calculate overall sir depending on length of FU
calc_relative_futime <- function(df, start_date_var, interval_yrs){
  
  #get number of digits
  dig <- interval_yrs %>% max(.) %>% log10(.) %>% {floor(.) + 1}
  
  #set-up progress bar
  #cli::cli_progress_step("Splitting data")
  
  
  ## split data by start_date_var
  df_n <- df %>%
    tidytable::group_split({{start_date_var}}, .named = TRUE)
  
  #1) calc pat_status for a certain date
  
  i <- 0
  #cli::cli_progress_step("Calculate new p_status and p_futimeyrs variables. Got {i}/{length(df_n)} groups.")
  for(i in cli::cli_progress_along(seq_along(df_n), "Calculating Status and FU")){  
    
    for(yr in seq_along(interval_yrs)){
      
      fu_end_str <- {as.Date(names(df_n[i])) + lubridate::years(interval_yrs[yr])} %>% as.character()
      status_var_name <- sprintf("p_status_yr%02d", interval_yrs[yr])
      futime_var_name <- sprintf("p_futime_yr%02d", interval_yrs[yr])
      
      df_n[[i]] <- df_n[[i]] %>%
        msSPChelpR::pat_status_tt(
          wide_df = ., fu_end = fu_end_str, dattype = NULL, status_var = !!status_var_name,
          life_var = "p_dead.1", spc_var = "p_spc.1", birthdat_var = "p_datebirth.1", lifedat_var = "p_datedeath.1",
          lifedatmin_var = "p_dodmin.1", fcdat_var = "t_datediag.1", spcdat_var = "t_datediag.2",
          life_stat_alive = "alive", life_stat_dead = "dead", spc_stat_yes = "SPC developed",
          spc_stat_no = "No SPC", lifedat_fu_end = "2017-12-15", use_lifedatmin = TRUE,
          check = FALSE, as_labelled_factor = TRUE
        ) %>%
        msSPChelpR::calc_futime_tt(
          ., futime_var_new = !!futime_var_name, fu_end = fu_end_str,
          dattype = NULL, time_unit = "years", 
          status_var = !!status_var_name, lifedat_var = "p_datedeath.1", 
          fcdat_var = "t_datediag.1", spcdat_var = "t_datediag.2",
          check = FALSE, quiet = TRUE
        )
      
      
    }# end loop for multiple interval_yrs provided
    
    #cli::cli_progress_update()
    
    
  } #end loop for start_date_var (split by registries)
  
  tidytable::bind_rows(df_n)
  
}

# helper function to prepare data for multiple parallel sir calculations
prep_sir_nested_df <- function(df, keep_vars = c("")){
  
  cli::cli_alert_info("Prepare data - nested df")
  
  df %>%
    #only keep variables relevant for SIR calculation
    tidytable::select(
      tidyselect::any_of(c("p_id", "p_sex.1", "p_region.1", "t_agegroupdiag.1",
                           "t_yeardiag.1", "t_sitewhogen.2", "p_race.1",
                           "t_agediag.1", "t_singleyeardiag.1")),
      tidyselect::starts_with("p_status_yr"),
      tidyselect::starts_with("p_futime_yr"),
      tidyselect::any_of(keep_vars)) %>%
    #reshape longer
    tidytable::pivot_longer(
      cols = c(tidyselect::starts_with("p_status_yr"),
               tidyselect::starts_with("p_futime_yr")),
      names_to = c(".value", "yr"),
      names_pattern = "(.*)_yr(.*)"
    ) %>%
    #remove cases with irrelevant p_status
    tidytable::filter(!p_status %in% c(
      "NA - Patient not born before end of FU",
      "NA - Patient did not develop cancer before end of FU"
    )) %>%
    #calculate count_var -> spc developed
    tidytable::mutate(
      tvar = 1,
      count_spc = tidytable::case_when(
        .data$p_status %in% c("Patient alive after SPC", "Patient dead after SPC") ~ 1,
        .data$p_status %in% c("Patient alive after FC (with or without following SPC after end of FU)", 
                              "Patient dead after FC") ~ 0,
        TRUE ~ NA_real_
      )) %>%
    tidytable::mutate(
      n_fc = tidytable::n(),
      n_spc = sum(.data$count_spc, na.rm = TRUE),
      .by = yr
    ) %>%
    tidytable::nest_by(c(yr, n_fc, n_spc), .keep = FALSE)
  
} 


# helper function to calculate overall sir depending on length of FU
calc_sir_nested_df <- function(df_n, refrates, ybreak_vars = c("tvar"), 
                               futime_breaks = c(0, 0.5, 1, 5, 10, Inf), fu_sum_filter = "^Total",
                               region_var = "p_region.1",
                               age_var = "t_agegroupdiag.1",
                               sex_var = "p_sex.1",
                               year_var = "t_yeardiag.1",
                               race_var = NULL,
                               site_var = "t_sitewhogen.2",
                               futime_var = "p_futime",
                               quiet = FALSE){
  if(ybreak_vars[1] == "tvar"){
    ybreak_sum_vars <- "total_var"
  }else{
    ybreak_sum_vars <- ybreak_vars
  }

  df_n %>% 
    #calculate SIR
    tidytable::mutate(sir = tidytable::map(
      data, ~ msSPChelpR::sir_byfutime(.,
                                       dattype = NULL,
                                       ybreak_vars = ybreak_vars,
                                       xbreak_var = "none",
                                       futime_breaks = futime_breaks,
                                       count_var = "count_spc",
                                       refrates_df = refrates,
                                       calc_total_row = TRUE,
                                       calc_total_fu = TRUE,
                                       region_var = region_var,
                                       age_var = age_var,
                                       sex_var = sex_var,
                                       year_var = year_var,
                                       race_var = race_var,
                                       site_var = site_var,
                                       futime_var = futime_var,
                                       alpha = 0.05,
                                       quiet = quiet))) %>%
    tidytable::mutate(sir = tidytable::map(sir, ~ tidytable::filter(., !is.na(.data$ref_population_pyar)))) %>%
    tidytable::mutate(sir_sum = tidytable::map(
      sir, ~  msSPChelpR::summarize_sir_results(.,
                                                summarize_groups = c("region", "age", "year"),
                                                summarize_site = FALSE,
                                                output = "long",  output_information = "reduced",
                                                add_total_row = "start",  add_total_fu = "start",
                                                shorten_total_cols = TRUE,
                                                fubreak_var_name = "fu_time", ybreak_var_name = "yvar_name",
                                                xbreak_var_name = "none", site_var_name = "t_site",
                                                alpha = 0.05
      ))) %>%
    #extract SIR, LCI, UCI
    tidytable::mutate(sir_sum = tidytable::map(
      sir_sum, ~ tidytable::filter(., .data$yvar_name %in% c(ybreak_sum_vars) & str_detect(.data$fu_time, fu_sum_filter)) %>%
        tidytable::select(sex, t_site, fu_time, yvar_label, sir, sir_lci, sir_uci, expected, observed)
    ))
}




### challenges: if using relative fu_length, each register will have a different fu_end date
### what variables need to be calculated before SIR?

calc_refrates_sublung <- function(histo, long_df, refpop_df, sublung_var, 
                             region_var, age_var, sex_var, year_var, race_var, fill_sites = "no",
                             exhistgroup_var = NULL, 
                             exclude_named_histo = TRUE, quiet = FALSE){
  
  if(is.character(sublung_var)){
    sublung_var <- rlang::ensym(sublung_var)
  }
  
  if(is.null(exhistgroup_var)){
    exhistgroup_var <- sublung_var
    exhisto <- histo
  } else{
    #if exclude_histgroup_var is different from sublung_var
    if(is.character(exhistgroup_var)){
      exhistgroup_var <- rlang::ensym(exhistgroup_var)
    }
    if(!(rlang::as_name(exhistgroup_var) %in% c("t_histgroupiarc"))){
      rlang::abort(c(
        "i" = "Parameter `exhistgroup_var` is currently only supported for \"t_histgroupiarc\".", 
        paste0("Default `exhistgroup_var = NULL` or a valid value must be used instead of: ", rlang::as_name(exhistgroup_var))
      ))
    }
    
    #now determine what histo to exclude
    if(rlang::as_name(exhistgroup_var) == "t_histgroupiarc"){
      exhisto <- tidytable::case_when(
        as.numeric(histo) >= 8051 & as.numeric(histo) <= 8084 ~ "Squamous carcinomas",
        as.numeric(histo) >= 8120 & as.numeric(histo) <= 8131 ~ "Squamous carcinomas",
        as.numeric(histo) >= 8090 & as.numeric(histo) <= 8110 ~ "Basal cell carcinomas",
        as.numeric(histo) >= 8140 & as.numeric(histo) <= 8149 ~ "Adenocarcinomas",
        as.numeric(histo) >= 8160 & as.numeric(histo) <= 8162 ~ "Adenocarcinomas",
        as.numeric(histo) >= 8190 & as.numeric(histo) <= 8221 ~ "Adenocarcinomas",
        as.numeric(histo) >= 8260 & as.numeric(histo) <= 8337 ~ "Adenocarcinomas",
        as.numeric(histo) >= 8350 & as.numeric(histo) <= 8551 ~ "Adenocarcinomas",
        as.numeric(histo) >= 8570 & as.numeric(histo) <= 8576 ~ "Adenocarcinomas",
        as.numeric(histo) >= 8940 & as.numeric(histo) <= 8941 ~ "Adenocarcinomas",
        as.numeric(histo) >= 8030 & as.numeric(histo) <= 8046 ~ "Other specific carcinomas",
        as.numeric(histo) >= 8150 & as.numeric(histo) <= 8157 ~ "Other specific carcinomas",
        as.numeric(histo) >= 8170 & as.numeric(histo) <= 8180 ~ "Other specific carcinomas",
        as.numeric(histo) >= 8230 & as.numeric(histo) <= 8255 ~ "Other specific carcinomas",
        as.numeric(histo) >= 8340 & as.numeric(histo) <= 8347 ~ "Other specific carcinomas",
        as.numeric(histo) >= 8560 & as.numeric(histo) <= 8562 ~ "Other specific carcinomas",
        as.numeric(histo) >= 8580 & as.numeric(histo) <= 8671 ~ "Other specific carcinomas",
        as.numeric(histo) >= 8010 & as.numeric(histo) <= 8015 ~ "Unspecified carcinomas (NOS)",
        as.numeric(histo) >= 8020 & as.numeric(histo) <= 8022 ~ "Unspecified carcinomas (NOS)",
        as.numeric(histo) >= 8050 & as.numeric(histo) <= 8050 ~ "Unspecified carcinomas (NOS)",
        as.numeric(histo) >= 8680 & as.numeric(histo) <= 8713 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 8800 & as.numeric(histo) <= 8921 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 8990 & as.numeric(histo) <= 8991 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 9040 & as.numeric(histo) <= 9044 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 9120 & as.numeric(histo) <= 9125 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 9130 & as.numeric(histo) <= 9136 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 9141 & as.numeric(histo) <= 9252 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 9370 & as.numeric(histo) <= 9373 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 9540 & as.numeric(histo) <= 9582 ~ "Sarcomas and soft tissue tumours",
        as.numeric(histo) >= 9050 & as.numeric(histo) <= 9055 ~ "Mesothelioma",
        as.numeric(histo) >= 9840 & as.numeric(histo) <= 9840 ~ "Myeloid",
        as.numeric(histo) >= 9861 & as.numeric(histo) <= 9931 ~ "Myeloid",
        as.numeric(histo) >= 9945 & as.numeric(histo) <= 9946 ~ "Myeloid",
        as.numeric(histo) >= 9950 & as.numeric(histo) <= 9950 ~ "Myeloid",
        as.numeric(histo) >= 9961 & as.numeric(histo) <= 9964 ~ "Myeloid",
        as.numeric(histo) >= 9965 & as.numeric(histo) <= 9967 ~ "Myeloid", #new coding
        as.numeric(histo) >= 9980 & as.numeric(histo) <= 9987 ~ "Myeloid",
        as.numeric(histo) >= 9992 & as.numeric(histo) <= 9992 ~ "Myeloid", #new coding
        as.numeric(histo) >= 9670 & as.numeric(histo) <= 9699 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9728 & as.numeric(histo) <= 9728 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9731 & as.numeric(histo) <= 9734 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9735 & as.numeric(histo) <= 9738 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9761 & as.numeric(histo) <= 9767 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9769 & as.numeric(histo) <= 9769 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9823 & as.numeric(histo) <= 9826 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9833 & as.numeric(histo) <= 9833 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9836 & as.numeric(histo) <= 9836 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9940 & as.numeric(histo) <= 9940 ~ "B-cell neoplasms",
        as.numeric(histo) >= 9700 & as.numeric(histo) <= 9719 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9726 & as.numeric(histo) <= 9726 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9729 & as.numeric(histo) <= 9729 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9768 & as.numeric(histo) <= 9768 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9827 & as.numeric(histo) <= 9831 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9834 & as.numeric(histo) <= 9834 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9837 & as.numeric(histo) <= 9837 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9948 & as.numeric(histo) <= 9948 ~ "T-cell and NK-cell neoplasms",
        as.numeric(histo) >= 9650 & as.numeric(histo) <= 9667 ~ "Hodgkin lymphoma",
        as.numeric(histo) >= 9740 & as.numeric(histo) <= 9742 ~ "Mast-cell Tumours",
        as.numeric(histo) >= 9750 & as.numeric(histo) <= 9758 ~ "Histiocytes and Accessory Lymphoid cells",
        as.numeric(histo) >= 9590 & as.numeric(histo) <= 9591 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9596 & as.numeric(histo) <= 9596 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9597 & as.numeric(histo) <= 9597 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9727 & as.numeric(histo) <= 9727 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9760 & as.numeric(histo) <= 9760 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9800 & as.numeric(histo) <= 9801 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9805 & as.numeric(histo) <= 9805 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9806 & as.numeric(histo) <= 9809 ~ "Unspecified haematopoietic cancers", #new coding
        as.numeric(histo) >= 9811 & as.numeric(histo) <= 9816 ~ "Unspecified haematopoietic cancers", #new coding
        as.numeric(histo) >= 9820 & as.numeric(histo) <= 9820 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9832 & as.numeric(histo) <= 9832 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9835 & as.numeric(histo) <= 9835 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9860 & as.numeric(histo) <= 9860 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9960 & as.numeric(histo) <= 9960 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9970 & as.numeric(histo) <= 9970 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9971 & as.numeric(histo) <= 9971 ~ "Unspecified haematopoietic cancers", #new coding
        as.numeric(histo) >= 9975 & as.numeric(histo) <= 9975 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9989 & as.numeric(histo) <= 9989 ~ "Unspecified haematopoietic cancers",
        as.numeric(histo) >= 9140 & as.numeric(histo) <= 9140 ~ "Kaposi sarcoma",
        as.numeric(histo) >= 8720 & as.numeric(histo) <= 8790 ~ "Other specified types of cancer",
        as.numeric(histo) >= 8930 & as.numeric(histo) <= 8936 ~ "Other specified types of cancer",
        as.numeric(histo) >= 8950 & as.numeric(histo) <= 8983 ~ "Other specified types of cancer",
        as.numeric(histo) >= 9000 & as.numeric(histo) <= 9030 ~ "Other specified types of cancer",
        as.numeric(histo) >= 9060 & as.numeric(histo) <= 9110 ~ "Other specified types of cancer",
        as.numeric(histo) >= 9260 & as.numeric(histo) <= 9365 ~ "Other specified types of cancer",
        as.numeric(histo) >= 9380 & as.numeric(histo) <= 9539 ~ "Other specified types of cancer",
        as.numeric(histo) >= 8000 & as.numeric(histo) <= 8005 ~ "Unspecified types of cancer",
        .default = NA)
    }
  }

  
  if(exclude_named_histo){
    #Version EX - exclude the named histology and calculate rates for all other histologies
    long_df %>%
      #create variable to indicate to be counted as case
      tidytable::mutate(is_case = case_when(
        t_sitewhogen == "Lung and Bronchus" & !!exhistgroup_var != exhisto ~ 1,
        .default = 0)) %>%
      #calculate refrates
      msSPChelpR::calc_refrates(dattype = NULL, count_var = "is_case",                               
                                refpop_df = refpop_df, calc_totals = TRUE, fill_sites = fill_sites,
                                region_var = region_var, age_var = age_var, 
                                sex_var = sex_var, year_var = year_var, 
                                race_var = race_var, site_var = "t_sitewhogen",
                                quiet = quiet) %>%
      tidytable::mutate(histology = paste("excluding", histo, "[[", exhisto, "]]"))
    
  } else{
    #Version IN - calculate rates for each single histology
    long_df %>%
      #create variable to indicate to be counted as case
      tidytable::mutate(is_case = case_when(
        t_sitewhogen == "Lung and Bronchus" & !!sublung_var == histo ~ 1,
        TRUE ~ 0)) %>%
      #calculate refrates
      msSPChelpR::calc_refrates(dattype = NULL, count_var = "is_case",                               
                                refpop_df = refpop_df, calc_totals = TRUE, fill_sites = fill_sites,
                                region_var = region_var, age_var = age_var, 
                                sex_var = sex_var, year_var = year_var, 
                                race_var = race_var, site_var = "t_sitewhogen",
                                quiet = quiet) %>%
      tidytable::mutate(histology = histo)
  }
}


## -------- General functions -------------

clean_session <- function(keep_objects = c("infile_counties", "infile_dir", "infile_counties",
                                           "outfile_icd2d_dco", "outfile_icd3d_dco", "outfile_siterwho_dco",
                                           "read_seer_refrates", "formats", "clean_session",
                                           "restart_session", "test_result", "test_results", "check_tests",
                                           "check_files_updated")) {
  #show previous warnings
  summary(warnings())
  #remove workspace
  list_objects <- ls(all.names = TRUE, envir = .GlobalEnv)
  list_objects <- list_objects[!list_objects %in% keep_objects] 
  rm(list=list_objects, envir = .GlobalEnv)
  #clear mem
  for(i in 1:10){
    gc()
    Sys.sleep(2)
  }
}

check_tests <- function(log_vector, rm = FALSE, quiet_when_passed = FALSE){
  #give test results
  if(all(log_vector)==TRUE){
    if(rm && !quiet_when_passed){
      rlang::inform(paste0("All tests passed. Object ", substitute(log_vector)," will be deleted."))
      rm(list = as.character(substitute(log_vector)), envir = .GlobalEnv )
    }
    if(rm && quiet_when_passed){
      rm(list = as.character(substitute(log_vector)), envir = .GlobalEnv )
    }
    if(!rm && !quiet_when_passed){
      rlang::inform("All tests passed.")
    }
  } else{
    rlang::abort("Some tests have failed!")
  }
}

check_files_updated <- function(file_list, tolerance_minutes = 10){
  #get modified time
  time_modified <- sapply(file_list, file.info)
  time_modified <- time_modified["mtime", ]
  
  #calculate expected time by adding tolerance in minutes
  time_expected <- Sys.time() - (tolerance_minutes * 60)
  
  #make check and return logical vector #TRUE if file has been updated within tolerance
  time_expected < time_modified
}


