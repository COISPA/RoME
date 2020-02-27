###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################


# Load functions for the checks
# 
 source("check_identical_records.r")
 source("check_quasiidentical_records.r")
 source("check_consistencyTA_duration.r")
 source("check_consistencyTA_distance.r")
 source("check_hauls_TATB.r")
 source("check_hauls_TBTA.r")
 source("check_rubincode.r")
 source("check_species_TBTC.r")
 source("check_haul_species_TCTB.r")
 source("check_raising.r")
 source("check_length.r")
 source("check_weight.r")
 source("check_mat_stages.r")
 source("check_nbtotTB.r")
 source("check_length_class_codeTC.r")
 source("check_nb_per_sexTC.r")
 source("check_distance.r")
 source("check_position.r")
 source("check_dictionary.r")
 source("check_bridles_length.r")
 source("graphs_TA.r")
 source("check_position_in_Med.r")
 source("check_area_year.r")
 source("check_depth.r")
 source("check_stratum.r")
 source("check_quadrant.r")
 source("check_step_length_distr.r")
 source("check_unique_valid_haul.r")
 source("check_weight_tot_nb.r")
 source("check_spawning_period.r")
 source("check_sex_inversion.r")
 source("check_smallest_mature.r")
 source("check_no_empty_fields.r")
 source("check_dm.r")
 source("check_0_fieldsTA.r")
 source("check_temperature.r")
 source("check_stratum_code.r")
 source("check_date_haul.r")
 source("check_TE_TC.r")
 source("check_individual_weightTE.r")
 source("check_individual_weightTC.r")
 source("check_nm_TB.r")
 source("check_nb_TE.r")
 source("facilities.r")
 source("check_G1_G2.r")
 source("scheme_individual_data.r")
 source("check_subsampling.r")
 source("check_hauls_TATL.r")
 source("check_associations_category_TL.r")
 source("check_hauls_TLTA.r") 
 source("check_no_empty_fields.r")

# 
# #R-sufi file creation
# 
 source("create_strata.r")
 source("create_length.r")
 source("create_haul.r")
 source("create_catch.r")
 source("RSufi_files.r")
 
 
 # additional functions
 source("checkHeader.r")
 source("convertToCsv.r")
 source("copyTables.r")