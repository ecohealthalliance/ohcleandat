othertext_lookup <- function(questionnaire = c("animal_owner")){
  
  if(questionnaire == "animal_owner"){
    
    lookup <- tibble::tribble(
      ~name,                            ~other_name,
      "f2_species_own",                  "f2a_species_own_oexp",
      "f6e_rvf_vax_type",                "f6e_rvf_vax_type_oexp",
      "f6a_protocol",                    "f6a_protocol_other",
      "f6b_which_vax",                   "f6c_ani_vax_num_oexp",
      "f6i_rvf_vax_chalenge_mult",       "f6i_rvf_vax_chalenge_oexp",
      "f7b_abortion_3_which",            "f7b_abortion_3_which_oexp",
      "f7d_abortion_12_which",           "f7d_abortion_12_which_oexp",
      "f7f_abortus_dispose",             "f7f_abortus_dispose_oexp",
      "f8_ani_contact",                  "f8_ani_contact_oexp",
      "f8c_contact_other_sp",            "f8c_contact_other_sp_oexp",
      "f9f_ani_contact_wildlife",        "f9_ani_contact_oexp",
      "f9a_kraal_c",                     "f9a_kraal_oexp_c",
      "f9b_kraal_sh",                    "f9b_kraal_oexp_sh",
      "f9c_kraal_g",                     "f9c_kraal_oexp_g",
      "f9d_kraal_o",                     "f9d_kraal_oexp_o",
      "f14_new_ani",                     "f14_new_ani_oexp",
      "f14b_inj_type",                   "f14b_inj_type_oexp",
      "f16_animal_purpose",              "f16_animal_purpose_oexp",
      "f17a_slaughter_loc",              "f17a_slaughter_loc_oexp",
      "f18a_fencing_type",               "f18_fencing_type_oexp",
      "f19_sick_ani_self_treat",         "f19_sick_ani_self_treat_oexp"
    )
  }
  
  return(lookup)
  
}


