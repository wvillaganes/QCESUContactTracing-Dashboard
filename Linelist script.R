ll <- read_excel(path = "C:/Users/QCESU - WILSON/Documents/CESU MDB FILES/DOH LINELIST UPDATE.xlsx", sheet = "DATA")


ll$report_date <- as.Date(ll$report_date)
total_reported_cases <- nrow(ll)
ll$`DNPRC Remarks` <- toupper(ll$`DNPRC Remarks`)
ll$`NPC Remarks` <- toupper(ll$`NPC Remarks`)
ll$DNPRC <- toupper(ll$DNPRC)




mdb_found_count <- sum(str_detect(ll$CESU, "YES"), na.rm = TRUE)
v_count <- sum(str_detect(ll$CESU, "YES") & !is.na(ll$`Field Traced Date`), na.rm = TRUE)
to_input_mdb <- mdb_found_count - v_count
investigated_1stbd_ft <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & str_detect(ll$DNPRC, "INVESTIGATED") &
                            str_detect(ll$`DNPRC Remarks`,"FIELD"), na.rm = TRUE)
investigated_1stbd_ETOC <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & str_detect(ll$DNPRC, "INVESTIGATED") &
                                 str_detect(ll$`DNPRC Remarks`,"OTHER CITY"), na.rm = TRUE) 
investigated_1stbd_ETOD <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & str_detect(ll$DNPRC, "INVESTIGATED") &
                                 str_detect(ll$`DNPRC Remarks`,"OTHER DISTRICT"), na.rm = TRUE)
investigated_1stbd_NR <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & str_detect(ll$DNPRC, "INVESTIGATED") &
                                 str_detect(ll$`DNPRC Remarks`,"NON"), na.rm = TRUE)
investigated_1stbd_UTL <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & str_detect(ll$DNPRC, "INVESTIGATED") &
                                    str_detect(ll$`DNPRC Remarks`,"LOCATE"), na.rm = TRUE)
investigated_1stbd_DC <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & str_detect(ll$DNPRC, "INVESTIGATED") &
                                str_detect(ll$`DNPRC Remarks`,"DUPLICATE"), na.rm = TRUE)
investigated_1stbd_No_remark <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & !str_detect(ll$DNPRC, "NOT INVESTIGATED") &
                               (is.na(ll$`DNPRC Remarks`) | str_detect(ll$`DNPRC Remarks`, "NO REMARKS")), na.rm = TRUE)
notinvestigated_1st <- sum(str_detect(ll$CESU, "YES") & is.na(ll$`Field Traced Date`) & 
                              (str_detect(ll$DNPRC, "NOT INVESTIGATED") | is.na(ll$DNPRC)), na.rm = TRUE)
balance <- total_reported_cases - mdb_found_count

found_on_npc <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`), na.rm = TRUE)
OZ_count_npc <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "OUT OF CITY"),
                    na.rm = TRUE)
UTC_npc <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "UNABLE"),na.rm = TRUE)
RFHH_npc <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST"),na.rm = TRUE)
suspect_pending_npc <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "SUSPECT"),na.rm = TRUE)
AIMDB_npc <-sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "ALREADY"),na.rm = TRUE)


ETF_investigated_npc <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
  !str_detect(ll$DNPRC, "NOT INVESTIGATED"),  na.rm = TRUE)

investigated_2ndbd_ft <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                               !str_detect(ll$DNPRC, "NOT INVESTIGATED") & str_detect(ll$`DNPRC Remarks`,"FIELD"), na.rm = TRUE)
investigated_2ndbd_ETOC <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                                 !str_detect(ll$DNPRC, "NOT INVESTIGATED") & str_detect(ll$`DNPRC Remarks`,"OTHER CITY"), na.rm = TRUE) 
investigated_2ndbd_ETOD <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                                 !str_detect(ll$DNPRC, "NOT INVESTIGATED") & str_detect(ll$`DNPRC Remarks`,"OTHER DISTRICT"), na.rm = TRUE)
investigated_2nd_NR <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                               !str_detect(ll$DNPRC, "NOT INVESTIGATED") & str_detect(ll$`DNPRC Remarks`,"NON"), na.rm = TRUE)
investigated_2ndbd_UTL <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                                !str_detect(ll$DNPRC, "NOT INVESTIGATED") & str_detect(ll$`DNPRC Remarks`,"LOCATE"), na.rm = TRUE)
investigated_2ndbd_DC <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                               !str_detect(ll$DNPRC, "NOT INVESTIGATED") & str_detect(ll$`DNPRC Remarks`,"DUPLICATE"), na.rm = TRUE)
investigated_2ndbd_No_remark <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                                      !str_detect(ll$DNPRC, "NOT INVESTIGATED") & (is.na(ll$`DNPRC Remarks`) | str_detect(ll$`DNPRC Remarks`, "No remarks")), na.rm = TRUE)
investigated_2ndbd_unco <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") &
                                 !str_detect(ll$DNPRC, "NOT INVESTIGATED") & str_detect(ll$`DNPRC Remarks`,"UNCOOPERATIVE"), na.rm = TRUE)

notinvestigated_2nd <- sum(is.na(ll$CESU) & !is.na(ll$`NPC Report Date`) & str_detect(ll$`NPC Remarks`, "REQUEST") & 
                              (str_detect(ll$DNPRC, "NOT INVESTIGATED") | is.na(ll$DNPRC)), na.rm = TRUE)

discrepancy <- balance - found_on_npc 



