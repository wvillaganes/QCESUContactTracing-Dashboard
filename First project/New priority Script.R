## Import excel file
NPC <- read_excel(path = "C:/Users/QCESU - WILSON/Documents/CESU MDB FILES/New priority cases 4.0.xlsx", sheet = "NPC")

## Data preparation
NPC$Result <- toupper(NPC$Result)
NPC$Barangay <- toupper(NPC$Barangay)
NPC$City <- toupper(NPC$Barangay)
NPC$Sex[NPC$Sex %in% c("FEMALE","F","F ", " F", "f", "f ", " f")] <- "Female"
NPC$Sex[NPC$Sex %in% c("MALE","M", "M ", " M", "m", "m ", " m")] <- "Male"


NPC_dashboard_daily <- NPC %>%
  filter(`Report date` == Sys.Date())

Reported_count = nrow(NPC_dashboard_daily)

Endorsed_count_d = sum(str_detect(NPC_dashboard_daily$Remarks, "REQUEST"))

OZ_count_d = sum(str_detect(NPC_dashboard_daily$Remarks, "OUT OF CITY"))

Visited_count_d = sum(str_detect(NPC_dashboard_daily$Remarks, "VISITED"))

Pending_count_d = sum(str_detect(NPC_dashboard_daily$Remarks, "match not found"),
                      na.rm = TRUE)
C_pending_count_d = sum(str_detect(NPC_dashboard_daily$Remarks,"match not found") &
                          str_detect(NPC_dashboard_daily$`Patient classification`, "CONFIRMED"),
                        na.rm = TRUE)
S_pending_count_d = sum(str_detect(NPC_dashboard_daily$Remarks,"match not found") &
                          str_detect(NPC_dashboard_daily$`Patient classification`, "SUSPECT"),
                        na.rm = TRUE)
P_pending_count_d = sum(str_detect(NPC_dashboard_daily$Remarks,"match not found") &
                          str_detect(NPC_dashboard_daily$`Patient classification`, "PROBABLE"),
                        na.rm = TRUE)
Wrong_number_d = sum(str_detect(NPC_dashboard_daily$Remarks, "WRONG NUMBER"))
Cannot_be_reach_d = sum(str_detect(NPC_dashboard_daily$Remarks, "CANNOT BE"))
Droppedcall_count_d = sum(str_detect(NPC_dashboard_daily$Remarks, "DROPPED CALL"))
Wrong_person_d = sum(str_detect(NPC_dashboard_daily$Remarks, "WRONG PERSON"))
NATPhone_d = sum(str_detect(NPC_dashboard_daily$Remarks, "NOT ANSWERING"))
No_number_d = sum(str_detect(NPC_dashboard_daily$Remarks, "NO NUMBER") &
                    str_detect(NPC_dashboard_daily$`Patient classification`, "CONFIRMED"))
Unable_to_contact_d = sum(str_detect(NPC_dashboard_daily$Remarks,"UNABLE"))

Reported_count_cumulative = nrow(NPC)

Endorsed_count_cum = sum(str_detect(NPC$Remarks, "REQUEST"))
OZ_count_cum = sum(str_detect(NPC$Remarks, "OUT OF CITY"))
Visited_count_cum = sum(str_detect(NPC$Remarks, "VISITED"))
Pending_count_cum = sum(str_detect(NPC$Remarks, "match not found"), na.rm = TRUE)
C_pending_count_cum = sum(str_detect(NPC$Remarks,"match not found") &
                            str_detect(NPC$`Patient classification`, "CONFIRMED"), na.rm = TRUE)
S_pending_count_cum = sum(str_detect(NPC$Remarks,"match not found") &
                            str_detect(NPC$`Patient classification`, "SUSPECT"), na.rm = TRUE)
P_pending_count_cum = sum(str_detect(NPC$Remarks,"match not found") &
                            str_detect(NPC$`Patient classification`, "PROBABLE"), na.rm = TRUE)
Wrong_number_cum = sum(str_detect(NPC$Remarks, "WRONG NUMBER"))
Cannot_be_reach_cum = sum(str_detect(NPC$Remarks, "CANNOT BE"))
Droppedcall_count_cum = sum(str_detect(NPC$Remarks, "DROPPED CALL"))
Wrong_person_cum = sum(str_detect(NPC$Remarks, "WRONG PERSON"))
NATPhone_cum = sum(str_detect(NPC$Remarks, "NOT ANSWERING"))
No_number_cum = sum(str_detect(NPC$Remarks, "NO NUMBER"))
Unable_to_contact_cum = sum(str_detect(NPC$Remarks,"UNABLE"))
