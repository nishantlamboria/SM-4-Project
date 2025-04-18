#Importing and cleaning all the data

districts = c("ambala", "bhiwani", "faridabad", "fatehabad", "gurgaon",
              "hissar", "jhajjar", "jind", "kaithal", "karnal", "kurukshetra",
              "mewat", "palwal", "panipat",
              "rewari", "rohtak", "sirsa", "sonepat", "yamunanagar")

state_data = read.csv("IMD_data_haryana.csv")
state_mat = as.matrix(state_data[,3:14])
state_vec <- as.vector(t(state_mat))

for(d in districts){
  cat(paste0(d, "_data = read.csv(\"IMD_data_", d,  ".csv\")"))
}

ambala_data = read.csv("IMD_data_ambala.csv")
bhiwani_data = read.csv("IMD_data_bhiwani.csv")
faridabad_data = read.csv("IMD_data_faridabad.csv")
fatehabad_data = read.csv("IMD_data_fatehabad.csv")
gurgaon_data = read.csv("IMD_data_gurgaon.csv")
hissar_data = read.csv("IMD_data_hissar.csv")
jhajjar_data = read.csv("IMD_data_jhajjar.csv")
jind_data = read.csv("IMD_data_jind.csv")
kaithal_data = read.csv("IMD_data_kaithal.csv")
karnal_data = read.csv("IMD_data_karnal.csv")
kurukshetra_data = read.csv("IMD_data_kurukshetra.csv")
mewat_data = read.csv("IMD_data_mewat.csv")
palwal_data = read.csv("IMD_data_palwal.csv")
panipat_data = read.csv("IMD_data_panipat.csv")
rewari_data = read.csv("IMD_data_rewari.csv")
rohtak_data = read.csv("IMD_data_rohtak.csv")
sirsa_data = read.csv("IMD_data_sirsa.csv")
sonepat_data = read.csv("IMD_data_sonepat.csv")
yamunanagar_data = read.csv("IMD_data_yamunanagar.csv")

# Handling missing values

for (d in districts) {
  df <- get(paste0(d, "_data"))
  
  for (j in 3:14) {
    month_mean <- mean(df[[j]], na.rm = TRUE)
    df[[j]][is.na(df[[j]])] <- month_mean
  }
  
  for (i in 1:nrow(df)){
    df[i,15] <- sum(df[i,3:14])
    df[i,16] <- sum(df[i,3:4])
    df[i,17] <- sum(df[i,5:7])
    df[i,18] <- sum(df[i,8:11])
    df[i,19] <- sum(df[i,12:14])
  }
  
  # Assign the cleaned data back to the object
  assign(paste0(d, "_data"), df)
  
  mat <- as.matrix(df[, 3:14])    # extract columns 3 to 14 and convert to matrix
  assign(paste0(d, "_mat"), mat)
  
  vec <- as.vector(t(mat))    # transpose to make it row-wise, then flatten
  assign(paste0(d, "_vec"), vec)
}

