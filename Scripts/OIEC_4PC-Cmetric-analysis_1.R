


library(convevol)

OIEC_Cmetricmatrix <-
  as.matrix(array(OIEC_df[, 1:4])) # The numbers here are what PCs you select for the analysis

cleanwhaletree <- read.tree("Data/OIEC_cleanwhaletree_1.tree")

convtips1 <- c(
  "Cephalorhynchus_commersonii",
  "Cephalorhynchus_heavisidii",
  "Delphinapterus_leucas",
  "Delphinus_delphis",
  "Feresa_attenuata",
  "Grampus_griseus",
  "Inia_geoffrensis",
  "Lagenodelphis_hosei",
  "Lagenorhynchus_acutus",
  "Lagenorhynchus_albirostris",
  "Lagenorhynchus_australis",
  "Lagenorhynchus_cruciger",
  "Lagenorhynchus_obscurus",
  "Lipotes_vexillifer",
  "Monodon_monoceros",
  "Neophocaena_phocaenoides",
  "Orcaella_brevirostris",
  "Orcinus_orca",
  "Peponocephala_electra",
  "Phocoena_dioptrica",
  "Phocoena_phocoena",
  "Phocoenoides_dalli",
  "Platanista_gangetica",
  "Pontoporia_blainvillei",
  "Pseudorca_crassidens",
  "Sotalia_fluviatilis",
  "Sotalia_guianensis",
  "Sousa_chinensis",
  "Stenella_attenuata",
  "Stenella_coeruleoalba",
  "Stenella_longirostris",
  "Steno_bredanensis",
  "Tursiops_aduncus",
  "Tursiops_truncatus"
)

ConvReg1 <- convrat(phyl = cleanwhaletree,
                    phendata = OIEC_Cmetricmatrix,
                    convtips = convtips1)
ConvReg1

ConvReg1_pval <- convratsig(phyl = cleanwhaletree,
                            phendata = OIEC_Cmetricmatrix,
                            convtips = c)
ConvReg1_pval