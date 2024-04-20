library('dplyr')
library('tidyr')

df <- read.csv('Intel_CPUs.csv')

df <- df %>%
  select(Graphics_Base_Frequency,
         Graphics_Max_Dynamic_Frequency,
         Graphics_Video_Max_Memory,
         Graphics_Output,
         Max_Resolution_HDMI,
         Max_Resolution_DP,
         Max_Resolution_eDP_Integrated_Flat_Panel,
         DirectX_Support,
         PCI_Express_Revision,
         PCI_Express_Configurations_,
         Max_nb_of_PCI_Express_Lanes,
         T,
         Intel_Hyper_Threading_Technology_,
         Intel_Virtualization_Technology_VTx_,
         Intel_64_,
         Instruction_Set,
         Instruction_Set_Extensions,
         Idle_States,
         Thermal_Monitoring_Technologies,
         Secure_Key,
         Execute_Disable_Bit)

apply(is.na(df),2, sum)
apply(is.na(df), 2, mean)
apply(df, 2, unique)


apply(df, 2, function(x) mean(is.na(x) | x == "" | x == "N/A"))

md.pattern(df)

####### apply
df$Graphics_Base_Frequency <- sapply(df$Graphics_Base_Frequency, clean_graphics_base_frequency)

df$Graphics_Max_Dynamic_Frequency <- sapply(df$Graphics_Max_Dynamic_Frequency, clean_graphics_base_frequency)

df$Graphics_Video_Max_Memory <- sapply(df$Graphics_Video_Max_Memory, clean_graphics_video)

df$Max_nb_of_PCI_Express_Lanes <- as.numeric(df$Max_nb_of_PCI_Express_Lanes)


df <- boolean_convert(df, 'Intel_Hyper_Threading_Technology_')
df <- boolean_convert(df, 'Intel_Virtualization_Technology_VTx_')
df <- boolean_convert(df, 'Intel_64_')

df$Instruction_Set <- factor(ifelse(df$Instruction_Set == "", NA, df$Instruction_Set), 
                               levels=c('Itanium 64-bit', '64-bit', '32-bit'))

df <- boolean_convert(df, 'Idle_States')
df <- boolean_convert(df, 'Thermal_Monitoring_Technologies')
df <- boolean_convert(df, 'Secure_Key')
df <- boolean_convert(df, 'Execute_Disable_Bit')


unique(df$Graphics_Output)
df$Graphics_Output <- factor(sapply(df$Graphics_Output, clean_graphic_output))


df['SSE_FPU'] <- str_detect(df$Instruction_Set_Extensions, 'SSE|AVX|Yes')
df['AES'] <- str_detect(df$Instruction_Set_Extensions, 'AES')
df['MMX'] <- str_detect(df$Instruction_Set_Extensions, 'MMX')

df <- df %>% select(-Instruction_Set_Extensions)

df['HDMI_resolution'] <- factor(sapply(df$Max_Resolution_HDMI, resolution_extract))
df['HDMI_frequency'] <- factor(sapply(df$Max_Resolution_HDMI, resolution_freq_extract))

df['DP_resolution'] <- factor(sapply(df$Max_Resolution_DP, resolution_extract))
df['DP_frequency'] <- factor(sapply(df$Max_Resolution_DP, resolution_freq_extract))

df['eDP_resolution'] <- factor(sapply(df$Max_Resolution_eDP_Integrated_Flat_Panel, resolution_extract))
df['eDP_frequency'] <- factor(sapply(df$Max_Resolution_eDP_Integrated_Flat_Panel, resolution_freq_extract))


unique(df$PCI_Express_Revision)
df$PCI_Express_Revision <- factor(sapply(df$PCI_Express_Revision, latest_pci), levels=c(3, 2, 1, 0))

df$T <- sapply(df$T, temperature_extract)
unique(df$PCI_Express_Configurations_)

df$PCI_Express_Configurations_ <- factor(sapply(df$PCI_Express_Configurations_, configuration_extract))
unique(df$PCI_Express_Configurations_)

unique(df$DirectX_Support)
df$DirectX_Support <- factor(sapply(df$DirectX_Support, latest_directX_version))
