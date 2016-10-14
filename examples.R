autopolsample[doc == 2 & renew == 1,autrenew := 1]
grep("PP",colnames(final.data))
'PPlus' %in% colnames(final.data)
